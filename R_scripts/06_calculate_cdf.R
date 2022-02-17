#####################################################################
### script for estimating cumulative distribution functions by site ##
### and calculating percentiles of annual precipitation of treatments#
### during experiment years. 
#######################################################################

# specifically the aim is if a site (drought or control) received x mm of 
# precip during a 12 month period, what percentile is that relative to the 
# long term (50 years) tpa data set for the site. 
# percentiles calculated using emperical cdf and normal approximated cdf

# script started 5/23/19


# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(spatstat)
source("R_scripts/functions.R")

# path to data folders
path_oct <- '~/Dropbox/IDE Meeting_Oct2019'
path_ms <- '~/Dropbox/IDE MS_Single year extreme'

# load worldclim monthly precip data --------------------------------------

# can get tpa tool data from previous commits. 

precip1 <- read.csv(file.path(path_ms, "Data/precip", "worldclim_monthly_precip.csv"),
         as.is = TRUE)

# getting monthly
precip2 <- precip1 %>% 
  group_by(site_code, year) %>% 
  summarize(n = n(),
            totalPRE = sum(wc_ppt)) %>% 
  filter(n == 12) %>% # only full years (1 month from 2019 provided)
  select(-n) %>% 
  ungroup()

precip3 <- split(precip2, precip2$site_code)

# calculate and save inter-annual CV

cv <- precip2 %>% 
  group_by(site_code) %>% 
  summarise(map = mean(totalPRE),
            cv = sd(totalPRE)/map*100)

write_csv(cv, file.path(path_ms, "Data/precip", "worldclim_interannual_cv.csv"))

# load worldclim map data -----------------------------------------------------

site_info <- read.csv(file.path(path_ms, "Data\\Site_Elev-Disturb.csv"))


latlon <- site_info[,c('site_code','longitud','latitud')]
###remove duplicate sites for now

latlon <- latlon[!duplicated(latlon$site_code),]

# some norway sites are NA b/ coords on water so adjust lat downward
# doesn't seem to be needed anymore
#to_adjust <- c("lygraint.no", "lygraold.no", "lygrayng.no")

#latlon[latlon$site_code %in% to_adjust, ]$latitud <- latlon[latlon$site_code %in% to_adjust, ]$latitud - 0.02

# Get worldclim MAP

map_files <- list.files(file.path(path_oct, "data\\precip\\wc2.0_30s_prec"),
                        pattern = "wc2.0_30s_prec", full.names = TRUE)

mat_files <- list.files(file.path(path_ms, "Data\\wc2.1_30s_tavg"),
                        pattern = "wc2.1_30s_tavg", full.names = TRUE)
mat_files
for (i in seq_along(map_files)) {
  month <- str_extract(map_files[[i]], "(?<=prec_)\\d{2}(?=.tif$)")
  tmp = raster::raster(map_files[[i]])
  latlon[[paste0("precip", month)]] <- raster::extract(tmp,latlon[, c("longitud", "latitud")])
}

for (i in seq_along(mat_files)) {
  month <- str_extract(mat_files[[i]], "(?<=tavg_)\\d{2}(?=.tif$)")
  tmp = raster::raster(mat_files[[i]])
  latlon[[paste0("tavg", month)]] <- raster::extract(tmp,latlon[, c("longitud", "latitud")])
}

# checking sites
# plot(p12, xlim = c(5, 6), ylim = c(60.4, 60.7))
# points(latlon[, 2:3])

latlon$wc_map <- rowSums(dplyr::select(latlon, matches("precip\\d{2}")))
latlon$wc_mat <- rowMeans(dplyr::select(latlon, matches("tavg\\d{2}")))

stopifnot(all(!is.na(latlon$wc_map)),
          all(!is.na(latlon$wc_mat))) # run check

site_info <- merge(site_info,latlon[,c("site_code","wc_map")])

# write_csv(latlon[,c("site_code","wc_map", "wc_mat")], 
#           file.path(path_ms, "Data/precip", "worldclim_map.csv"))

#   -----------------------------------------------------------------------

# compute probability density function for each site, based on last 50 years

# density functions
# this is like a smoothed histogram
density <- lapply(precip3, function(df){
  ppt <- df %>% 
#    filter(year > 1964) %>% 
    .$totalPRE
  d <- density(ppt)
  d
})

# run the following code if you want to visualize the density functions
# for each site:
# par(mfrow = c(2, 2))
# walk2(density, site_codes, function(x, y){
#   plot(x, main = y)
# })

# compute cumulative density function -------------------------------------

# this ~ takes the area under the curve of the density function
# a smoothed version of the emperical cumulative density function (which one would get with ecdf())
cdf1 <- lapply(density, function(x){
  cdf <- CDF(x)
  cdf
})


# observed precipitation by site/year/trmt --------------------------------

# grabbing newest file

p1 <- newest_file_path(
  file.path(path_oct, 'data/precip'),
  "anpp_clean_trt_ppt_no-perc_\\d{4}-\\d+-\\d+.csv")
p1

# temp--change path back to p1
site_ppt <- read.csv(p1, as.is = TRUE, na.strings = c("NA", "NULL"))

site_ppt2 <- site_ppt %>% 
  mutate(biomass_date = ymd(biomass_date),
         first_treatment_date = ymd(first_treatment_date))


# sites for pub fig -------------------------------------------------------

# site list for sites to be included in histogram of site % MAP
sites95 <- read_csv(file.path(path_ms,
                              "Data/96SiteInfo-DS3.csv")) %>% 
  rename(site_code = `Site code`)


# calculating percentiles given annual precip --------------------------------

for (i in 1:nrow(site_ppt2)){
  print(i)
  site_code <- site_ppt2$site_code[i]
  print(site_code)
  ppt <- site_ppt2$ppt[i] # precip for the site/year of interest

  # percentile based on cdf for the site
  f <-cdf1[[site_code]]
  
  if(is.null(f)){
    warning(paste("no data was available for:", site_code))
    site_ppt2$perc_obs[i] <- NA
    site_ppt2$perc_norm[i] <- NA
  } else {
    
    tpa_ppt <- precip3[[site_code]]%>% 
      #filter(year > 1964) %>% 
      .$totalPRE
    
    site_ppt2$perc_obs[i] <- f(ppt)*100 # convert quantile to percentile
    # normal approximations
    site_ppt2$perc_norm[i] <- pnorm(ppt,
                                    mean = mean(tpa_ppt,na.rm=T),
                                    sd = sd(tpa_ppt,na.rm=T))*100

  }
}

# worldlclim data missing for location with precip data?
#  rerun 01_worldclim_extract-monthly.R, but the worldclim rasters
# are needed to do this (and they're not on dropbox b/ too big)
out <- site_ppt2 %>% 
  filter(!is.na(ppt) & is.na(perc_obs)) %>% 
  .$site_code %>% 
  unique()

if(length(out) > 0) {
  warning("worldclim data not extracted for the following sites:\n", out)

}

# figures -----------------------------------------------------------------

# percentiles normal approx vs imperical

# pdf(file.path(path_oct,
#               paste0("figures/precip/percentiles_emperical_vs_normal", today(), ".pdf")
#               ))

plot(site_ppt2$perc_norm, site_ppt2$perc_obs,
     xlab = "Ambient precip percentile from normal CDF",
     ylab = "Ambient precip percentile from emperical CDF")
abline(0, 1)
dev.off()

# since normal and emperical percentiles are nearly the same, just keeping the normal


# calculating n_treat days adjusted for roof off time
# some parsing failures occur--because not legit set/remove
# dates for some pre trmt dates (this is ok)
# 
site_ppt3 <- site_ppt2 %>% 
  rename(percentile = perc_norm) %>% 
  dplyr::select(-perc_obs) %>% 
  mutate(plot_dummy = plot,
         n_treat_days =   as.numeric(biomass_date - first_treatment_date)) %>% 
  # temp fix b/ pozos doesn't have biomass date
  filter(trt %in% c("Control", "Drought")) %>% 
  group_by(site_code, plot_dummy) %>% 
  nest() %>% 
  # see functions.R fo
  # not set/remove dates provided for cedarsav and kranz.de, which throws warnings
  mutate(n_treat_days_adj = map(data, calc_n_treat_days_adj, return_df = FALSE)) %>% 
  unnest(cols = c("data", "n_treat_days_adj")) %>% 
  ungroup() %>% 
  select(-plot_dummy)

# n_treat_days_adj parsed correctly if no rows:
site_ppt3 %>% 
  filter(X365day.trt == "No", is.na(n_treat_days_adj), n_treat_days > 0)

site_ppt4 <- site_ppt3 %>% 
  mutate(n_treat_days_adj = ifelse(is.na(n_treat_days_adj),
                                   n_treat_days,
                                   n_treat_days_adj)
         )

# testing ~~~~~~~~~
# for inconsistencies---mostly resolved
site_ppt4 %>% 
  select(-matches("sub$"), -matches("ghcn"), -plot, -block) %>% 
  filter(!is.na(ppt)) %>% 
  # this first group_by/mutate is so that control/trt have same
  # drought_days etc values
  group_by(site_code, year) %>% 
  mutate(n_unique_bio = length(unique(biomass_date)),
            n_unique_first = length(unique(first_treatment_date))) %>% 
  filter(n_unique_first > 1) 

site_ppt4 %>% 
  select(-matches("sub$"), -matches("ghcn"), -plot, -block) %>% 
  filter(!is.na(ppt)) %>% 
  # this first group_by/mutate is so that control/trt have same
  # drought_days etc values
  group_by(site_code, year) %>% 
  mutate(n_unique_bio = length(unique(biomass_date)),
         n_unique_first = length(unique(first_treatment_date))) %>% 
  filter(n_unique_first > 1)



# no non-unique block 
site_ppt4 %>% 
  group_by(site_code, plot, block, year) %>% 
  filter(biomass_date == max(biomass_date)) %>% 
  summarize(n = n()) %>% 
  filter(n > 1)


# end testing ~~~~~~~~~

# NOTE: figure out why stubai.at has two rows for 'year 1'
# STOP I think this average (based on possible control/drought biomass date
# descrepencies may be leading to the issue of couple sites haveing 'more' 
# drt than ctrl precip--- SOLVE THIS PROBLEM
# also an issue with n_treat_days_adj (is average), --this is also issue
# for brandjberg where there are two first_treat_dates (off by years)

# not problem ~ solved--just keeping the max date for trmt/control
wide_yr0 <- site_ppt4 %>% 
  select(-matches("sub$"), -matches("ghcn"), -plot, -block) %>% 
  filter(!is.na(ppt)) %>% 
  # this first group_by/mutate is so that control/trt have same
  # drought_days etc values
  group_by(site_code, year, trt) %>% 
  filter(biomass_date == max(biomass_date)) %>% 
  summarise_at(vars(matches("percentile"), matches("ppt"), n_treat_days, n_treat_days_adj,
                    num_drought_days, biomass_date, first_treatment_date, annual_ppt_used),
                             .funs = list(~mean(., na.rm = TRUE))) %>% 
  group_by(year, site_code) %>% 
  # in cases when different biomass harvest dates (and thus n_treat days) for
  # control and drought trmts then use the trmt date/days
  mutate(biomass_date = ifelse(length(biomass_date[trt == "Drought"]) > 0 && 
                                 !is.na(biomass_date[trt == "Drought"]),
                               biomass_date[trt == "Drought"],
                               biomass_date[trt == "Control"]),
         n_treat_days_adj = ifelse(length(n_treat_days_adj[trt == "Drought"]) > 0 && 
                                     !is.na(n_treat_days_adj[trt == "Drought"]) ,
                                   n_treat_days_adj[trt == "Drought"],
                                   n_treat_days_adj[trt == "Control"]),
         biomass_date = as.Date(biomass_date, origin = "1970-01-01")) %>% 
  ungroup() %>% 
  gather(key = "key", value = "value", ppt, percentile) %>% 
  mutate(key = paste(key, trt, sep = "_")) %>% 
  select(-trt) %>% 
  spread(key = "key", value = "value") %>% 
  as_tibble() %>% 
  mutate(trt_yr_adj = cut(n_treat_days, c(-10000, 1, 365, 700, 10000), 
                          labels = c("pre-trt", "< 365 days trt", 
                                     "365 - 700 days trt", 
                                     "700 + trt days"),
                          right = FALSE)
         )

# indicates problem (ie averaged accross different data sources)
stopifnot(wide_yr0$annual_ppt_used %in% c(0, 1))

wide_yr0$annual_ppt_used <- as.logical(wide_yr0$annual_ppt_used) 

# seperately grouping first year with 365 trmt days
yr1_lab = "113 + days trt (first yr w/ 113 days trmt)"
wide_yr1 <- wide_yr0 %>% 
  filter(n_treat_days >= 113 & n_treat_days <= 657) %>% 
  group_by(site_code) %>% 
  filter(n_treat_days_adj == min(n_treat_days_adj)) %>% 
  mutate(trt_yr_adj = yr1_lab) %>% 
  bind_rows(wide_yr0) %>% 
  left_join(site_info[, c("site_code", "wc_map")], by = "site_code")

wide_yr1 %>% 
  filter(is.na(ppt_Control)) %>% 
  pull(site_code) %>% 
  unique()

wide_yr1 %>% 
  filter(!is.na(ppt_Drought)) %>% 
  pull(site_code) %>% 
  unique()
# sites without data

yr1_sites <- wide_yr1 %>% 
  filter(!is.na(ppt_Control) & !is.na(ppt_Drought),
         trt_yr_adj == yr1_lab, !annual_ppt_used) %>% 
  pull(site_code)

wide_yr1 %>% 
  dplyr::filter(trt_yr_adj == yr1_lab, !is.na(ppt_Control), annual_ppt_used) %>% 
  pull(site_code) %>% 
  unique()
  

n <- yr1_sites %>% 
  unique() %>% 
  length()
n

wide_yr1b <- wide_yr1 %>% 
  mutate(
    # percent annual precip is of MAP
    perc_ap_map = ppt_Control/wc_map*100,
    # log of ap/map ratio
    log_ap_map = log(ppt_Control/wc_map))

# have added in the 1/31/20 data set:
# "cerrillos.ar"  "chacra.ar"     "chilcasdrt.ar" "morient.ar"    "riomayo.ar"  
# "sclaudio.ar"   "spvdrt.ar"  

site_ppt4 %>% 
  filter(!is.na(ppt)) %>% 
  pull(site_code) %>% 
  unique() %>% 
  length()

#only year one
wide_year_one <- wide_yr1b %>% 
  filter(trt_yr_adj == yr1_lab) %>% 
  mutate(is_trmt_365 = ifelse(n_treat_days_adj < n_treat_days, "No", "Yes"))

# figures -----------------------------------------------------------------
if (FALSE){ # set to FALSE to not run the figure making code
theme_set(theme_classic())

base1 <- list(
  geom_abline (slope = 1, intercept = 0),
  labs(subtitle = "Control vs Drought treatment precip",
       caption = "number of treatment days taken from 'n_treat_days' column"),
  theme(plot.title = element_text(size = 13))
)

image_path <- file.path(
  path_ms,
  paste0("Figures/precip/ambient_vs_drought_precip_", today(), ".pdf"))

perc_source <- "WorldClim used to calculate percentiles"
#pdf(image_path,  height = 7, width = 10)

# histograms
hist(wide_year_one$n_treat_days,
     xlab = "days", 
     main = paste("sites with biomass date occuring 365-700 days since treatment establishment (n = ",
                  n, ")"))
perc_ap_map_lab <- "AP percent of MAP (AP/MAP*100)"
hist(wide_year_one$perc_ap_map,
     xlab = perc_ap_map_lab, 
     main = "Annual Precip percent of MAP")

log_ap_map_lab <- "Log(AP/MAP)"
hist(wide_year_one$log_ap_map,
     xlab = "Log(AP/MAP)", 
     main = "Log of Annual Precip/MAP Ratio")

hist(wide_year_one$percentile_Control,
     xlab = "Percentile", 
     main = paste("Annual precip percentile (control plots).", perc_source)) 

hist(wide_year_one$percentile_Drought,
     xlab = "Percentile", 
     main = paste("Annual precip percentile (Drought plots).", perc_source)) 

g0 <- ggplot(wide_year_one, aes(shape = is_trmt_365, color = is_trmt_365)) +
  labs(caption = "sites with year 1 (365-700 days) data")

g0 +
  geom_point(aes(wc_map, ppt_Control)) +
  labs(x = "MAP (WorldClim)",
       y = "Control Precip") +
  geom_abline(slope = 1, intercept = 0)

g0 +
  geom_point(aes(wc_map, ppt_Drought)) +
  labs(x = "MAP (WorldClim)",
       y = "Drought Precip") +
  geom_abline(slope = 1, intercept = 0)

g0+
  labs(xlab = "AP percent of MAP (AP/MAP*100)")+
  geom_point(aes(x = perc_ap_map, y = percentile_Control)) +
  labs(x = perc_ap_map_lab,
       y = "Control percentile",
       caption = perc_source) 

g0+
  labs(xlab = "drought percent of MAP (AP/MAP*100)")+
  geom_point(aes(x = ppt_Drought/wc_map*100, y = percentile_Drought)) +
  labs(x = paste("drought", perc_ap_map_lab),
       y = "Drought percentile",
       caption = perc_source) 
  
g0+
  labs(xlab = "AP percent of MAP (AP/MAP*100)")+
  geom_point(aes(x = perc_ap_map, y = percentile_Control)) +
  labs(x = perc_ap_map_lab,
       y = "Control percentile")

g0+
  labs(xlab = "AP percent of MAP (AP/MAP*100)")+
  geom_point(aes(x = log_ap_map, y = percentile_Control)) +
  labs(x = log_ap_map_lab,
       y = "Control percentile")

# ctrl vs drt percentiles
ggplot(wide_yr1) +
  base1+
  geom_point(aes(percentile_Control, percentile_Drought)) +
  labs(x = "Control precipitation percentile",
       y = "Drought precipitation percentile",
       title = "Percentiles of annual precipitation for each site/year") +
  facet_wrap(~trt_yr_adj)

trt_years <- unique(wide_yr1$trt_yr_adj) %>% 
  sort()

# labeled figures by treatment year for percentiles
map(trt_years[trt_years!= "365 - 700 days trt"], function(yr) {
  g3 <- wide_yr1b %>% 
    filter(trt_yr_adj == yr) %>% 
    mutate(is_trmt_365 = ifelse(n_treat_days_adj < n_treat_days, "No", "Yes")) %>% 
    ggplot() +
    geom_hline(yintercept = c(1, 5, 10), linetype = 2, 
               color = c("red", "orange", "yellow"))
  
  # ap/map percent
  g4 <- g3 + 
    geom_point(aes(perc_ap_map, percentile_Drought,
                 shape = is_trmt_365, color = is_trmt_365)) +
    labs(x = perc_ap_map_lab,
       y = "Drought precipitation percentile",
       title = paste("drought percentile vs percent MAP for ", yr)) 
  
  g4b <- g4+
    ggrepel::geom_text_repel(
      aes(x = perc_ap_map, y =  percentile_Drought, label = site_code),
      size = 3, min.segment.length = unit(0.1, "lines"), angle = 0) 
  
  g5 <- g3 + 
    geom_point(aes(log_ap_map, percentile_Drought,
                   shape = is_trmt_365, color = is_trmt_365)) +
    labs(x = log_ap_map_lab,
         y = "Drought precipitation percentile",
         title = paste("drought percentile vs log(AP/MAP) for ", yr)) 
  g5
  g5b <- g5 +
    ggrepel::geom_text_repel(
      aes(x = log_ap_map, y =  percentile_Drought, label = site_code),
      size = 3, min.segment.length = unit(0.1, "lines"), angle = 0)
  list(g4, g4b, g5, g5b)
})

# labeled figures by treatment year for percentiles
map(trt_years, function(yr) {
  wide_yr1 %>% 
    filter(trt_yr_adj == yr) %>% 
    mutate(is_trmt_365 = ifelse(n_treat_days_adj < n_treat_days, "No", "Yes")) %>% 
  ggplot() +
    base1+
    geom_hline(yintercept = c(1, 5, 10), linetype = 2, alpha = 0.3, 
               color = c("red", "orange", "yellow")) +
    geom_vline(xintercept = c(20, 80), linetype = 2) +
    geom_point(aes(percentile_Control, percentile_Drought,
                   shape = is_trmt_365, color = is_trmt_365)) +
    labs(x = "Control precipitation percentile",
         y = "Drought precipitation percentile",
         title = paste("Percentiles of annual precipitation for ", yr),
         subtitle = "WordClim used to calculate percentiles") +
    ggrepel::geom_text_repel(
      aes(x = percentile_Control, y =  percentile_Drought, label = site_code),
      size = 3, min.segment.length = unit(0.1, "lines"), angle = 0)
})


# ctrl vs drt annual precip

ggplot(wide_yr1) +
  base1+
  geom_point(aes(ppt_Control, ppt_Drought)) +
  labs(x = "Control 12 month precipitation (mm)",
       y = "Drought 12 month precipitation (mm)",
       title = "Annual precipitation for each site/year") +
  facet_wrap(~trt_yr_adj)

# labeled figures by treatment year of annual precip
map(trt_years, function(yr) {
  wide_yr1 %>% 
    filter(trt_yr_adj == yr) %>% 
    ggplot() +
    base1+
    geom_point(aes(ppt_Control, ppt_Drought)) + 
    labs(x = "Control 12 month precipitation (mm)",
         y = "Drought 12 month precipitation (mm)",
         title = paste("Annual precipitation for ", yr)) +
    ggrepel::geom_text_repel(
      aes(x = ppt_Control, y =  ppt_Drought, label = site_code),
      size = 3, min.segment.length = unit(0.1, "lines"), angle = 0)
})
dev.off()


}

# saving the data (csv) ---------------------------------------------------

# write_csv(site_ppt4,
#           file.path(path_ms, "Data/precip", "anpp_clean_trt_ppt_06-26-2020.csv"))

wide2save <- wide_yr1 %>% 
  rename(ppt_ambient = ppt_Control,
         ppt_drought = ppt_Drought,
         percentile_ambient = percentile_Control,
         percentile_drought = percentile_Drought) %>% 
  # so don't have duplicated rows
  filter(trt_yr_adj != yr1_lab) %>% 
  mutate(perc_reduction = 100-ppt_drought/ppt_ambient*100)

# this includes the  worldclim percentiles
write_csv(wide2save,
          file.path(path_ms, "Data/precip",
                    "precip_by_trmt_year_with_percentiles_2021-05-12.csv"))

tibble(site_code = yr1_sites) %>% 
  write_csv(file.path(path_ms, "Data/precip",
                      "sites_with_year1_ppt_data.csv"))


# testing ######################

mis_sites <- c("credoj.au",
               "credom.au",
               "dang.cn",
               "guaribas.br",
               "matta.il",
              "passogavia.it",
               "yanchi.cn",
              " youyu.cn",
              "lcnorth.cl", 
             "lcsouth.cl",
              "qdtnorth.cl",  
              "qdtsouth.cl")

mis_sites[! mis_sites %in% wide2save$site_code]

wide2save %>% 
  filter(site_code %in% mis_sites) 

wide2save %>% 
  filter(str_detect(site_code, ".cn$")) %>% 
  pull(site_code)
# end testing ################

# checks ------------------------------------------------------------------

wide2save %>% 
  filter(str_detect(site_code, ".br$")) #brazil sites--one still missing

# sanity checks--
missing_drt_ppt <- wide2save %>% 
  filter(is.na(ppt_drought) & !is.na(ppt_ambient)) %>% 
  pull(site_code) %>% 
  unique()

# STOP track down what is going on here
missing_drt_ppt
wide2save %>% filter(site_code %in% missing_drt_ppt)
# not sure why brandjberg is calculating drt ppt even when no drt trmt
# percent given in file---two first_treatment_dates issue?

# the issue is that these sites don't have drought treatments?
site_ppt4 %>% group_by(site_code, year) %>% 
  summarise(n_c = sum(trt == "Control"), 
            n_t = sum(str_detect(trt, "rought"))) %>% 
  filter(n_t ==0) %>% 
  pull(site_code) %>% 
  unique()


wide2save %>% 
  filter(is.na(ppt_ambient))

wide2save$site_code %>% unique() %>% sort()


# unique sites with varying cuttoffs ---------------------------------------

n_sites <- expand_grid(min_n_treat_days = c(113, 300, 365, 0),
                  max_n_treat_days = 657,
                  min_reduction = c(0, 15, 25),
                  n_sites = NA)

sites_list <- n_sites %>% 
  mutate(site_code = NA_character_) %>% 
  select(-n_sites) %>% 
  .[0, ]
for(i in 1:nrow(n_sites)) {
   sites <- wide2save %>% filter(perc_reduction > n_sites$min_reduction[i], 
                       n_treat_days >= n_sites$min_n_treat_days[i],
                       n_treat_days <= n_sites$max_n_treat_days[i],
                       !is.na(ppt_ambient), !is.na(ppt_drought), !annual_ppt_used) %>% 
     pull(site_code) %>% 
     unique() %>% 
     sort()
  n_sites$n_sites[i] <- length(sites)
  df <- n_sites[rep(i, length(sites)), 
                c('min_n_treat_days', 'max_n_treat_days', 'min_reduction')]
  df$site_code <- sites
  sites_list <- bind_rows(sites_list, df)
}
n_sites  %>% 
  arrange(min_n_treat_days, min_reduction)

sites_list <- sites_list  %>% 
  arrange(min_n_treat_days, min_reduction, site_code)

write_csv(sites_list, file.path(path_ms, "Data/precip",
                     "first_yr_sites_by_cutoff.csv"))

# histograms for publication--------------------------------------------------

trmt_labs <- c("ppt_Control" = "Control Treatment",
               "ppt_Drought" = "Drought Treatment")
# control and drought as percent of MAP

jpeg(file.path(path_ms, "Figures/precip/percent_MAP_hists.jpeg"), height = 6, 
     width = 4,
     units = "in",
     res = 600)

df_for_hist <- wide_year_one %>%   # filtering based on cuttoff
  filter(trt_yr_adj == yr1_lab, !annual_ppt_used, 
         site_code %in% sites95$site_code) %>% 
  ungroup() %>% 
  select(site_code, year, ppt_Drought, ppt_Control, wc_map) %>% 
  pivot_longer(cols = c("ppt_Drought", "ppt_Control"),
               names_to = "trmt",
               values_to = "ppt") %>% 
  mutate(perc_ap_map = ppt/wc_map*100)

breaks <- seq(from = 0, to = 200, by = 10)

ggplot(df_for_hist, aes(perc_ap_map)) +
  geom_histogram(color = "black", fill = "dark grey", breaks = breaks) +
  lemon::facet_rep_wrap(~trmt, ncol = 1, labeller = as_labeller(trmt_labs)) +
  labs(x = "Percent MAP (Annual Precipitation/MAP*100)",
       y = "# of Sites") +
  theme_classic() +
  theme(strip.background = element_blank())
dev.off()

# mean percent ap map
df_for_hist %>% 
  group_by(trmt) %>% 
  summarize(perc_ap_map = mean(perc_ap_map, na.rm = TRUE))

df_for_hist$site_code %>% unique() %>% sort()

