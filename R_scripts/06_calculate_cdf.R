#####################################################################
### script for estimating cumulative distribution functions by site ##
### and calculating percentiles of annual precipitation of treatments#
### during experiment years. 
#######################################################################

# specifically the aim is if a site (drought or control) received x mm of 
# precip during a 12 month period, what percentile is that relative to the 
# long term (50 years) tpa data set for the site. 
# percentiles calculated using emperical cdf and normal approximated cdf

# Note--output currently does not have ppt_source column (all NAs)
# and there are some duplicated rows

# additionally--the output should be for all years, not just for year 1

# script started 5/23/19


# packages ----------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(spatstat)
source("R_scripts/functions.R")
source("R_scripts/dropbox_path.R") # where path to dropbox should be set
# path to data folders
path_oct <- file.path(path, 'IDE Meeting_Oct2019')
path_ms <- file.path(path, 'IDE MS_Single year extreme')

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

site_ppt <- read.csv(p1, as.is = TRUE, na.strings = c("NA", "NULL"))

site_ppt2 <- site_ppt %>% 
  mutate(biomass_date = ymd(biomass_date),
         first_treatment_date = ymd(first_treatment_date))


# sites for pub fig -------------------------------------------------------

# site list for sites to be included in histogram of site % MAP
sites95 <- read_csv(file.path(path_ms,
                              "Data/msyr1_sites.csv")) 


# calculating percentiles given annual precip --------------------------------

for (i in 1:nrow(site_ppt2)){
  #print(i)
  site_code <- site_ppt2$site_code[i]
  #print(site_code) # print when troubleshooting
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
test <- site_ppt4 %>% 
  select(-matches("(sub)|(ghcn)|(chirps)$"),-plot, -block) %>% 
  filter(!is.na(ppt)) %>% 
  # this first group_by/mutate is so that control/trt have same
  # drought_days etc values
  group_by(site_code, year) %>% 
  mutate(n_unique_bio = length(unique(biomass_date)),
            n_unique_first = length(unique(first_treatment_date))) %>% 
  filter(n_unique_first > 1) 

if(length(test$site_code) > 0) {
  stop('some duplicate rows in site_ppt4')
}

# no non-unique block 
test <- site_ppt4%>% 
  group_by(site_code, trt,plot, block, year) %>% 
  filter(biomass_date == max(biomass_date)) %>% 
  summarize(n = n()) %>% 
  filter(n > 1) %>% 
  pull(site_code) %>% 
  unique()

if(length(test) > 0) warning('duplication issues')


# end testing ~~~~~~~~~

# NOTE: figure out why stubai.at has two rows for 'year 1'
# STOP I think this average (based on possible control/drought biomass date
# descrepencies may be leading to the issue of couple sites haveing 'more' 
# drt than ctrl precip--- SOLVE THIS PROBLEM
# also an issue with n_treat_days_adj (is average), --this is also issue
# for brandjberg where there are two first_treat_dates (off by years)

# function to replace the control value with the drought value
# (e.g. for when biomass dates are different)
use_drt_value <- function(x, trt) {
  out <- ifelse(length(x[trt == "Drought"]) > 0 && 
                  !is.na(x[trt == "Drought"]),
                x[trt == "Drought"],
                x[trt == "Control"])
  out
}

# not problem ~ solved--just keeping the max date for trmt/control
wide_yr0 <- site_ppt4 %>% 
  select(-matches("(sub)|(ghcn)|(chirps)$"),-plot, -block) %>%  
  filter(!is.na(ppt)) %>% 
  # this first group_by/mutate is so that control/trt have same
  # drought_days etc values
  group_by(site_code, year, trt) %>% 
  filter(biomass_date == max(biomass_date)) %>% 
  summarise(across(
      .cols = c(matches("percentile"), ppt, n_treat_days, n_treat_days_adj,
                   num_drought_days, biomass_date, first_treatment_date, annual_ppt_used),
      .fns = mean, na.rm = TRUE
    ),
    # in theory there could be more than 1 ppt_source, so just grabbing first
    ppt_source = unique(ppt_source)[1],
    fake_bioDat = unique(fake_bioDat)[1],
    .groups = 'drop') %>% 
  group_by(year, site_code) %>% 
  # in cases when different biomass harvest dates (and thus n_treat days) for
  # control and drought trmts then use the trmt date/days
  mutate(biomass_date = use_drt_value(biomass_date, trt),
         n_treat_days_adj = use_drt_value(n_treat_days_adj, trt),
         n_treat_days = use_drt_value(n_treat_days, trt),
         num_drought_days = use_drt_value(num_drought_days, trt),
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

test <- duplicated(wide_yr0[ c("year", "site_code")])

if(sum(test) > 0) {
  warning('duplicated year/site_code combinations present')
}


# separately grouping first year with 365 trmt days
yr1_lab = "113 + days trt (first yr w/ 113 days trmt)"
wide_yr1 <- wide_yr0 %>% 
  filter(n_treat_days >= 113 & n_treat_days <= 657) %>% 
  group_by(site_code) %>% 
  filter(n_treat_days_adj == min(n_treat_days_adj)) %>% 
  mutate(trt_yr_adj = yr1_lab) %>% 
  bind_rows(wide_yr0) %>% 
  # the precip column is site submitted precip
  left_join(site_info[, c("site_code", 'wc_map', "precip")], by = "site_code") %>% 
  rename(site_map = precip) 

if (sum(is.na(wide_yr1$site_map)) > 0) {
  warning('site MAP not available for all sites (substitute in world clim?)')
}

# some site/years only collected data on control plots
test <- wide_yr1 %>% 
  filter(is.na(ppt_Drought) & !is.na(ppt_Control)) %>% 
  select(year, site_code) %>% 
  left_join(site_ppt2) %>% 
  pull(trt)

if (!all(test == 'Control')) {
  warning('drought ppt missing for times when drought biomass was created')
}

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
    perc_ap_map = ppt_Control/site_map*100,
    # log of ap/map ratio
    log_ap_map = log(ppt_Control/site_map))

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
                    "precip_by_trmt_year_with_percentiles_2022-04-16.csv"))

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


# * checking for missing years --------------------------------------------

# years that are missing but have years of data before and after them.
# this should only be because there were too many days with NA ppt that year
# or there is no biomass data for that year
missing_years <- wide2save %>% 
  arrange(site_code, year) %>% 
  group_by(site_code) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  mutate(diffs_yr = diff_na(year)) %>% 
  filter(diffs_yr > 1) %>% 
  # this doesn't currently account for the fact
  # that could have multiple consecutive yrs missing
  mutate(missing_year = year -1) %>% 
  select(site_code, missing_year)

reason_missing <- missing_years %>% 
  left_join(site_ppt, by = c("site_code", 'missing_year' = 'year')) %>% 
  ungroup() %>% 
  mutate(sub_na = rowSums(.[ c("ppt_num_NA_sub", "ppt_num_wc_interp_sub")],
                          na.rm = TRUE)) %>% 
  group_by(site_code, missing_year) %>% 
  summarise(across(c(ppt_num_NA_ghcn, sub_na), mean, na.rm = TRUE),
            biomass_data_missing = all(is.na(biomass_date))) %>% 
  mutate(too_many_missing = (ppt_num_NA_ghcn > 30 | is.na(ppt_num_NA_ghcn)) & sub_na > 30) %>% 
  filter(!biomass_data_missing & !too_many_missing)


if(any(!reason_missing$biomass_data_missing & 
       !reason_missing$too_many_missing)) {
  stop('some years have missing data for an unknown reason')
} else {
  message('check passed')
}

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
         site_code %in% sites95$site_code, !fake_bioDat) %>% 
  ungroup() %>% 
  select(site_code, year, ppt_Drought, ppt_Control, site_map) %>% 
  pivot_longer(cols = c("ppt_Drought", "ppt_Control"),
               names_to = "trmt",
               values_to = "ppt") %>% 
  mutate(perc_ap_map = ppt/site_map*100)

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
perc_ap_map <- df_for_hist %>% 
  group_by(trmt) %>% 
  summarize(perc_ap_map = mean(perc_ap_map, na.rm = TRUE))

df_for_hist$site_code %>% unique() %>% sort()


# print info about sites in the histogram
sink(file.path(path_ms, "Figures/precip/percent_MAP_hists_info.txt"))
n <- df_for_hist %>% 
  filter(!is.na(ppt)) %>% 
  pull(site_code) %>% 
  unique() %>% 
  length()


cat("Information about percent_MAP_hists.jpeg\n\n")
cat("Number of sites shown in histogram: ", n, "\n\n")

cat("On average, drought plots received ", 
    round(perc_ap_map$perc_ap_map[perc_ap_map$trmt == "ppt_Drought"], 1), 
    "% of mean annual precipitation.\n")
cat("On average, control plots received ", 
    round(perc_ap_map$perc_ap_map[perc_ap_map$trmt == "ppt_Control"], 1), 
    "% of mean annual precipitation.\n")
sink()

