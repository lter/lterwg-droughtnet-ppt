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
path_oct <- 'E:/Dropbox/IDE Meeting_Oct2019'
path_1yr <- 'E:/Dropbox/IDE MS_Single year extreme'

# load tpa precip data -----------------------------------------------------

# tpa data now stored in one folder

# folder path (fist folder)
precip_folder1 <- file.path(path_oct, "data/precip/Yearly Precip_TPA/drought_net_trends_tpa")

file_names1 <- list.files(precip_folder1)


precip1 <- lapply(file_names1, function(file_name){
  print(file_name)
  read.csv(file.path(precip_folder1, file_name), as.is = TRUE) %>% 
    dplyr::select(site_code, year, totalPRE)
})


precip3 <- precip1 

# extracting site codes from each file
site_codes <- sapply(precip3, function(df){
  # check: all should have one unique site code
  stopifnot(length(unique(df$site_code)) == 1)
  
  sc <- df$site_code[[1]]
  
  sc
})

# nameing list elements (important for indexing later)
names(precip3) <- site_codes


#   -----------------------------------------------------------------------

# compute probability density function for each site, based on last 50 years

# density functions
# this is like a smoothed histogram
density <- lapply(precip3, function(df){
  ppt <- df %>% 
    filter(year > 1964) %>% 
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


# calculating percentiles given annual precip --------------------------------

for (i in 1:nrow(site_ppt2)){
  print(i)
  site_code <- site_ppt2$site_code[i]
  print(site_code)
  ppt <- site_ppt2$ppt[i] # precip for the site/year of interest

  # percentile based on cdf for the site
  f <-cdf1[[site_code]]
  
  if(is.null(f)){
    warning(paste("no tpa data was available for:", site_code))
    site_ppt2$perc_obs[i] <- NA
    site_ppt2$perc_norm[i] <- NA
  } else {
    
    tpa_ppt <- precip3[[site_code]]%>% 
      filter(year > 1964) %>% 
      .$totalPRE
    
    site_ppt2$perc_obs[i] <- f(ppt)*100 # convert quantile to percentile
    # normal approximations
    site_ppt2$perc_norm[i] <- pnorm(ppt,
                                    mean = mean(tpa_ppt,na.rm=T),
                                    sd = sd(tpa_ppt,na.rm=T))*100

  }
}

# any tpa data missing for location with precip data?
# --if yes then need to download the tpa data
site_ppt2 %>% 
  filter(!is.na(ppt) & is.na(perc_obs)) %>% 
  .$site_code %>% 
  unique()

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


site_ppt2 %>% 
  filter(X365day.trt == "No", n_treat_days > 0) %>% 
  select(site_code, matches("365")) %>% 
  .[!duplicated(.), ]

df <- site_ppt2 %>% 
  filter(site_code == "biddulph.ca", plot == 1)


# calculating n_treat days adjusted for roof off time
# some parsing failures occur--because not legit set/remove
# dates for some pre trmt dates (this is ok)
site_ppt3 <- site_ppt2 %>% 
  rename(percentile = perc_norm) %>% 
  dplyr::select(-perc_obs) %>% 
  mutate(plot_dummy = plot) %>% 
  group_by(site_code, plot_dummy) %>% 
  nest() %>% 
  # see functions.R fo
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
# for inconsistencies---NOT RESOLVED
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

# send as CSV to Kate
out <- site_ppt4 %>% 
  select(-matches("sub$"), -matches("ghcn"), -plot, -block) %>% 
  filter(!is.na(ppt)) %>% 
  # this first group_by/mutate is so that control/trt have same
  # drought_days etc values
  group_by(site_code, year, trt) %>% 
  # NOTE: using mean instead of max may be way to go...fewer sites have discrepancy. 
  summarize(mean_bio = mean(biomass_date)#,
         #mean_first = mean(first_treatment_date)
         ) %>% 
  ungroup() %>% 
  spread(key = trt, value = mean_bio) %>% 
  filter(Control != Drought) 

# write_csv(out, "biomass_date_mismatch_forKW.csv")

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
wide_yr1 <- site_ppt4 %>% 
  select(-matches("sub$"), -matches("ghcn"), -plot, -block) %>% 
  filter(!is.na(ppt)) %>% 
  # this first group_by/mutate is so that control/trt have same
  # drought_days etc values
  group_by(site_code, year) %>% 
  mutate_at(vars(n_treat_days, n_treat_days_adj, num_drought_days, 
                 biomass_date, first_treatment_date),
               .funs = list(~mean(., na.rm = TRUE))) %>% 
  group_by(site_code, year, trt) %>% 
  summarise_at(vars(matches("percentile"), matches("ppt"), n_treat_days, n_treat_days_adj,
                    num_drought_days, biomass_date, first_treatment_date),
                             .funs = list(~mean(., na.rm = TRUE))) %>% 
  gather(key = "key", value = "value", ppt, percentile) %>% 
  mutate(key = paste(key, trt, sep = "_")) %>% 
  select(-trt) %>% 
  spread(key = "key", value = "value") %>% 
  as_tibble() %>% 
  mutate(trt_yr_adj = cut(n_treat_days_adj, c(-10000, 1, 364, 729, 10000), 
                          labels = c("pre-trt", "< 365 days trt", "365-729 days trt", "730 + trt days"),
                          right = FALSE)
         )
  
# sites without data
site_ppt4 %>% 
  filter(n_treat_days >=365 & n_treat_days < 730, is.na(ppt)) %>% 
  pull(site_code) %>% 
  unique() %>% 
  length()

# have added in the 1/31/20 data set:
# "cerrillos.ar"  "chacra.ar"     "chilcasdrt.ar" "morient.ar"    "riomayo.ar"  
# "sclaudio.ar"   "spvdrt.ar"  
site_ppt4 %>% 
  filter(n_treat_days_adj >=150 & n_treat_days_adj < 730, !is.na(ppt)) %>% 
  pull(site_code) %>% 
  unique() %>% 
  length()

site_ppt4 %>% 
  filter(!is.na(ppt)) %>% 
  pull(site_code) %>% 
  unique() %>% 
  length()

site_ppt3$site_code %>% unique() %>% length()




# figures -----------------------------------------------------------------
theme_set(theme_classic())

base1 <- list(
  geom_abline (slope = 1, intercept = 0),
  labs(subtitle = "Control vs Drought treatment precip",
       caption = "number of treatment days taken from 'n_treat_days_adj' column"),
  theme(plot.title = element_text(size = 13))
)

image_path <- file.path(
  path_oct,
  paste0("figures/precip/ambient_vs_drought_precip_", today(), ".pdf"))

pdf(image_path,  height = 7, width = 10)

# ctrl vs drt percentiles
ggplot(wide_yr1) +
  base1+
  geom_point(aes(percentile_Control, percentile_Drought)) +
  labs(x = "Control precipitation percentile",
       y = "Drought precipitation percentile",
       title = "Percentiles of annual precipitation for each site/year") +
  facet_wrap(~trt_yr_adj)

trt_years <- levels(wide_yr1$trt_yr_adj)[-1]

# labeled figures by treatment year for percentiles
map(trt_years, function(yr) {
  wide_yr1 %>% 
    filter(trt_yr_adj == yr) %>% 
  ggplot() +
    base1+
    geom_point(aes(percentile_Control, percentile_Drought)) +
    labs(x = "Control precipitation percentile",
         y = "Drought precipitation percentile",
         title = paste("Percentiles of annual precipitation for ", yr)) +
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


# saving the data (csv) ---------------------------------------------------

# write_csv(site_ppt4,
#           file.path(path_oct, "Full biomass", "anpp_clean_trt_ppt_02-26-2020.csv"))

wide2save <- wide_yr1 %>% 
  rename(ppt_ambient = ppt_Control,
         ppt_drought = ppt_Drought,
         percentile_ambient = percentile_Control,
         percentile_drought = percentile_Drought)

write_csv(wide2save,
          file.path(path_1yr, "Data/precip",
                    "precip_by_trmt_year_with_percentiles_2020-02-26.csv"))


# checks ------------------------------------------------------------------

wide2save %>% 
  filter(str_detect(site_code, ".br$")) #brazil sites--one still missing

# sanity checks--
missing_drt_ppt <- wide2save %>% 
  group_by(site_code) %>% 
  summarize(any_data = any(is.na(ppt_drought))) %>% 
  filter(any_data) %>% 
  pull(site_code)

missing_drt_ppt

# not sure why brandjberg is calculating drt ppt even when no drt trmt
# percent given in file---two first_treatment_dates issue?
site_ppt4 %>% filter(site_code  %in% missing_drt_ppt, trt %in% c("Drought"), is.na(drought_trt), !is.na(ppt))
is.na(wide2save$ppt_ambient) %>% sum()

wide2save %>% 
  filter(is.na(ppt_ambient))

wide2save$site_code %>% unique() %>% sort()
