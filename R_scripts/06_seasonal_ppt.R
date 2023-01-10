# Martin Holdrege

# script started 1/10/23

# Purpose: 
# 1) calculate the mean precipitation during a given window of time before
# the average biomass harvest date for a site
# 2) calculate drought severity for trmt and control, based on the actual
# amount of precipitation received during the given year during that window


# dependencies ------------------------------------------------------------

library(tidyverse)
source("R_scripts/functions.R") 
source("R_scripts/dropbox_path.R")

# user params -------------------------------------------------------------

# width (in days) of the window of interest
window <- 120
date_string <- "2023-01-10" # for appending to output file names

# read in data ------------------------------------------------------------


# *mswep ------------------------------------------------------------------
# file created in 02_mswep_extract_daily_ppt.R
p_mswep <- newest_file_path(
  path = file.path(path,'IDE/data_raw/climate/mswep/'),
  file_regex = 'mswep_ppt_daily_all-sites_\\d{4}-\\d+-\\d+.csv' 
)

mswep1 <- read_csv(p_mswep, show_col_types = FALSE)


# *ppt by site-year data --------------------------------------------------
# file created in 05_precipitation reduction calculations.R
# ppt for the given window summed for drought and conrol prior to each
# biomass date
p <- newest_file_path(
  path = file.path(path,'IDE Meeting_Oct2019/data/precip/'),
  file_regex = paste0('anpp_clean_trt_ppt_no-perc_', window,
                      "-0days_\\d{4}-\\d+-\\d+.csv"))
)

sites_full4 <- read_csv(p, show_col_types = FALSE)


# * yrly data -------------------------------------------------------------

sites_full5 <- sites_full4 %>% 
  filter(!fake_bioDat, !annual_ppt_used) %>% 
  select(site_code, year, biomass_date, ppt_mswep, block, plot, trt)

# max day of year sampling date (this is the mean of the max dates for
# control and drought plots)
yearly_max_doy <- sites_full5 %>% 
  group_by(site_code, year, trt) %>% 
  # last biomass date (for each treatment) by year
  filter(biomass_date == max(biomass_date)) %>% 
  mutate(doy = lubridate::yday(biomass_date)) %>% 
  group_by(site_code, year) %>% 
  summarize(doy = mean(doy), .groups = 'drop') %>% 
  mutate(doy = round(doy))

# the unique yearly means
max_doys <- yearly_max_doy %>% 
  select(site_code, doy) %>% 
  distinct()


sites_full6 <- sites_full5 %>% 
  group_by(site_code, year, trt) %>% 
  filter(biomass_date == biomass_date) %>% 
  select(site_code, year, ppt_mswep, trt) %>% 
  mutate(ppt_source = "mswep", 
         window = paste0(window, "-0days")) %>% 
  rename(seas_ppt = ppt_mswep) %>% 
  distinct()
  

# calculate seasonal averages ---------------------------------------------

max_doys$seas_ppt_avg <- NA 

if(!all(max_doys$site_code %in% mswep2$site_code)){
  stop("some sites missing from mswep dataset")
}

# seasonal averages for the window before each doy for each site
# this loop is slow and takes several minutes
# consider recoding this using foreach to run in parallel
for (i in 1:nrow(max_doys)) {

  df <- mswep1 %>% 
    filter(site_code == max_doys$site_code[i])

  max_doys$seas_ppt_avg[i] <- mean_ppt_window(df = df,
                                              end_doy = max_doys$doy[i],
                                              window = window)
}

# * checking data -----------------------------------------------------------

# note--some sites have strange multiple dates per year
sites_full5 %>% 
  select(site_code, year, biomass_date) %>% 
  group_by(site_code, year) %>% 
  mutate(diff = max(biomass_date) - min(biomass_date)) %>% 
  arrange(desc(diff))

# combining data for output -----------------------------------------------

comb1 <- yearly_max_doy %>% 
  left_join(max_doys, by = c("site_code", "doy")) %>% 
  select(-doy) %>% 
  left_join(sites_full6, by = c("site_code", "year"))


# saving files ------------------------------------------------------------

write_csv(comb1, 
          file.path(path, "IDE/data_processed/climate",
                    paste0("seasonal_ppt_", date_string, ".csv"))
)
