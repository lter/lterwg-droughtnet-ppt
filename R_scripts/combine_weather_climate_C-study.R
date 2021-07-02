# Martin Holdrege

# Script started 7/2/2021

# Purpose: pull together climate and weather data for Baoku Shi for soil C
# sequestration study. This is only for a subset of IDE sites

# this script requires that scripts up through 04_...have been run to create
# the necessary files that are read in here. 

# dependencies ------------------------------------------------------------

library(tidyverse)
source("R_scripts/functions.R")
path <- "~/Dropbox/" # path to IDE dropbox folder

# read in data ------------------------------------------------------------


# * sites -----------------------------------------------------------------
# site codes of sites included in C sequestration study

sites1 <- readxl::read_xlsx(file.path(
  path, "IDE_data_May 2018/BaokuData/Drought Effects on soil carbon_Site overview-all-21.5.8.xlsx"),
  sheet = "Site codes",
  na = c("", "NA"))

site_codes <- sites1$`Site codes`[!is.na(sites1$`Site codes`)] %>% 
  unique()

# * submitted weather data ------------------------------------------------
# compiled/cleaned weather data submitted by PIs

# grabbing most recent submitted daily weather file
p1 <- newest_file_path(
  path = file.path(path, "IDE Meeting_Oct2019/data/precip"),
  file_regex =  "submitted_daily_weather_WC_supplemented_\\d{4}-\\d{2}-\\d{2}.csv" 
)
p1

wthr_sub1 <- read_csv(p1)
wthr_sub1$X1 <- NULL


# * noaa weather data -----------------------------------------------------
# data automatically downloaded for sites using rnoaa

# GHCN data--getting newest file based on file name
p2 <- newest_file_path(
  path = file.path(path, "IDE Meeting_Oct2019/data/precip"),
  file_regex = 'GHCN_daily_precip_\\d{4}-\\d+-\\d+.csv' 
)
p2
wthr_ghcn1 <- read_csv(p2)


# * climate data ----------------------------------------------------------

# monthly precipitation data from world clim. We never pulled temperature
# data
clim1 <- read_csv(file.path(path, "IDE MS_Single year extreme/Data/precip", 
                            "worldclim_monthly_precip.csv"))

# combine weather data ----------------------------------------------------

wc_string <- "missing value filled in using worldclim mean values."

wthr_sub2 <- wthr_sub1 %>% 
  filter(site_code %in% site_codes) %>% 
  # making note of weather worldclim data used
  mutate(note_weather = ifelse(wc == "Y",
                               paste(wc_string, note_weather),
                               note_weather)) %>% 
  select(site_code, station_name, date, precip, min_temp, max_temp, mean_temp, 
         note_weather) 

# unique site/date combination of submitted data
site_date <- paste(wthr_sub2$site_code, wthr_sub2$date, sep = "_")

# temperature data not pulled when using ghcn
wthr_ghcn2 <- wthr_ghcn1 %>% 
  mutate(site_date_ghcn = paste(site_code, date, sep = "_")) %>% 
  filter(site_code %in% site_codes,
         # excluding site/dates for which data submitted
         !site_date_ghcn %in% site_date) %>% 
  rename(station_name = id,
         precip = ppt) %>% 
  mutate(note_weather = "data pulled from nearest GHCN station") %>% 
  select(site_code, station_name, date, precip, note_weather)


wthr1 <- bind_rows(wthr_sub2, wthr_ghcn2) %>% 
  arrange(site_code, date)

wthr1$site_code %>% 
  unique() %>% 
  length() # 21 sites have data


# clean climate data ------------------------------------------------------

# calculate annual data
clim2 <- clim1 %>% 
  filter(site_code %in% site_codes) %>% 
  rename(annual_ppt = wc_ppt) %>% 
  group_by(site_code, year) %>% 
  summarise(annual_ppt = sum(annual_ppt),
            .groups = "drop") %>% 
  mutate(annual_temp = NA,
         note_climate = "data from worldclim")

clim2$site_code %>% 
  unique() %>% 
  length() # 21 sites have data

# save data ---------------------------------------------------------------

# daily weather file
write_csv(wthr1, 
          file.path(path, "IDE_data_May 2018/BaokuData/weather_C-study.csv"))

# climate file

write_csv(clim2, 
          file.path(path, "IDE_data_May 2018/BaokuData/climate_C-study.csv"))
