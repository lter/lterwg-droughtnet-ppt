# Script to read and parse biomass file
# to get treatment and measurement dates for the site
# used in the GHCN_process_expt_years.R file which is used to download 
# corresponding met data. 

# script started 5/21/19

library(tidyverse)
# library(lubridate)

# path to data folders
path <- 'C:/Users/grad/Dropbox/IDE Meeting_May2019'

bio1 <- read.csv(file.path(path, "Full biomass\\full_biomass_5-21-2019.csv"),
                 as.is = TRUE, na.strings = c("NULL"))

# extract year of measurement
trt_yrs <- bio1 %>% 
  group_by(site_code) %>% 
  summarise(first_treatment_year = min(first_treatment_year),
            pre_treatment_year = first_treatment_year - 1,
            last_year = max(year))

# quality check (where are NAs?):
trt_yrs %>% 
  filter(!complete.cases(.))

# CAUTION: following line of code may not be needed/could be wrong
# if database updated. 
trt_yrs$pre_treatment_year[trt_yrs$site_code == "pne.br"] <- 2017 # filling in missing value. 


