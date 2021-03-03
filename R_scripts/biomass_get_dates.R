# Script to read and parse biomass file
# to get treatment and measurement dates for the site
# used in the GHCN_process_expt_years.R file which is used to download 
# corresponding met data. 

# script started 5/21/19

library(tidyverse)
library(stringr)
library(lubridate)
source("R_scripts/functions.R")

# path to dropbox folder
path <- '~/Dropbox'

# grabbing newest file
p1 <- newest_file_path(
  path =  file.path(path, "IDE MS_Single year extreme/Data"),
  file_regex = "Full_Biomass-SurveyResults_\\d{1,2}-\\d{1,2}-\\d{4}.csv",
  mdy = TRUE)

p1

bio1 <- read.csv(p1, as.is = TRUE, na.strings = c("NULL"))

# extract year of measurement
# pozos.ar doesn't have biomass date. 
# neudamm.na, only has pretreatment data
trt_yrs <- bio1 %>% 
  # parse date string (not working)
  # mutate(biomass_date = str_replace(biomass_date, "/n", ""),
  #        biomass_date = parse_date(biomass_date),
  #        first_treatment_date = str_replace(first_treatment_date, "/n", ""),
  #        first_treatment_date = parse_date(first_treatment_date)) %>% 
  group_by(site_code) %>% 
  summarise(first_treatment_year = min(first_treatment_year),
            pre_treatment_year = first_treatment_year - 1,
            last_year = max(year))

# quality check:
trt_yrs %>% 
  filter(!complete.cases(.)) # where are NAs

trt_yrs$site_code %>% 
  unique() %>% 
  length() # should equal number of sites


