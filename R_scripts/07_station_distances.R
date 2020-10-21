# Distances between weather stations and IDE sites

# this script has been updated--the question it answers is: what is the 
# distance from site to station for the weather stations used?

# earlier version (see commit history) pulled in distances from survey
# questionnare

# script started 9/28/19


# packages etc ------------------------------------------------------------

library(tidyverse)
library(lubridate)
source("R_scripts/functions.R")
theme_set(theme_classic())

# directory to may 2019 meeting folder
path <- "~/Dropbox"


# load data ---------------------------------------------------------------

# from 06_calculate_cdf.R
p1 <- newest_file_path(
  path = file.path(path,'IDE MS_Single year extreme/Data/precip'),
  file_regex = 'precip_by_trmt_year_with_percentiles_\\d{4}-\\d+-\\d+.csv' 
)

wide2save <- read_csv(p1)

# from 05_precipitation reduction calculations.R
p2 <- newest_file_path(
  file.path(path, 'IDE Meeting_Oct2019/data/precip'),
  "anpp_clean_trt_ppt_no-perc_\\d{4}-\\d+-\\d+.csv")

sites_full3 <- read_csv(p2)

# ghcn data (for distances)
p3 <- newest_file_path(
  path = file.path(path, 'IDE Meeting_Oct2019/data/precip'),
  file_regex = 'GHCN_daily_precip_\\d{4}-\\d+-\\d+.csv' 
)

ghcn1 <- read_csv(p3)

# submitted station data
# from 02_process_submitted_weather.R

p4 <- newest_file_path(
  path = file.path(path, 'IDE Meeting_Oct2019/data/precip'),
  file_regex = 'submitted_weather_station_info_\\d{4}-\\d+-\\d+.csv' 
)

sub_stn1 <- read_csv(p4)

# determine sites/yrs used -----------------------------------------------

# site and year criteria used for 1 year ms extreme paper. 
# this could be improved by pulling in specific list of sites used from Kate
site_yrs <- wide2save %>% 
  filter(n_treat_days > 120 & n_treat_days < 650, perc_reduction > 15,
         !annual_ppt_used) %>% 
  group_by(site_code) %>% 
  filter(year == min(year)) %>% 
  ungroup() %>% 
  select(site_code, year)


# source of data ----------------------------------------------------------

# ghcn or submitted data used
source_used <- sites_full3 %>% 
  filter(!annual_ppt_used, !is.na(ppt)) %>% 
  # approx equality check
  mutate(ghcn_used = abs(ppt - ppt_ghcn) < 0.0001,
         ghcn_used = ifelse(is.na(ghcn_used), FALSE, ghcn_used),
         year = year(biomass_date)) %>% 
  group_by(year, site_code) %>% 
  summarize(ghcn_used = mean(ghcn_used),
            ghcn_used = as.logical(ghcn_used)) %>% 
  right_join(site_yrs, by = c("site_code", "year"))
  

# ghcn distances ----------------------------------------------------------

# at this point the original ghcn code was writting to just pull
# data from one wx station. If that were to change then we'd want to
# change this code to get station specific distances. 
ghcn2 <- ghcn1 %>% 
  group_by(site_code) %>% 
  summarize(ghcn_dist = mean(distance))


# submitted distances -----------------------------------------------------
# distances to stations from submitted wx
sub_stn2 <- sub_stn1 %>% 
  filter(!is.na(distance), site_code != "unknown") 

sub_stn2 %>% 
  group_by(site_code) %>% 
  summarize(n = n()) %>% 
  arrange(desc(n))

sub_stn3 <- sub_stn2 %>% 
  group_by(site_code) %>% 
  # to be conservative taking the farther station if two stations given
  summarize(sub_dist = max(distance))


# combine distances -------------------------------------------------------

dist1 <- source_used %>% 
  ungroup() %>% 
  left_join(ghcn2, by = "site_code") %>% 
  left_join(sub_stn3, by = "site_code") %>% 
  mutate(distance = ifelse(ghcn_used, ghcn_dist, sub_dist))


# summary -----------------------------------------------------------------

summary(dist1$distance)
filter(dist1, distance == max(distance, na.rm = TRUE))

hist(dist1$distance)

plot(sub_dist ~ ghcn_dist, data = dist1)
abline(a = 0, b = 1)
