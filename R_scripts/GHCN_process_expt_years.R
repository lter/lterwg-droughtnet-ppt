###############################################################
# GHCN data for experiment years (i.e. >= 2014)
##############################################################

library(rnoaa)
library(tidyverse)
library(lubridate)

# functions used in this script (an others)
source('R_scripts/functions.R')


# path to data folders
path <- 'C:/Users/grad/Dropbox/IDE Meeting_May2019'

# site locations
site <- read.csv(file.path(path, 'IDE Site Info/Sites_Loc_DrtTrt.csv'), as.is = TRUE)

site <- site[,c('site_code','lat','long')]
colnames(site) <- c('id','lat','long')

# load csv:
stations <- read.csv(file.path(path, 'IDE Site Info/GHCND_Stations.csv'), as.is = TRUE)

stations$X <- NULL # unnecessary column created in csv

stationsPpt <- stations %>% 
  filter(element == "PRCP", first_year <= 2013)

nearStation <- meteo_nearby_stations(site,lat_colname='lat',lon_colname = 'long',station_data = stationsPpt,limit=3)

# extracting just the nearest station for each site
nearest <- lapply(nearStation, function(x){
  x[1, ]
})

nearest_df <- bind_rows(nearest)
nearest_df$site_code <- names(nearest)

nearest_df[nearest_df$distance > 100,]
nearest_df[nearest_df$distance < 20,]

#nearest_test <- nearest_df[1:5,]


# for each site, getting data from nearest station

precip <- ghcn_download_parse(nearest_df) # see functions.r file for details

# this is the full parsed data for relevant years
precipFull <- precip %>% 
  arrange(site_code, year, month, day_of_month) %>% 
  filter(year >= 2013) %>%  # only keeping data that matches TPA time frame
  mutate(date = make_date(year, month, day_of_month),
         ppt = ppt/10) %>% # convert 10ths of mm to mm
  filter(!is.na(date)) # e.g 30th day of february

ppt_annual <- precipFull %>% 
  group_by(id, site_code, year) %>% 
  summarise(n_days = n(),# number of rows per year/site (number of observations (missing data or not))
            n_NA = sum(is.na(ppt)), # number of days with missing data
            ap = sum(ppt, na.rm = TRUE) #annual precipitation
  ) 


summary(ppt_annual)

# just "good years"
ppt_annual2 <- ppt_annual %>% 
  mutate(n_good = n_days - n_NA) %>%  # number of good observations/year
  filter(n_good > 334)

ppt_summary <- ppt_annual2 %>% 
  group_by(id, site_code) %>% 
  summarize(n_good_yrs = n(), # number of good years per stations
            year_start = min(year), # first "good year"
            year_end = max(year),# last good year
            map = mean(ap)) %>% 
  arrange(n_good_yrs)
