###############################################################
# GHCN data for experiment years (i.e. >= 2014)
##############################################################

library(rnoaa)
library(tidyverse)
library(lubridate)
library(purrr)


# functions used in this script (an others)
source('R_scripts/functions.R')
source("R_scripts/biomass_get_dates.R")

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
  filter(element == "PRCP", first_year <= 2013, last_year > 2013)

nearStation <- meteo_nearby_stations(site,lat_colname='lat',lon_colname = 'long',station_data = stationsPpt,limit = 5)

# adding rank of nearest stations (ranked by distance)
names <- names(nearStation)
nearStation2 <- map2(nearStation, names, function(df, y){
  df %>% 
    arrange(distance) %>% 
    mutate(rank = 1:nrow(df),
           site_code = y)
})

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

# data frame of precip data from nearest station to each site
# see functions.r file for details
precip <- ghcn_download_parse(nearest_df, return_list = TRUE) 

precipFull <- lapply(precip, ghcn_parse_dates)

# pseudo code:
# for a given weather station (data frame in the list)
# check if pre-treatment year and first treatment year have good data
# if yes, all good, move one.
# if no, return data from the next closest site
# repeat check, iterate for closest 5 stations?

precipFull2 <- lapply(precipFull, function(df){
  sc <- df$site_code[1]
  # data on when biomass measured (from biomass_get_dates.R file)
  yrs_site <- trt_yrs %>% 
    filter(site_code == sc)
  # vector of years during which measurements taken
  years <- yrs_site$pre_treatment_year:yrs_site$last_year
  
  # only precip data for when biomass measured
  df2 <- df %>% 
    filter(year %in% years) 
  
  annual <- good_days_per_yr(df2)
  
  # checking if pre treat yr and first trt year are good
  is_good <- annual %>%
    filter(year %in% years[1:2]) %>% 
    .$is_good
  
  
  if(all(is_good)){
    return(df2)
  } else {
    # nearest stations (closest one already checked above)
    near <- nearStation2[[sc]] 
    for (i in 2:nrow(near)){
      tmp1 <- ghcn_download_parse(near[i, ], return_list = FALSE) 
      tmp2 <- ghcn_parse_dates(tmp1)
      tmp3 <- tmp2 %>% 
        filter(year %in% years) 
      
      annual <- good_days_per_yr(tmp3)
      
      # checking if pre treat yr and first trt year are good
      is_good <- annual %>%
        filter(year %in% years[1:2]) %>% 
        .$is_good
      if(all(is_good)){
        return(tmp3)
      }
    }
  }
  NULL
})

sapply(precipFull2, is.null) %>% 
  sum()

ppt_summary <- ppt_annual2 %>% 
  group_by(id, site_code) %>% 
  summarize(n_good_yrs = n(), # number of good years per stations
            year_start = min(year), # first "good year"
            year_end = max(year),# last good year
            map = mean(ap)) %>% 
  arrange(desc(n_good_yrs))

