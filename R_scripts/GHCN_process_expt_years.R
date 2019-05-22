###############################################################
# GHCN data for experiment years (e.g.) >= 2014 in most cases)
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

# note: one site started in 1999 and this filter doesn't account for that. 
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

# pseudo code for next section:
# for a given weather station (data frame in the list)
# check if pre-treatment year and first treatment year have good data
# if yes, all good, move one.
# if no, return data from the next closest site
# repeat check, iterate for closest 5 stations(?)

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

precipFull3 <- bind_rows(precipFull2)

# for each site, get distance to the chosen station (in precipFull2)
# and elevetion of the chosen station
precip <- precipFull2[["cedarsav.us"]]
near <- nearStation2[["cedarsav.us"]]

station_distances <- map2(precipFull2, nearStation2,
                          function(precip, near){
  
  if(!is.data.frame(precip)){
    return(NULL)
  }

  if(nrow(precip) == 0){
    return(NULL)
  } # if no data
  print(precip$site_code[1])  
  # error check:
  stopifnot(length(unique(precip$id)) == 1,
            length(unique(precip$site_code)) == 1)
  

  df <- precip[1, ] %>% 
    select(id, site_code) %>% 
    left_join(near, by = c("id", "site_code")) %>% # adding info from near station chose
    select(-c(name, latitude, longitude))
  df
})

station_distances2 <- bind_rows(station_distances)
station_distances2 %>% 
  filter(distance < 100) %>% 
  nrow()




