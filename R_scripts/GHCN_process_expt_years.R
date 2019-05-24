###############################################################
# GHCN data for experiment years (e.g.) >= 2013 in most cases)
##############################################################


# dependencies ------------------------------------------------------------

library(rnoaa)
library(tidyverse)
library(lubridate)
library(purrr)

source('R_scripts/functions.R') # functions used in script
source("R_scripts/biomass_get_dates.R") # biomass dates

# path to data folders
path <- 'C:/Users/grad/Dropbox/IDE Meeting_May2019'

# parse site elevation ----------------------------------------------------

siteElev <-read.csv(file.path(path, 'IDE Site Info/Site_Elev-Disturb.csv'),
                    as.is = TRUE)
siteElev2 <- siteElev %>% 
  select(site_code, elev)

# one site (ethadb.au) has no elevation
siteElev2 %>% 
  arrange(desc(elev))

# load site locations -----------------------------------------------------

site <- read.csv(file.path(path, 'IDE Site Info/Sites_Loc_DrtTrt.csv'), 
                 as.is = TRUE)

site <- site[,c('site_code','lat','long')]
colnames(site) <- c('id','lat','long') # names used for library(rnoaa) function

# load station data -------------------------------------------------------

stations <- read.csv(file.path(path, 'IDE Site Info/GHCND_Stations.csv'), 
                     as.is = TRUE)

stations$X <- NULL # unnecessary column created in csv

# extract closest n stations to each site ---------------------------------

# note: one site started in 1999 and this filter doesn't account for that.
# (though can likely still find data for it)

stationsPpt <- stations %>% 
  filter(element == "PRCP", first_year <= 2013, last_year > 2013)

# stations within 100 km of each site
nearStation0 <- meteo_nearby_stations(site,lat_colname='lat',lon_colname = 'long',
                                      station_data = stationsPpt, var = "PRCP", 
                                      radius = 100)
near <- nearStation0[[1]]
site_code <- names(nearStation0)[1]


# adding station and site elevation to all stations within 100km of a given site.
stations_elev <- stations %>% 
  select("id", "name", "elevation")

nearStation0b <- map2(nearStation0, names(nearStation0), 
                      function(near, site_code){
                      out <- near %>% 
                        # add in station elevation
                        left_join(y = stations_elev, 
                                  by = c("id", "name")) %>% 
                        select(names(near), elevation) %>% 
                        rename(station_elevation = elevation) %>% 
                        mutate(site_code = site_code) %>% 
                        # add in site elevation
                        left_join(siteElev2, by = "site_code") %>% 
                        rename(site_elevation = elev) %>% 
                        mutate(diff_elevation = site_elevation - station_elevation) %>% 
                        distinct() # discard duplicated rows (not sure why occur)
                      
                      out
                    })

# number of sites that have stations within 100km
(sapply(nearStation0b, nrow) > 0) %>% 
  sum()

# filter out stations 500m higher/lower than site.

nearStation <- lapply(nearStation0b, function(df){
  out <- df %>%
    # keeping site where elevation not given.
    filter(abs(diff_elevation) < 500 | is.na(diff_elevation))
  out
})

# number of sites that have stations within 100km and 500m elevation
(sapply(nearStation, nrow) > 0) %>% 
  sum()

# adding rank of nearest stations (ranked by distance)
nearStation2 <- lapply(nearStation, function(df){
  if(nrow(df) == 0){
    return(NULL)
  }
  df %>% 
    arrange(distance) %>% 
    # rank is nothing if no rows
    mutate(rank = 1:nrow(df))
})

nearStation2_df <- bind_rows(nearStation2) # used later for joining distance data
# extracting just the nearest station for each site
nearest <- lapply(nearStation, function(df){
  if(nrow(df) == 0){
    return(NULL)
  }
  df[1, ]
})

nearest_df <- bind_rows(nearest)

# for each site, getting data from nearest station

# data frame of precip data from nearest station to each site
# see functions.r file for details
precip <- ghcn_download_parse(nearest_df, return_list = TRUE) 

precipFull <- lapply(precip, ghcn_parse_dates)

# code for next section:
# for a given weather station (data frame in the list)
# check if pre-treatment year (and year before that) and first treatment yr and 2nd treat year are present and have good data
# (i.e. 4 years)
# if yes, all good, move one return those three important years, (and following years if the data is present, regardless of whether is is complete)
# if no, return data from the next closest site
# repeat check, iterate for next closest stations (all those within 100km)

# note kiskun.hu met data problematic (years with 0 precip, but not NAs)
precipFull2 <- lapply(precipFull, function(df){
  # error checking
  stopifnot(length(unique(df$site_code)) == 1)
  
  sc <- df$site_code[1]
  print(sc)
  # data on when biomass measured (from biomass_get_dates.R file)
  yrs_site <- trt_yrs %>% 
    filter(site_code == sc)
  # vector of years during which measurements taken, 
  # plus year before pre-treatment
  years <- (yrs_site$pre_treatment_year-1):yrs_site$last_year
  
  if(length(years) == 1) message("file indicates only getting 1 year data")
  
  # only precip data for when biomass measured
  df2 <- df %>% 
    filter(year %in% years) 
  
  annual <- good_days_per_yr(df2)
  
  # most important years are the pre treat years, first treat year, and 
  #   second treat year if it exists, so dataset must be good for those years
  important_years <- if(length(years) <= 3){
    years
  } else {
    years[1:4]
  }
  is_good <- annual %>%
    filter(year %in% important_years) %>% 
    .$is_good
  # are all the important years in the df?
  important_years_present <- important_years %in% df2$year 
  
  if(all(is_good, important_years_present)){
    return(df2) # return the good station data
    
  } else if(nrow(nearStation2[[sc]]) < 2) {
    
    return(NULL) # if no additional stations to look for
  
  } else {
    # next nearest stations (closest one already checked above)
    near <- nearStation2[[sc]] 
    for (i in 2:nrow(near)){
      # download data
      tmp1 <- ghcn_download_parse(near[i, ], return_list = FALSE) 
      tmp2 <- ghcn_parse_dates(tmp1) # parse
      tmp3 <- tmp2 %>% 
        filter(year %in% years) # filter years for experiment
      
      annual <- good_days_per_yr(tmp3)
      
      # checking if pre treat yr and first trt year are good
      is_good <- annual %>%
        filter(year %in% important_years) %>% 
        .$is_good
      # are the important years present
      important_years_present <- important_years %in% tmp3$year
      if(all(is_good, important_years_present)){
        return(tmp3)
      }
    }
  }
  NULL
})

precipFull3 <- bind_rows(precipFull2)

# names of sites that have data from a station within 100 km
precipFull3$site_code %>% 
  unique()
 
# add back info on distance/elevation to station 
precipFull4 <- nearStation2_df %>% 
  select(id, site_code, distance, matches("elevation")) %>% 
  right_join(precipFull3, by = c("id", "site_code"))

# write.csv(precipFull4,
#           file.path(path, 'IDE Site Info/GHCN_daily_precip_20190523.csv'),
#           row.names = FALSE)

# data check (comparing to mannualy calculated values)
hw <- precipFull4 %>% 
  filter(site_code == "hard.us") %>% 
  group_by(year) %>% 
  summarize(ap = sum(ppt, na.rm = TRUE))


  