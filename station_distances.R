# Distances between weather stations and IDE sites

# This script is meant to look at the distances of GHCN stations to sites
# (ie those auto downloaded using rnoaa and those ) vs distances to sites
# of PI submitted stations. 
# If coordinates are good enough also calculate different between GHCN stations
# and PI submitted stations (in some cases the station auto selected will be 
# the same as the station provided by PI).

# script started 9/28/19


# packages etc ------------------------------------------------------------

library(tidyverse)

# directory to may 2019 meeting folder
path_may <- "E:/Dropbox/IDE Meeting_May2019"

# load data ---------------------------------------------------------------

# survey of who has access to weather station data etc.
survey1 <- read.csv(file.path(path_may, "IDE Survey/SurveyResults_9-27-2019.csv"),
                    as.is = TRUE,
                    encoding = "UTF-8")



# process survey ----------------------------------------------------------

head(survey1)
names(survey1)
survey2 <- survey1 %>% 
  rename(
    site_name = What.is.your.site.name.,
    pi = What.is.the.PI.s.name.,
    site_name = What.is.your.site.name.,
    site_coords = What.are.your.site.s.coordinates.,
    station_access = Do.you.have.access.to.daily.weather.data.for.your.site.from.a.weather.station.,
    station_coords = What.are.the.coordinates.for.your.weather.station.,
    elev_diff = What.is.the.approximate.elevation.difference..m..from.the.closest.weather.station.to.your.site.,
    distance = What.is.the.approximate.distance..km..from.the.closest.weather.station.to.your.site.,
    station_id = Provide.the.station.code..if.applicable.,
    source = What.is.the.source.of.your.daily.weather.) %>% 
  select(site_code, site_name, site_coords, pi, station_access, distance,
         station_coords, elev_diff, source, station_id, Comments)

# no decimal included in the coords, but not sure what correct value should be 
# survey2[survey2$site_code == "torla.es", ]$station_coords <- "74.1213  47.25902"

# parsing the lat/longitude
survey3 <- survey2 %>% 
  mutate(
    # is it in decimal degree format?
    dec_deg = str_detect(
      station_coords, 
      "^[^0-9]*[0-9]{1,3}[,.][0-9]+[^0-9]+[0-9]{1,3}[,.][0-9]+[^0-9]*$"),
    station_lat = ifelse(
      dec_deg, 
      str_extract(station_coords, "^[^0-9]*[0-9]{1,3}[,.][0-9]+"),
      NA),
    station_lon = ifelse(
      dec_deg, 
      str_extract(station_coords, "[^0-9]+[0-9]{1,3}[,.][0-9]+[^0-9]*$"),
      NA),
    station_lat = station_lat %>% 
      str_replace_all(",",".") %>% 
      str_replace_all("^[^-0-9]+", "")%>% 
      str_replace_all("[^-0-9]+$", ""),
    station_lon = station_lon %>% 
      str_replace_all(",",".") %>% 
      str_replace_all("^[^-0-9]+", "") %>% 
      str_replace_all("[^-0-9]+$", ""),
    lon_deg = station_coords %>% 
      str_extract("[0-9]+[^°0-9]*°[^°]+$") %>% 
      str_extract("[0-9]+"),
    lon_min = station_coords %>% 
      str_extract("°[^°]*[0-9]+[^°]*$") %>% 
      str_extract("[0-9]+"),
    lon_sec = station_coords %>% 
      str_extract("°[^°]*[0-9]+[^°]+[0-9.]+[^°]*$") %>% 
      str_extract("[0-9.]+[^0-9]+$") %>% 
      str_extract("[0-9.]+"),
    lat_deg = station_coords %>% 
      str_extract("^.*[0-9]+[^°]*°") %>% 
      str_extract("[0-9]+"),
    lat_min = station_coords %>% 
      str_extract("°[^°]*[0-9]+") %>% 
      str_extract("[0-9]+"),
    lat_sec = station_coords %>% 
      str_replace("[0-9]+[^°0-9]*°[^°]+$", "") %>% # getting rid of lon
      str_extract("°[^°]*[0-9]+[^°]+[0-9.]+") %>% 
      str_extract("[0-9.]+$")
  ) %>% 
  select(matches("coord"), matches("lat_"), matches("lon_"),  
         matches("station_"), everything())

survey4 <- survey3 %>% 
  mutate(lat_ns = ifelse(dec_deg,
                         NA,
                         str_extract(station_coords, "[NnSs]")), 
         lon_we = ifelse(dec_deg,
                         NA,
                         str_extract(station_coords, "[WsEd]")),
         lon_we = ifelse(site_code == "matador.ca", "W", lon_we), # not entered originally
         lon_we = ifelse(site_code == "rhijn.nl", "E", lon_we),
         lat_ns = ifelse(site_code == "rhijn.nl", "N", lat_ns),
         # if seconds not give, assuming is 0
         lat_sec = ifelse(is.na(lat_sec) & !is.na(lat_deg), 0, lat_sec),
         lon_sec = ifelse(is.na(lon_sec) & !is.na(lon_deg), 0, lat_sec),
         ) %>% 
  mutate_at(.vars = vars(lat_deg, lat_min, lat_sec, lon_deg, lon_min, lon_sec),
            .funs = as.numeric) %>% 
  mutate(
    station_lat = ifelse(
      is.na(station_lat) & !is.na(lat_deg),
      biogeo::dms2dd(lat_deg, lat_min, lat_sec, lat_ns),
      station_lat),
    station_lon = ifelse(
      is.na(station_lon) & !is.na(lon_deg),
      biogeo::dms2dd(lon_deg, lon_min, lon_sec, lon_we),
      station_lon)) %>%
  select(station_coords, station_lat, station_lon, everything())

survey4[survey4$site_code %in% c("ethadb.au", "ethadn.au"),]$station_lat <- 
  biogeo::dms2dd(23, 40.780, 0, "S")

survey4[survey4$site_code %in% c("ethadb.au", "ethadn.au"),]$station_lon <- 
  biogeo::dms2dd(138, 26.011, 0, "E")

# double check lat/lon calculated for all. 
survey4 %>% 
  filter(is.na(station_coords) & (is.na(station_lat) | is.na(station_lon)))
