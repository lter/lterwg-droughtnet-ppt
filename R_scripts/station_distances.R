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
theme_set(theme_classic())


# directory to may 2019 meeting folder
path_may <- "E:/Dropbox/IDE Meeting_May2019"
path_oct <- "E:/Dropbox/IDE Meeting_Oct2019"

# load data ---------------------------------------------------------------

# survey of who has access to weather station data etc.
# NOTE: this is not the newest version of file (newest doesn't yet have site_codes)
survey1 <- read.csv(file.path(path_may, "IDE Survey/SurveyResults_9-27-2019_distance_corrected.csv"),
                    as.is = TRUE,
                    encoding = "UTF-8")

# ghcn data gotten via rnoaa for sites where it was available. 
ghcn_data1 <- read.csv(file.path(path_may, 'IDE Site Info/GHCN_daily_precip_20190523.csv'),
                       as.is = TRUE)

# info on all the sites 
site_elev1 <-read.csv(file.path(path_oct, 'IDE Site Info/Site_Elev-Disturb_UPDATED_9-30-2019.csv'),
                    as.is = TRUE)

# metadata for all ghcnd stations.
stations <- read.csv(file.path(path_may, 'IDE Site Info/GHCND_Stations.csv'), 
                     as.is = TRUE)

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
    distance = distance_cor,
    station_id = Provide.the.station.code..if.applicable.,
    source = What.is.the.source.of.your.daily.weather.) %>% 
  select(site_code, site_name, site_coords, pi, station_access, distance,
         station_coords, elev_diff, source, station_id, Comments)

survey2 %>% 
  filter(!site_name %in% site_elev1$site_name) %>% 
  select(site_name)

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

# lat/lon not provided in way it could be parsed above:
survey4[survey4$site_code %in% c("ethadb.au", "ethadn.au"),]$station_lat <- 
  biogeo::dms2dd(23, 40.780, 0, "S")

survey4[survey4$site_code %in% c("ethadb.au", "ethadn.au"),]$station_lon <- 
  biogeo::dms2dd(138, 26.011, 0, "E")

# double check lat/lon calculated for all. 
survey4 %>% 
  filter(is.na(station_coords) & (is.na(station_lat) | is.na(station_lon)))

survey4 <- survey4 %>% 
  mutate(station_lat = str_replace_all(station_lat, " ", "") %>% 
           as.numeric(),
         station_lon = str_replace_all(station_lon, " ", "") %>% 
           as.numeric()
         )

# process ghcn data -------------------------------------------------------

first <- function(x){
  # return the first value of a vector, assuming all elements are the same
  stopifnot(
    is.vector(x),
    length(unique(x)) == 1
  )
  x[1]
}

# getting ghcn station for each site:
ghcn_stations1 <- ghcn_data1 %>% 
  select(id:diff_elevation) %>% 
  group_by(site_code) %>% 
  summarise_all(.funs = first) %>% 
  rename(ghcn_id = id, dist_ghcn_site = distance, ghcn_elev = station_elevation,
         site_elev = site_elevation, elev_diff_ghcn_site = diff_elevation)

stations2 <- stations %>% 
  filter(element == "PRCP") %>% 
  select(id:name) %>% 
  group_by(id) %>% 
  summarise_all(.funs = first)

# joining in station metadata
ghcn_stations2 <- ghcn_stations1 %>% 
  left_join(stations2, by  = c("ghcn_id" = "id")) %>% 
  rename(ghcn_lat = latitude, ghcn_lon = longitude, ghcn_name = name) %>% 
  select(-state, - elevation)


# process site data -------------------------------------------------------

site_elev2 <- site_elev1 %>% 
  select(site_name:longitud, elev) %>% 
  rename(site_lat = latitud, site_lon = longitud, site_elev = elev)


# combine tables ----------------------------------------------------------

# check--shouldn't be any rows
anti_join(ghcn_stations2, site_elev2, by = "site_code") 


missing <- anti_join(survey4, site_elev2, by = "site_code") %>% 
  select(site_code) %>% 
  filter(!str_detect(site_code, "data")) %>% 
  pull() 
missing

if(length(missing > 1)) {
  warning("not all sites from survey are in the site_elev file")
}
  
# site and station info
site_stn1 <- ghcn_stations2 %>% 
  select(-site_elev) %>% 
  right_join(site_elev2, by = "site_code")
names(site_stn1)

names(survey4)

# pi_stn is meant to denote pi selected weather station
site_stn2 <- survey4 %>% 
  rename(pi_stn_lat = station_lat, 
         pi_stn_lon = station_lon,
         pi_stn_id = station_id,
         pi_stn_acces = station_access,
         elev_diff_site_pi_stn = elev_diff,
         dist_site_pi_stn = distance,
         pi_stn_coords = station_coords
         ) %>% 
  select(site_code, matches("pi_stn")) %>% 
  right_join(site_stn1, by = "site_code")


# calculated distances ----------------------------------------------------

# distance between GHCN selected station and pi specified station
site_stn2$dist_ghcn_pi_stn <- geosphere::distHaversine(
  p1 = as.matrix(site_stn2[, c("pi_stn_lon", "pi_stn_lat")]),
  p2 = as.matrix(site_stn2[, c("ghcn_lon", "ghcn_lat")])
)/1000 #(convert m to km)


# figures -----------------------------------------------------------------
site_stn3 <- site_stn2

caption <- paste0("Figure generated in station_distances.R script on ", 
                  lubridate::today())

# pdf(file.path(path_oct, "figures/precip/station_distances_2019-09-30.pdf"),
#     height = 5, width = 8)

ggplot(site_stn3) + 
  geom_point(aes(dist_ghcn_site, dist_site_pi_stn, size = dist_ghcn_pi_stn),
             alpha = 0.5) +
  geom_abline(slope = 1, intercept = 1) +
  labs(x = "Distance (IDE site to GHCN station, km)",
       y = "Distance (IDE site to PI selected station, km)",
       title = "Distances between IDE sites and weather stations",
       subtitle = "Coordinates of PI selected stations is from Survey",
       caption = caption)+
  guides(size=guide_legend(title="Distance (PI station to GHCN)"))

dev.off()


