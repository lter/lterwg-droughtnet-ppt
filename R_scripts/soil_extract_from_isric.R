# Martin Holdrege

# script started May 4, 2021

# Purpose--extract soil info (percent sand) for the IDE sites
# from ISRIC world soil information

# Code inspired by 
# https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/xy_info_from_R.md


# dependencies ------------------------------------------------------------

library(rgdal) # requires having gdal installed (https://trac.osgeo.org/osgeo4w/)
library(gdalUtils)
library(sf)
library(tidyverse)

path <- "~/Dropbox"

# read in lat/lon ---------------------------------------------------------

site1 <- read_csv(file.path(
  path, "IDE MS_Single year extreme/Data/Site_Elev-Disturb.csv"))

site2 <- site1 %>% 
  select(site_code, latitud, longitud) %>% 
  mutate(latitud = as.numeric(latitud),
         longitud = as.numeric(longitud))


# variables of interest ---------------------------------------------------

voi <- "sand"
depth = "0-5cm"
layer = "mean"
voi_layer <- paste(voi, depth, layer, sep = "_") # layer of interest

# base of search path
webdav_path="/vsicurl/https://files.isric.org/soilgrids/latest/data/"

# re-project point data ---------------------------------------------------

# crs 4326 is just regular unprojected lat/lon for commonly used datum
# i think safe to assume lat/lon are in this format
spdata <- st_as_sf(site2,coords = c("longitud", "latitud"), crs = 4326)

# reproject to homolosine 
igh <- '+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
spdata_igh <- st_transform(spdata, igh)


data_igh <- data.frame(st_coordinates(spdata_igh),id = spdata_igh$site_code)


# get values from pixels --------------------------------------------------

fun_pixel_values=function(rowPX,data,VOI,VOI_LYR){
  print(as.character(data$id[rowPX])) # print to track progress
  as.numeric(
    gdallocationinfo(
      srcfile=paste0(webdav_path,"/",VOI,"/", VOI_LYR,".vrt"),
      x=data[rowPX,"X"],
      y=data[rowPX,"Y"],
      geoloc=TRUE,
      valonly=TRUE))
}

# looping through each site for 0-5cm depths
# units are per thousand
sand05_vals <- map_dbl(1:nrow(data_igh), fun_pixel_values, data = data_igh,
                       VOI = voi, VOI_LYR = voi_layer)

