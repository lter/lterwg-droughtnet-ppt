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

# reproject to homolosine (projection that the soil data is in)
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
# I'm not sure why many of these are NAs
sand05_vals <- map_dbl(1:nrow(data_igh), fun_pixel_values, data = data_igh,
                       VOI = voi, VOI_LYR = voi_layer)
voi_layer <- paste(voi, "5-15cm", layer, sep = "_")
sand15_vals <- map_dbl(1:nrow(data_igh), fun_pixel_values, data = data_igh,
                       VOI = voi, VOI_LYR = voi_layer)

voi_layer <- paste(voi, "5-15cm", layer, sep = "_")
sand15_vals <- map_dbl(1:nrow(data_igh), fun_pixel_values, data = data_igh,
                       VOI = voi, VOI_LYR = voi_layer)

voi_layer <- paste(voi, "15-30cm", layer, sep = "_")
sand30_vals <- map_dbl(1:nrow(data_igh), fun_pixel_values, data = data_igh,
                       VOI = voi, VOI_LYR = voi_layer)

voi_layer <- paste(voi, "30-60cm", layer, sep = "_")
sand60_vals <- map_dbl(1:nrow(data_igh), fun_pixel_values, data = data_igh,
                       VOI = voi, VOI_LYR = voi_layer)

voi_layer <- paste(voi, "60-100cm", layer, sep = "_")
sand100_vals <- map_dbl(1:nrow(data_igh), fun_pixel_values, data = data_igh,
                       VOI = voi, VOI_LYR = voi_layer)

# replace negative values with NA
replace_neg <- function(x) ifelse(x < 0, NA, x)

site3 <- site2 %>% 
  mutate(sand0_5 = sand05_vals,
         sand5_15 = sand15_vals,
         sand15_30 = sand30_vals,
         sand30_60 = sand60_vals,
         sand60_100 = sand100_vals) %>% 
  mutate_at(.vars = vars(matches("sand")),
            .funs = replace_neg) %>% 
  rowwise() %>% 
  mutate(sand_mean = stats::weighted.mean(
    x = c(sand0_5, sand5_15, sand15_30,sand30_60, sand60_100), 
    # weighted by the cm of depth
    w = c(5, 10, 15, 30, 40),
    na.rm = TRUE),
    sand_mean = ifelse(is.nan(sand_mean), NA, sand_mean)) %>% 
  select(site_code, sand_mean, everything(), -longitud, -latitud) %>% 
  # convert to percent
  mutate_if(.predicate = is.numeric,
            .fun = list(function(x) x/10))


# save file ---------------------------------------------------------------

write_csv(site3,
          file.path(path, "IDE MS_Single year extreme/Data",
                    "site_sand_from_soilgrid.csv"))
