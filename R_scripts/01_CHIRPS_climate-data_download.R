# Written by Martin Holdrege

# Script started April 5, 2022

# Purpose:


# dependencies ------------------------------------------------------------

source("R_scripts/dropbox_path.R") # this loads the path to the dropbox
library(rgee) # see https://r-spatial.github.io/rgee/ for install details
library(tidyverse)

# connect to gee ----------------------------------------------------------
# note: this script requires a google earth engine account to 
# work, 
rgee::ee_Initialize(user = 'martinholdrege', drive = TRUE)

# load site locations -----------------------------------------------------

site1 <- read_csv(file.path(
  path, "IDE MS_Single year extreme/Data/Site_Elev-Disturb.csv")) 

# load ee assets-----------------------------------------------------------
# see:
# https://developers.google.com/earth-engine/datasets/catalog/UCSB-CHG_CHIRPS_DAILY?hl=en#bands

chirps1 <- ee$ImageCollection('UCSB-CHG/CHIRPS/DAILY')$select('precipitation')

# create feature collection of site codes--------------------------------
# creating a feature collection (like a shapefile) for use by earth engine

site2 <- site1 %>% 
  select(site_code, latitud, longitud) %>% 
  mutate(longitud = as.numeric(longitud))

fc_l1 <- pmap(site2, function(site_code, latitud, longitud) {

  # create a feature collection, for one site
  fc <- ee$FeatureCollection(list(
    ee$Feature(
      ee$Geometry$Point(list(longitud, latitud)),
      list(site_code = site_code)
    )))

  fc
}) 

site_codes <- site2$site_code
names(fc_l1) <- site_codes

# extract chirps data -----------------------------------------------------

# if testing restrict to a small date range
startDate <- '1981-01-01' # this is when the dataset starts 
#startDate <- '2022-02-01'
endDate <- '2022-02-28' 
chirps1 <-  chirps1$filter(ee$Filter$date(startDate, endDate))

# for testing
#point = fc_l1[[1]]
#image = ee$Image(chirps1$first())
# site_code = 'antelope.us'
tasks <- list()
for (site_code in site_codes) {
  point <- fc_l1[[site_code]]
  timeSeries = ee$FeatureCollection(chirps1$map(function(image) {
    
    stats = image$reduceRegion(
      reducer= ee$Reducer$first(),
      geometry= point$geometry(),
      scale =  10L
    )
    
    # reduceRegion doesn't return any output if the image doesn't intersect
    # with the point or if the image is masked out due to cloud
    # If there was no value found, we set the ppt to a NoData value -9999
    ppt = ee$List(list(stats$get('precipitation'), -9999))$reduce(ee$Reducer$firstNonNull())
    
    # Create a feature with null geometry and ppt value and date as properties
    f = ee$Feature(NULL, list(
      'precipitation' = ppt,
      'date' = ee$Date(image$get('system:time_start'))$format('YYYY-MM-dd'),
      'site_code' = site_code))
    f
  }))
  
  file_name <- paste0("CHIRPS_ppt_", site_code, "_",
                      startDate, "_", endDate)
  task_vector <- ee_table_to_drive(
    collection = timeSeries,
    description = file_name,
    folder = 'CHIRPS',
    fileNamePrefix = file_name,
    fileFormat = 'CSV'
  )
  task_vector$start()
  #ee_monitoring(task_vector) # optional
  tasks[[site_code]] <- task_vector

}

# once the above tasks have run, the files should be downloaded

# ee_drive_to_local(task = task_vector, 
#                   dsn = file.path("~", file_name))











