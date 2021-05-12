# Script for pre-processing GCN submitted weather, so that it fits into the
# processing pipeline in process_submitted_weather.R script.


# Script started 10/31/19


# The purpose here is to convert monthly precip values into average daily precip 
# and make other formatting changes etc. 


# packages etc ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

source("R_scripts/functions.R")

path_may <- "~/Dropbox/IDE Meeting_May2019"


# read in files -----------------------------------------------------------

# the template

temp_sheets <- c("metadata", "sites", "station", "weather")
temp1 <- map(temp_sheets, function(sheet) {
  read_xlsx(file.path(path_may, "IDE_weather/IDE_weather_template.xlsx"),
            sheet = sheet)
})
names(temp1) <- temp_sheets

# the GCN file

gcn_path <- file.path(
  path_may, 
  "IDE_weather/submitted_data/GCN/GCN-ten sites-precipitation2017and historical data.xlsx"
  )

gcn_sheets <- excel_sheets(gcn_path)

gcn1 <- map(gcn_sheets, function(sheet) {
  read_xlsx(gcn_path, sheet = sheet)
})
names(gcn1) <- gcn_sheets

# read in GCN_Bange seperately (daily data)

GCN_Bange <- read_xlsx(
  file.path(path_may, "IDE_weather/submitted_data/GCN/GCN-Bange-precipitation.xlsx")
  )


# discard sites with annual only data -------------------------------------

map(gcn1, names)

gcn2 <- keep(gcn1, function(x) {
  "Month" %in% names(x)
})

length(gcn2) # excluded two sites


# convert to daily precip -------------------------------------------------

# here daily precip is calculated as the amount of precip in that month 
# divided by number of days in that month
# this is so that we can get say get total precip in the 12 months prior to say Aug 15 2018
# in analyses in downstream pipelines, and make the data fit into those pipelines.

gcn3 <- map(gcn2, .f = monthly2daily_precip, year = "Year", month = "Month",
            precip = "Precipitation/mm")

map_dbl(gcn3, nrow)

# create tables ---------------------------------------------------------

# GCN_Bange ~~~~~

gcn3[["GCN-Bange"]] <- GCN_Bange %>% 
  mutate(date = ymd(paste(Year, Month, Day, sep = "-")),
         precip = `Cumulative precipitation at 20:00-20:00(0.1mm)`, 
         precip = ifelse(precip == 32700, 0, precip),# little precip
         precip = ifelse(precip == 32766, NA, precip), # missing data
         precip = ifelse(precip == 32744, NA, precip), # no data
         precip = precip/10 # 0.1mm to mm
  ) %>% 
  select(date, precip) 

# create tables that match the template

temp1

# I am arbitrarily naming the station_name the name of the site
gcn4 <- map2(gcn3, names(gcn3), function(x, name) {
  out <- list()
  out$weather <- x %>% 
    as_tibble() %>% 
    mutate(station_name = name,
           min_temp = NA_real_,
           max_temp = NA_real_,
           note_weather = "Daily weather computed from monthly values")
  
  out$station <- tibble(
    site = name,
    station_name = name,
    station_latitud = NA,
    station_longitud = NA,
    elev = NA,
    distance = NA,
    station_id = NA,
    source = NA,
    url = NA,
    note_station = NA
  )
  out$sites <- tibble(
    pi = NA,
    site = name,
    site_latitud = NA,
    site_longitud = NA
  )
  out$metadata = temp1$metadata
  out <- out[c("metadata", "sites", "station", "weather")] # standard ordering
  out
})


# gcn4[[1]]

# removing "-" from list names so can subset without using ``
names(gcn4) <- names(gcn4) %>% 
  stringr::str_replace("-", "_")

gcn4$GCN_Bange$weather$note_weather <- NA # not computed from monthly values
# saving the data ---------------------------------------------------------

saveRDS(gcn4,
        file =  file.path(
          path_may, 
          "IDE_weather/submitted_data/GCN/GCN-weather-cleaned_2019-12-02.rds"
        ))
