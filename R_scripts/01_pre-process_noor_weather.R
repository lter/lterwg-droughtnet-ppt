# Script for pre-processing weather data submitted for the Noor site, so that it fits into the
# processing pipeline in process_submitted_weather.R script.


# Script started 2/26/21


# The purpose here is to convert monthly precip values into average daily precip 
# and make other formatting changes etc. 


# packages etc ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
source("R_scripts/functions.R")


path_may <- "~/Dropbox/IDE Meeting_May2019"


# read in data ------------------------------------------------------------

p1 <- file.path(path_may, "IDE_weather/submitted_data/noor/Noshahr_station_from_Mehdi-Abedi.xlsx")


df1 <- read_xlsx(p1)

names(df1)

# template
temp <- readRDS(file.path(path_may, "IDE_weather/IDE_weather_template.RDS"))


# column names -----------------------------------------------------------

df2 <- df1 %>% 
  rename(year = ...2,
         month = ...3,
         precip = rrr24,
         min_temp = tmin_m,
         max_temp = tmax_max) %>% 
  select(-matches("...\\d+"))





# site/station info -------------------------------------------------------

sites <- temp$sites
station <- temp$station
sites$pi <- "Abedi, Mehdi"
sites$site <- "Noor"
station$site <- sites$site[1]
station$station_name <- df2$station_name[1]
station$station_id <- df2$station_id[1]


# weather sheet -----------------------------------------------------------

# convert to monthly to daily-

weather <- monthly2daily_precip(df2, year = "year",
                             month = "month", precip = "precip")

weather$min_temp <- monthly2daily_temp(df2, year = "year",
                                    month = "month", temp = "min_temp")

weather$max_temp <- monthly2daily_temp(df2, year = "year",
                                    month = "month", temp = "max_temp")
weather$station_name <- station$station_name

weather$note_weather <- "temp and precip from monthly values"

# saving file -------------------------------------------------------------

out <- list("metadata" = temp$metadata,
            "sites" = sites,
            "station" = station,
            "weather" = weather)

dest <- file.path(path_may, "IDE_weather/submitted_data/Noor_weather_clean.xlsx")

# openxlsx::write.xlsx(out,dest)
