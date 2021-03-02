# Script for pre-processingsand.us submitted weather, so that it fits into the
# processing pipeline in process_submitted_weather.R script.


# Script started 1/26/20


# The purpose here is to convert weekly precip values into average daily precip
# ppt data is provided as 

# packages etc ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)


path_may <- "~/Dropbox/IDE Meeting_May2019"


# read in data ------------------------------------------------------------

p1 <- file.path(path_may, "IDE_weather/submitted_data/sand.us/Young_Sandhills_IDE_weather.xlsx")


sites <- read_xlsx(p1, sheet = "sites")
station <- read_xlsx(p1, sheet = "station")
weather1 <- read_xlsx(p1, sheet = "weather")


# template
temp <- readRDS(file.path(path_may, "IDE_weather/IDE_weather_template.RDS"))


# weekly to daily ppt conversion ------------------------------------------

weather1 <- weather1 %>% 
  mutate(date = as.Date(date),
         week = 1:nrow(weather1))


weekly2daily_precip <- function(df) {
  # not all periods are 7 days long so using n_days
  dates <- seq(from = df$date - df$n_days +1, to = df$date, by = "1 day")
  out <- tibble(date = dates,
                station_name = df$station_name,
                precip = df$precip/df$n_days,
                min_temp = df$min_temp,
                max_temp = df$max_temp,
                note_weather = "weekly precip converted to daily precip")
  out
}

# converting to daily
weather2 <-  weather1 %>% 
  # assuming the first week was 7 days long
  mutate(n_days = c(7, diff(date))) %>% 
  group_by(week) %>% 
  nest() %>% 
  mutate(daily_data = map(data, weekly2daily_precip)) %>% 
  unnest(cols = "daily_data") %>% 
  ungroup() %>% 
  select(-data, -week)

sum(duplicated(weather1$date)) # should be no duplicates
sum(duplicated(weather2$date))

# shouldnt' have changed total
stopifnot(sum(weather2$precip) == sum(weather1$precip))

# saving file -------------------------------------------------------------

out <- list("metadata" = temp$metadata,
            "sites" = sites,
            "station" = station,
            "weather" = weather2)

dest <- file.path(path_may, "IDE_weather/submitted_data/sandhills_IDE_weather_clean.xlsx")

openxlsx::write.xlsx(out,dest)


