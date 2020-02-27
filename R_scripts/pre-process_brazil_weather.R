# Script for pre-processing brazil submitted weather, so that it fits into the
# processing pipeline in process_submitted_weather.R script.


# Script started 2/26/20


# The purpose here is to convert monthly precip values into average daily precip 
# and make other formatting changes etc. 


# packages etc ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
source("R_scripts/functions.R")


path_may <- "E:/Dropbox/IDE Meeting_May2019"

# load data ---------------------------------------------------------------

# note all 3 xls files only have 1 sheet each
folder <- file.path(path_may, "IDE_weather\\submitted_data\\brazil")
paths <- list.files(folder)


all1 <- map(file.path(folder, paths), function(path) {
  if (str_detect(path, ".csv$")) {
    read_csv(path)
  } else {
    read_xlsx(path)
  }
})
names(all1) <- str_extract(paths, "^[[:alpha:]]+")

# template
temp_sheets <- c("metadata", "sites", "station", "weather")
temp1 <- map(temp_sheets, function(sheet) {
  read_xlsx(file.path(path_may, "IDE_weather/IDE_weather_template.xlsx"),
            sheet = sheet)
})
names(temp1) <- temp_sheets


# parse col names ---------------------------------------------------------
all2 <- all1
all2$EEA <- all1$EEA %>% 
  rename(precip = `precipitation (mm)`) %>% 
  select(year, month, precip) %>% 
  mutate(station_name = "EEA",
         note_weather = NA,
         month = month.name[month])

all2$Guaribas <- all1$Guaribas %>% 
  rename(precip = `Precipitation (mm)`,
         note_weather = Observation,
         year = Year,
         month = Month) %>% 
  select(year, month, precip, note_weather) %>% 
  mutate(station_name = "Guaribas")

all2$PNE <- all1$PNE %>% 
  rename(month = month_name,
         precip = sum_precipitation)%>% 
  select(year, month, precip) %>% 
  mutate(note_weather = NA,
         station_name = "PNE")

# monthly to daily data ---------------------------------------------------

weather1 <- all2 %>% 
  bind_rows() %>% 
  drop_na(year, month) %>%  # one site had some blank rows
  mutate(site = station_name) %>% 
  group_by(site) %>% 
  nest() %>% 
  rename(df = data) %>% 
  mutate(df = map(df, 
                  monthly2daily_precip, 
                  year = "year",
                  month = "month",
                  precip = "precip",
                  station_name = TRUE),
         # otherwise returned as factor--causes issues with unnest
         df = map(df, function(x) {
           x$station_name = as.character(x$station_name)
           as_tibble(x)
         })) %>% 
  .$df %>% 
  bind_rows()

# prepping excel file -----------------------------------------------------

# sites sheet
sites <- tibble(
  pi = NA,
  # both pne sites have same lat/lon so assuming weather goes with both
  site = c("PNE_IDE Burned", "PNE_IDE Unburned", "EEA", "Guaribas"),
  site_latitud = NA,
  site_longitud = NA
)

temp2 <- map(temp1, function(df){
  map(df, function(x) {
    rep(NA, length(x))
  }) %>% 
    as_tibble()
})


n <- length(sites$site)

station <- temp1$station %>% 
  map(function(x) {
    rep(NA, n)
  }) %>% 
  as_tibble() %>% 
  mutate(site = sites$site,
         station_name = c("PNE", "PNE", "EEA", "Guaribas"))

# adding some info from raw files
station$url[station$station_name == "EEA"] <- all1$EEA$url[1]
station$note_station[station$station_name == "EEA"] <- all1$EEA$note_station[1]


weather1 <- weather1 %>% 
  mutate(min_temp = NA,
         max_temp = NA,
         note_weather = "monthly precip values used")

weather1 %>% 
  select(station_name, date) %>% 
  duplicated() %>% 
  sum()


# saving file -------------------------------------------------------------

out <- list("metadata" = temp1$metadata,
            "sites" = sites,
            "station" = station,
            "weather" = weather1)
dest <- file.path(path_may, "IDE_weather/submitted_data/brazil_weather_clean.xlsx")

# openxlsx::write.xlsx(out,dest)
