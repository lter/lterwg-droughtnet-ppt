# Script for pre-processing weather data submitted for the wupatki site, so that 
# monthly data is converted to daily so it fits into the
# processing pipeline in process_submitted_weather.R script.


# Script started 4/20/21


# packages etc ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
source("R_scripts/functions.R")


path_may <- "~/Dropbox/IDE Meeting_May2019"


# read in data ------------------------------------------------------------

p1 <- file.path(
  path_may,
  "IDE_weather/submitted_data/wupatki/IDE_weather_Wupatki_Munson_20210331.xlsx")


sheets <- c("sites", "station", "weather")
names(sheets) <- sheets
dfs1 <- map(sheets, 
            function(sheet) read_xlsx(p1, sheet = sheet, na = c("", "NA")))

# template
temp <- readRDS(file.path(path_may, "IDE_weather/IDE_weather_template.RDS"))


# weather sheet -----------------------------------------------------------

dfs2 <- dfs1
# convert to monthly to daily-

dfs1$weather <- dfs1$weather %>% 
  mutate(date = date(paste0(date, "-01")),
         month = month(date),
         year = year(date)) 

# warnings thrown aren't a concern here
dfs2$weather <- monthly2daily_precip(dfs1$weather, year = "year",
                                     month = "month", precip = "precip")

dfs2$weather$min_temp <- monthly2daily_temp(dfs1$weather, year = "year",
                                       month = "month", temp = "min_temp")

dfs2$weather$max_temp <- monthly2daily_temp(dfs1$weather, year = "year",
                                       month = "month", temp = "max_temp")
dfs2$weather$station_name <- dfs2$station$station_name

dfs2$weather$note_weather <- "temp and precip from monthly values"

# saving file -------------------------------------------------------------

out <- list("metadata" = temp$metadata,
            "sites" = dfs2$sites,
            "station" = dfs2$station,
            "weather" = dfs2$weather)

dest <- file.path(path_may, 
                  "IDE_weather/submitted_data/wupatki_weather_clean.xlsx")

openxlsx::write.xlsx(out,dest)
