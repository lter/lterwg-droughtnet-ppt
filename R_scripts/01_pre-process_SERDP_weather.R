# Script for pre-processing weather data submitted for three sites
# in the desert Southwest,data in the IDE_weather_SERDP (1).xlsx,
# problem is that each weather station has its own column
# so correcting that so can be input into the process_submitted_weather.R script.


# Script started 1/29/2022


# packages etc ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
source("R_scripts/functions.R")


path_may <- "~/Dropbox/IDE Meeting_May2019"


# read in data ------------------------------------------------------------

p1 <- file.path(
  path_may,
  "IDE_weather/submitted_data/SERDP/IDE_weather_SERDP (1).xlsx")

sheets <- c("sites", "station", "weather")
names(sheets) <- sheets
dfs1 <- map(sheets, 
            function(sheet) read_xlsx(p1, sheet = sheet, na = c("", "NA")))

# template
temp <- readRDS(file.path(path_may, "IDE_weather/IDE_weather_template.RDS"))


# weather sheet -----------------------------------------------------------

dfs2 <- dfs1
names(dfs1$weather)

w <- list()
w[[1]] <- dfs1$weather[, 1:6] 
w[[2]]<- dfs1$weather[, 7:12]
w[[3]] <- dfs1$weather[, 13:18]

# renaming
w2 <- map(w, function(x) {
  # removing dots and numbers from column names so kind bind rows
  names(x) <- str_replace(names(x), "\\.+\\d+", "")
  
  # so bind_rows works below
  x <- mutate(x, across(everything(), .fns = as.character))
  x
})

dfs2$weather <- bind_rows(w2) %>% 
  filter(!is.na(date)) %>% 
  # a few dates are '5 digit' dates, days since origin, 
  # but to parase downstream they can't end in .0
  mutate(date = str_replace(date, "\\.0$", "")) 

# saving file -------------------------------------------------------------

out <- list("metadata" = temp$metadata,
            "sites" = dfs2$sites,
            "station" = dfs2$station,
            "weather" = dfs2$weather)

dest <- file.path(path_may, 
                  "IDE_weather/submitted_data/SERDP_weather_clean.xlsx")

openxlsx::write.xlsx(out,dest)
