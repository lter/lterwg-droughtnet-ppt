# load and process submitted weather station data

# this script is designed to pull together the submitted weather station data 
# and for starters output a large file containing all the weather data (when possible).

# script started 9/30/19

# WORK IN PROGRESS, more checks on whether values aren't matching in joins are needed

# packages etc ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

path_may <- "E:/Dropbox/IDE Meeting_May2019"

path_oct <- "E:/Dropbox/IDE Meeting_Oct2019"


# site info ---------------------------------------------------------------

siteElev <-read.csv(file.path(path_oct, 'IDE Site Info/Site_Elev-Disturb_UPDATED_10-01-2019.csv'),
                    as.is = TRUE)

site_name_code <- siteElev %>% 
  select(site_name, site_code)

# check xlsx sheets -------------------------------------------------------

file_names <- list.files(file.path(path_may, "IDE_weather/submitted_data")) %>% 
  .[str_detect(., "\\.xlsx$")]

file_names2 <- str_replace(file_names, "\\.xlsx$", "")

file_paths <- file.path(path_may, "IDE_weather/submitted_data", file_names)
names(file_paths) <- file_names2

# note brookdale still needs to be done seperately (in its own folder)

sheets1 <- lapply(file_paths, excel_sheets)
names(sheets1) <- file_names2

sheets1
  
# load site tab ----------------------------------------------------------


sites1 <- lapply(file_paths, read_xlsx, sheet = "sites")
names(sites1) <- file_names2
# load station tab ------------------------------------------------------

stns1 <- lapply(file_paths, read_xlsx, sheet = "station")
names(stns1) <- file_names2

# load weather tab -------------------------------------------------------

safely_read_xlsx <- safely(read_xlsx) # not all files have weather tab

wthr1 <- lapply(file_paths, function(x){
  out <- safely_read_xlsx(x, sheet = "weather", na = c("NA", "", "N/A"))
  out$result
})

names(wthr1) <- file_names2

map_lgl(wthr1, is.null) # sites that don't have weather sheet

# combining multiple weather sheets for nielson sites
nielson_sheets <- sheets1$`IDE_weather_Nielsen_Australian Outback sites` %>%
  .[str_detect(., "_weather$")]

nielson_weather1 <- lapply(nielson_sheets, function(x){
  out <- read_xlsx(file_paths["IDE_weather_Nielsen_Australian Outback sites"],
                   sheet = x)
  out
})

nielson_weather2 <- bind_rows(nielson_weather1)

wthr1$`IDE_weather_Nielsen_Australian Outback sites` <- nielson_weather2


# process site info -------------------------------------------------------

# example rows left in some
sites2 <- lapply(sites1, function(x){
  
  stopifnot("pi" %in% names(x))
  
  out <- x %>% 
    filter(pi != "Doe; John")
  out
})

# process station info ---------------------------------------------------
stns1

# example rows left in some files
stns2 <- lapply(stns1, function(x){
  
  stopifnot("station_name" %in% names(x))
  
  out <- x %>% 
    filter(station_name != "example station", !is.na(station_name)) %>% 
    mutate_all(as.character) # so can bind rows later
  out
})

lapply(stns2, names)

stns2

# bringing in site code
stns3 <- lapply(stns2, function(x) {
  out <- left_join(x, site_name_code, by = c("site" = "site_name"))
  out
})


stns4 <- bind_rows(stns3) 

# sites where site name doesn't have match with siteElev file
stns4 %>%  filter(is.na(site_code)) %>% 
  select(site, site_code)

# Hongyuan doesn't have site code (b/ not biomass data as of yet)

# adding in missing codes
stns4[stns4$site == "Syferkuil South Africa", ]$site_code <- "syferkuil.za"
stns4[stns4$site == "Mar Chiquita", ]$site_code <- "marcdrt.ar"


# process weather tables ---------------------------------------------------


# rhijnauwen ~~~~~~~~~

# only first couple dates provided, they said they were sequential
first_date <- wthr1$IDE_weather_rhijnauwen$date[1]
n <- length(wthr1$IDE_weather_rhijnauwen$precip)
dates <- seq(from = lubridate::ymd(first_date), length.out = n,
             by = "day") %>% 
  as.character()

wthr1$IDE_weather_rhijnauwen$date <- dates

# ~~~~~~~

wthr1 <- lapply(wthr1, function(x){
  x %>% 
    filter(station_name != "example station") # getting rid of example row
}) 

# check that date formats look ok
map_chr(wthr1, function(x){
  x$date[1] %>% 
    as.character
})

# parse dates
wthr2 <- lapply(wthr1, function(x){
  x <- x %>% 
    mutate(date = as.character(date),
           date = parse_date(date))
  print(x[1, ])
  return(x)
})

# parse numeric cols
wthr3 <-lapply(wthr2, function(x){

  x <- x %>% 
    mutate_at(vars(precip, min_temp, max_temp),
           .funs = as.numeric)
  x
}) 

# add in station/site info
wthr4 <- lapply(wthr3, function(x){
  left_join(x, stns4, by = "station_name")
})

# check join was ok
lapply(wthr4, function(x) x[1, ]) %>% 
  bind_rows()

wthr5 <- bind_rows(wthr4)
dim(wthr5)


# checks ------------------------------------------------------------------

wthr5$station_name %>% unique()
wthr5$site_code %>% unique()


# save file ---------------------------------------------------------------

# daily weather for all submitted stations [not yet including a couple sites that need extra processing]

# write_csv(wthr5,
#           file.path(path_oct, 'data/precip/submitted_daily_weather_2019-10-01.csv'))
