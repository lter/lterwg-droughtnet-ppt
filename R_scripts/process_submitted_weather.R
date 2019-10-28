# load and process submitted weather station data

# this script is designed to pull together the submitted weather station data 
# and for starters output a large file containing all the weather data (when possible).

# script started 9/30/19

# WORK IN PROGRESS

# once finished this script is meant to:

# 1 Load each spreadsheet
# 2a Check whether appropriate sheets (site, station, weather) are present
# 2b Fix if not all sheets present
# 3a check if proper columns in site sheet are present
# 3b fix so all have appropriate columns
# 3c check content of site sheet
# 4a check if proper columns in station sheet are present
# 4b fix so all have appropriate columns
# 4c check content of station sheet (e.g. fix lat/lon))
# 5a check if proper columns in weather sheet are present
# 5b fix so all have appropriate columns
# 5c preliminary data check--column classes are correct, dates parsed, example data from template removed
#     solve specific known issues for a given site (NAs will be filled in a subsequent script)
# 6 combine into one master file
# 7 added in site codes
# 8 save file

# packages etc ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

source("R_scripts/functions.R")

path_may <- "E:/Dropbox/IDE Meeting_May2019"

path_oct <- "E:/Dropbox/IDE Meeting_Oct2019"


# site info ---------------------------------------------------------------

siteElev <-read.csv(file.path(path_oct, 'IDE Site Info/Site_Elev-Disturb_UPDATED_10-01-2019.csv'),
                    as.is = TRUE)

site_name_code <- siteElev %>% 
  select(site_name, site_code)

# check xlsx sheets -------------------------------------------------------

all_names <- list.files(file.path(path_may, "IDE_weather/submitted_data"))

file_names <- all_names %>% 
  .[str_detect(., "\\.xlsx$")] %>% # excluding sub-folders here
  str_replace("^~\\$", "") %>% # dealing with temporary ~$filename for opened files
  .[!duplicated(.)]

# file_names[file_names != "IDE_weather_rhijnauwen.xlsx"] # PW seperately cleaning this one

# what files/folders aren't we loading here?
all_names[!all_names %in% file_names]
# note brookdale and monte_oriental need to be done seperately


file_names2 <- str_replace(file_names, "\\.xlsx$", "") 
file_names2

# shortening file names for ease of use [for later subsetting]
file_names3 <- 
  file_names2 %>% 
  str_replace_all("weather|template*|IDE", "") %>% 
  str_replace_all("(^_+)|(^\\s+)|(_+$)|(\\s+$)", "") %>% # leading/trailing
  str_replace_all("\\s+|[-,)(]", "_") %>% 
  str_replace_all("_{2,}", "_")
file_names3  

file_paths <- file.path(path_may, "IDE_weather/submitted_data", file_names)
names(file_paths) <- file_names3


sheets1 <- lapply(file_paths, excel_sheets)
sheets1


# load all sheets ---------------------------------------------------------

# the idea here is to create a list where each list element is one spreadsheet
# those lists then have sub lists where each sub list is a sheet of the spreadsheet

# outer 'loop' each file_path and associated sheet names
all1 <- map2(file_paths, sheets1, function(path, sheets_of_path){
  # inner 'loop' load in each sheet of that file
  out <- map(sheets_of_path, function(sheet){ 
    read_xlsx(path = path, sheet = sheet, na = c("", "NA")) # not parsing all dates correctly
  })
  names(out) <- sheets_of_path
  out
})

all2 <- all1

# files with anomolous sheets ---------------------------------------------

# check for anomolous sites that don't have the usual 4 sheets 
not_anom <- map_dbl(all1, check_names, # check_names is fun in functions.R
                    names = "metadata,sites,station,weather")
anom_sheets1 <- sheets1[!not_anom] # files with anomolous tables 
names(anom_sheets1)


# Nielsen ~~~~~~~~~~~~~~~~~~~~~~

# combining multiple weather sheets for nielson sites
nielson_sheets <- sheets1$Nielsen_Australian_Outback_sites %>%
  .[str_detect(., "_weather$")]

all2$Nielsen_Australian_Outback_sites$weather <- 
  all1$Nielsen_Australian_Outback_sites[nielson_sheets] %>% 
  bind_rows()

all2$Nielsen_Australian_Outback_sites[nielson_sheets] <- NULL

names(all2$Nielsen_Australian_Outback_sites)

# OR_Byrne ~~~~~~~~~~~~~~~~~~~~

anom_sheets1$OR_Byrne # no problem (just no metadata tab)
all2$OR_Byrne$metadata <- NA

# SonoraAgrilifeResearchStation ~~~~~~

anom_sheets1$SonoraAgrilifeResearchStation # no problem (just no metadata tab)
all2$SonoraAgrilifeResearchStation$metadata <- NA

# wayqecha_met_summary ~~~~~~~~~~~~

anom_sheets1$wayqecha_met_summary

# wide format data, needs to be parsed
map(all1$wayqecha_met_summary, head)

head(all1$wayqecha_met_summary$`daily rainfall`)

# this data is from 5 years prior to experiment so not using here. 
# [note: code to parse this data can be found in older commit (mid Oct 2019) of this file]
all2$wayqecha_met_summary <- NULL 

anom_sheets1


# check if all sheets present -----------------------------------------------

all2_sheets_check <- map_lgl(all2, check_names, 
                             names = "metadata,sites,station,weather") 

if (!all(all2_sheets_check)) warning("Not all list elements have correct tables")

##########################################################################
################ process sites tables #####################################

all3 <- all2

# map(all3, function(x) head(x$sites))

# fix sites column names ---------------------------------------------------

site_col_names <- map(all2, function(x) names(x$sites))

# files were sites table don't have usual col names ~~~
which_sites_anom <- map_lgl(all2, function(x) {
  # do columns match these names?
  check_names(x$sites, names = "pi,site,site_latitud,site_longitud") 
}) %>% 
  .[.== FALSE]

anom_sites <- site_col_names[names(which_sites_anom)] # column names of those sites
anom_sites

# Syferkuil_South_Africa ~~~~
# see remark
all3$Syferkuil_South_Africa$sites$Remarks

all3$Syferkuil_South_Africa$sites$Remarks <- NULL

# SonoraAgrilifeResearchStation ~~~
all3$SonoraAgrilifeResearchStation$sites

all3$SonoraAgrilifeResearchStation$sites <- 
  all2$SonoraAgrilifeResearchStation$sites %>% 
  select(pi, site, latitude, longitude) %>% 
  rename(site_latitud = latitude, site_longitud = longitude)

# ethabuka ~~~
notes <- all3$ethabuka_2014_2019$sites$..5 
notes

# adding note to station sheet instead
all3$ethabuka_2014_2019$station$note_station
all3$ethabuka_2014_2019$station$note_station <- 
  paste(all3$ethabuka_2014_2019$station$note_station, notes[1:2], sep = ". ")

all3$ethabuka_2014_2019$sites$..5 <- NULL
all3$ethabuka_2014_2019$sites <- all3$ethabuka_2014_2019$sites %>% 
  filter(complete.cases(.)) # removing 1 row of NAs

# check if all col names fixed
check_names_in_list(list = all3, element_name = "sites",
                    names = "pi,site,site_latitud,site_longitud",
                    warning = "Not all sites tables have the correct column names")


# check sites table contents -----------------------------------------------

# map(all3, function(x) head(x$sites))

all3 <- map(all3, function(x){
  x$sites <- x$sites[x$sites$site != "Nowhere", ] # removing example row where necessary
  x
})

# all have non NA rows?
at_least_1row <- map_lgl(all3, function(x){
  nrow(x$sites[!is.na(x$sites$site), ]) >=1
})

if(!all(at_least_1row)) warning("some files have no site information")

# convert all columns to character--so can
all3 <- map(all3, function(x){
  x$sites <- mutate_all(x$sites, as.character)
  x
})

# bind_rows(map(all3, function(x) x$sites)) %>% View()

##########################################################################
################ process station tables ##################################

all4 <- all3

# check/fix station table col names -----------------------------------------

station_col_names <- map(all3, function(x) names(x$station))

station_cols <- "distance,elev,note_station,site,source,station_id,station_latitud,station_longitud,station_name,url"
# files were station tables don't have usual col names ~~~
station_names_good <- map_lgl(all3, function(x) {
  # do columns match these names?
  check_names(x$station, names = station_cols) 
}) 

anom_col_names <- station_col_names[!station_names_good]

# SonoraAgrilifeResearchStation ~~~~~
all4$SonoraAgrilifeResearchStation$station

stn_col_names <- names(all4$hardware_ranch$station)
all4$SonoraAgrilifeResearchStation$station <- 
  all4$SonoraAgrilifeResearchStation$station %>% 
  rename(source = provider, 
         station_id = `Station ID`, 
         station_latitud = latitude, 
         station_longitud = longitude,
         url = link) %>% 
  mutate(elev = `elevation (ft)`*0.3048, # convert to m
         distance = NA, # not provided
         site = all4$SonoraAgrilifeResearchStation$sites$site,
         station_name = station_id, # station name not given so using this
         note_station = NA) %>% 
  select(stn_col_names) # just keeping normal columns

# check if all col names fixed
check_names_in_list(
  list = all4,  element_name = "station", names = station_cols,
  warning = "Not all station tables have the correct column names"
)


# check/fix station sheet contents ----------------------------------------
all5 <- all4

# convert all cols to characte4
all5 <- map(all5, function(x){
  x$station <- mutate_all(x$station, as.character)
  x
})

# removing rows of all NAs from station
all5 <- map(all5, function(x) {
  num_missing <- rowSums(is.na(x$station))
  x$station <- x$station[num_missing != ncol(x$station), ]
  x
})

# adding the list element name as a column--for when binding together
all5 <- map2(all5, names(all5), function(x, name){
  x$station$file_name <- name
  x
})

# files where some rows of station sheet contain no station_name/site name
station_rows_complete <- map_lgl(all5, function(x){
  good_row <- rowSums(!is.na(x$station[, c("site", "station_name")])) == 2
  all(good_row)
})

names(all5[!station_rows_complete])

# viewing the data
map(all5[!station_rows_complete], function(x) x$station)

# filling in blank site name
all5$Ciempozuelos$station$site[2] <- all5$Ciempozuelos$station$site[1]

# Syferkuil--notes were on multiple lines
syf_stn <- all5$Syferkuil_South_Africa$station
syf_stn$note_station[1] <- 
  paste(syf_stn$note_station, collapse = " ")

all5$Syferkuil_South_Africa$station <- 
  syf_stn %>% 
  filter(!is.na(station_name))

# kranzberg--notes on multiple lines
kranz_stn <- all5$Kranzberg$station
kranz_stn$note_station[1] <- 
  paste(kranz_stn$note_station, collapse = " ")

all5$Kranzberg$station <- 
  kranz_stn %>% 
  filter(!is.na(station_name))

# check--all rows now have site/station name

if(!all(
  map_lgl(all5, function(x){
    good_row <- rowSums(!is.na(x$station[, c("site", "station_name")])) == 2
    all(good_row)
  })
)) {
  warning("site or station name still missing from some rows")
}


# parsing station lat/lon -------------------------------------------------

# adding E/W labels
all5$Potrok_Aike_Patagonia_Peri_Toledo_$station <- 
  all5$Potrok_Aike_Patagonia_Peri_Toledo_$station %>% 
  mutate(station_longitud = paste(station_longitud, "W"),
         station_latitud = paste(station_latitud, "S"))

# all station data now in 1 df
stn1 <- bind_rows(map(all5, function(x) x$station))
stn1
stn1$station_latitud
stn1$station_longitud
stn2 <- stn1

# look at ones not in decimal degrees:
stn1$station_latitud[str_detect(stn1$station_latitud, "[°ºA-z]")]
stn1$station_longitud[str_detect(stn1$station_longitud, "[°ºA-z]")]

biogeo::dmsparsefmt("107°44`00.007W", "ddd°mm`ssL") 
stn2 <- stn2 %>% 
  mutate(
    lat = station_latitud,
    lon = station_longitud,
    lat = str_replace(lat, "'", "`"), # making one second demarcator
    lon = str_replace(lon, "'", "`"),
    lat = str_replace(lat, "º", "°"),
    lon = str_replace(lon, "º", "°"),
    lat = str_replace_all(lat, "[\\s\"\\)\\(]", ""),
    lon = str_replace_all(lon, "[\\s\"\\)\\(]", ""),
    lon = str_replace_all(lon, "[\\s\"\\)\\(]|(°$)", ""), # just dd but adding deg symbol
    # when dd but N/E still used, getting rid of the letter (ie when no degree symbol:
    lat = str_replace(lat, "(?<!°.{0,10})N", ""), 
    lon = str_replace(lon, "(?<!°.{0,10})E", ""), 
    deg_lat = str_extract(lat, "^-*\\d+(?=°)"), # numbers before degree symbol
    min_lat = str_extract(lat, "(?<=°)\\d+"),
    sec_lat = str_extract(lat, "(?<=`)\\d+\\.*\\d*"),
    deg_lon = str_extract(lon, "^-*\\d+(?=°)"), # numbers before degree symbol
    min_lon = str_extract(lon, "(?<=°)\\d+"),
    sec_lon = str_extract(lon, "(?<=`)\\d+\\.*\\d*"),
    lat_ns = str_extract(lat, "[NSns]"),
    lon_ew = str_extract(lon, "[EeWw]"),
    is_lat_dms = str_detect(lat, "°"),
    is_lon_dms = str_detect(lon, "°"),
  ) 

stn3 <- stn2 %>% 
  mutate_at(vars(matches("sec_|min_|deg_")),
             .funs = as.numeric) %>% 
  mutate(
    station_latitud = ifelse(is_lat_dms,
                             biogeo::dms2dd(deg_lat, min_lat, sec_lat, lat_ns),
                             lat),
    station_longitud = ifelse(is_lon_dms,
                              biogeo::dms2dd(deg_lon, min_lon, sec_lon, lon_ew),
                              lon),
  ) %>% 
  mutate_at(vars(station_latitud, station_longitud),
            .funs = as.numeric) %>% 
  select(names(all5$hardware_ranch$station)) # just keeping the main cols

# CHECK: shouldn't be any NAs in lat/lon if parsed correctly
stn3 %>% 
  filter(is.na(station_longitud) | is.na(station_latitud) | is.na(site) 
         | is.na(station_name))


# parsing distance/elv -------------------------------------------------------

stn3$distance # examine for characters e.g (miles etc)
stn4$elev

# redo if throws as.numeric parsing error
stn4 <- stn3 %>% 
  mutate(distance = str_replace(distance, "km", ""),
         is_dist_m = str_detect(distance, "(?<![A-z])m(?![A-z])"), # just solitary m for meter
         distance = str_replace(distance, "(?<![A-z])m(?![A-z])", ""),
         distance = as.numeric(distance), 
         distance = ifelse(is_dist_m,
                           distance/1000, # m to km
                           distance),
         elev = str_replace(elev, "(?<![A-z])m(?![A-z])", ""), # ok to just remove m
         elev = as.numeric(elev)
         ) 

# check--all rows now have site/station name



# START HERE














# process tab ----------------------------------------------------------


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
