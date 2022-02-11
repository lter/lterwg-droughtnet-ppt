# load and process submitted weather station data

# this script is designed to pull together the submitted weather station data 
# and for starters output a large file containing all the weather data (when possible).

# script started 9/30/19

#  this script is meant to:

# 1 Load each spreadsheet
# 2a Check whether appropriate sheets (site, station, weather) are present
# 2b Fix if not all sheets present
# 3a check if proper columns in site sheet are present
# 3b fix so all have appropriate columns
# 3c check content of site sheet
# 4a check if proper columns in station sheet are present
# 4b fix so all have appropriate columns
# 4c check/fix content of station sheet (e.g. parse lat/lon))
# 5a check if proper columns in weather sheet are present
# 5b fix so all have appropriate columns
# 5c preliminary data check--column classes are correct, dates parsed, example data from template removed
#     solve specific known issues for a given site (NAs will be filled in a subsequent script)
# 6 combine into one master file
# 7 added in site codes
# 8 save file of all the submitted data

# packages etc ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)

source("R_scripts/functions.R")

path_may <- "~/Dropbox/IDE Meeting_May2019"

path_oct <- "~/Dropbox/IDE Meeting_Oct2019"
path_ms <-  "~/Dropbox/IDE MS_Single year extreme"

# site info ---------------------------------------------------------------


siteElev_path <- file.path(path_ms, "Data/Site_Elev-Disturb.csv")
siteElev <-read.csv(siteElev_path,
                    as.is = TRUE)

site_name_code <- siteElev %>% 
  select(site_name, site_code)

# survey results
sites_wthrdata1 <- read_xlsx(file.path(path_may, 
                    "IDE_weather/Sites_WeatherStationData_11-26-2019.xlsx"))

sites_wthrdata2 <- sites_wthrdata1 %>% 
  rename(site_name = `What is your site name?`) %>% 
  select(site_name, site_code)

# check xlsx sheets -------------------------------------------------------

all_names <- list.files(file.path(path_may, "IDE_weather/submitted_data"))

file_names <- all_names %>% 
  .[str_detect(., "\\.xlsx$")] %>% # excluding sub-folders here
  str_replace("^~\\$", "") %>% # dealing with temporary ~$filename for opened files
  .[!duplicated(.)]

# IDE_weather_rhijnauwen.xlsx cleaned here so not using processed file
file_names <- file_names[!file_names %in% "IDE_weather_rhijnauwen processed.xlsx"]

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
  str_replace_all("_{2,}", "_") %>% 
  str_replace_all("^_", "")
file_names3  

# shorter name
file_names3[file_names3 == "ECN_Wytham_DAILY_met_data_2015_2019_for_DroughtNet"] <- "ECN_Wytham"
file_names3[file_names3 == "Climate_data_2014_15_and_2015_16_rainfall_season_Matta_LTER"] <- "Matta_LTER"
# new data sheet provided but renaming so older code doesn't break:
file_names3[file_names3 == "PassoGavia_2021"] <- "PassoGavia"

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
    read_xlsx(path = path, sheet = sheet, 
              na = c("", "NA", "-", "Missing data, not registered", "ND")) 
  })
  names(out) <- sheets_of_path
  out
})

all2 <- all1

# load seperately prepped GCN data:
# (created in pre-process_GCN_weather.R script)

gcn1 <- readRDS(
  file.path(path_may, 
            "IDE_weather/submitted_data/GCN/GCN-weather-cleaned_2019-12-02.rds")
)

names(gcn1) 

all2 <- c(all2, gcn1)

if(sum(duplicated(names(all2))) != 0) warning("duplicate names")

# files with anomolous sheets ---------------------------------------------

# check for anomolous sites that don't have the usual 4 sheets 
not_anom <- map_dbl(all2, check_names, # check_names is fun in functions.R
                    names = "metadata,sites,station,weather")
anom_sheets1 <- sheets1[!not_anom] # files with anomolous tables 
names(anom_sheets1)

wthr_col_names <- c("station_name", "date", "precip", "min_temp", "max_temp", 
                    "note_weather")

# make a empty template
template <- map(all2$hardware_ranch, function(x) {
  x[1, ] %>% 
    mutate_all(function(x) NA) # fill with NAs
})
template$metadata <- all2$hardware_ranch$metadata

# Nielsen ~~~~~~~~~~~~~~~~~~~~~~

# newer file (dec 5) submitted, with corrected data for charlevill, so using that
all1$Nielsen_Australian_Outback_sites <- 
  all1$Nielsen_Australian_Outback_sites_Dec_5_2019

all2$Nielsen_Australian_Outback_sites_Dec_5_2019 <- NULL

# combining multiple weather sheets for nielson sites
nielson_sheets <- sheets1$Nielsen_Australian_Outback_sites %>%
  .[str_detect(., "_weather$")]

# wrong station_name repeated here by mistake
all1$Nielsen_Australian_Outback_sites$Charleville_weather$station_name <- "Charleville" 

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

# GCN_Suihua 
# temp data is daily mean

all2$GCN_Suihua$weather <- bind_rows(all2$GCN_Suihua$weather2018,
                                     all2$GCN_Suihua$weather2019) %>% 
  rename(precip = `precip(mm)`,
         mean_temp = "temp(℃)") %>% 
  select(-"RH(%)")

# many "temp C" values of 100.0--so assuming this data is corrupted somehow
# hist(as.numeric(all2$GCN_Suihua$weather$mean_temp))

all2$GCN_Suihua$weather$mean_temp <- NA
all2$GCN_Suihua$weather$note_weather <- "mean temp data suspect so discarded in processing (MH)"
all2$GCN_Suihua$weather2018 <- NULL
all2$GCN_Suihua$weather2019 <- NULL

# ECN_Wytham ~~~
# weather sheet
ecn_weather <- all2$ECN_Wytham$`2015-2019_calculated_DAILY`[-(1:5),]

ecn_weather2 <- ecn_weather[-1, ]
names(ecn_weather2) <- as.character(ecn_weather[1, ]) %>% 
  str_replace_all("[^A-z]", "")

all2$ECN_Wytham$weather <- ecn_weather2 %>% 
  mutate(first = paste0(Year, "-01-01")) %>% # first day of year
  group_by(first) %>% 
  # converting DOY to date
  mutate(date = as.Date(as.numeric(DayofYear) -1, origin = unique(first)),
         station_name = "ECN Automated Weather Station on Upper Seeds") %>% 
  ungroup() %>% 
  rename(precip = `SumofRainfalltotal[mm]`,
         max_temp = `MaxofAirtempmax[C]`,
         min_temp = `MinofAirtempminimum[C]`,
         mean_temp = `AverageofAirtempAv[C]`,
         note_weather = DataQualityComment) %>% 
  select(all_of(wthr_col_names))

# station sheet
all2$ECN_Wytham$metadata <- template$metadata

# no info and distance/elev provided
all2$ECN_Wytham$station <- template$station %>% 
  mutate(site = "Wytham_RainDrop",
         station_name = "ECN Automated Weather Station on Upper Seeds", 
         station_latitud = 51.7706267,
         station_longitud = -1.3324990,
         source = "Environmental Change Network site at Wytham",
         note_station = paste("must cite, see citation agreement. Environmental", 
                              "Change Network at Wytham (site run by Stefanie Schäfer",
                              "and Denise Pallett from the Centre for Ecology & Hyrdology).")
         )

all2$ECN_Wytham$sites <- template$sites %>% 
  mutate(site = "Wytham_RainDrop",
         pi = "Andrew Hector")

all2$ECN_Wytham$Sheet5 <- NULL
all2$ECN_Wytham$`2015-2019_calculated_DAILY` <- NULL

# purdue ~~~

pur_wthr1 <- all2$Purdue_daily_data_for$`Purdue - Daily Weather Data`
pur_stn1 <- all2$Purdue_daily_data_for$Metadata
names(pur_stn1) <- c("name", "value")
all2$Purdue_daily_data_for$weather <- pur_wthr1 %>% 
  select(wthr_col_names)

all2$Purdue_daily_data_for$station <- template$station %>% 
  mutate(
    site = pur_wthr1$site_name[1],
    station_name = pur_wthr1$station_name[1],
    station_latitud = pur_stn1$value[pur_stn1$name == "Latitude"],
    station_longitud = pur_stn1$value[pur_stn1$name == "Longitude"],
    source = pur_stn1$name[1])

all2$Purdue_daily_data_for$sites <- template$sites %>% 
  mutate(site = pur_wthr1$site_name[1])

all2$Purdue_daily_data_for$metadata <- template$metadata

all2$Purdue_daily_data_for$Metadata <- NULL
all2$Purdue_daily_data_for$`Purdue - Daily Weather Data` <- NULL

# matta ~~~
all2$Matta_LTER <- template
all2$Matta_LTER$weather <- map(all1$Matta_LTER,
              function(x) {
                cols <- x[1, ] # first row is col names
                df <- x[-1, ]
                names(df) <- as.vector(unlist(cols))
                df2 <- df %>% 
                  rename(date = Date,
                         precip = `Rain mm`,
                         mean_temp = `Air Temp Daily Mean Mean`,
                         min_temp = `Air Temp Daily Mean Min`,
                         max_temp = `Air Temp Daily Mean Max`
                         ) %>% 
                  mutate(note_weather = NA,
                         # no station name given-so using site name
                         station_name = "Matta LTER")
                df2
              }) %>% 
  bind_rows()

all2$Matta_LTER$station <- all2$Matta_LTER$station %>% 
  mutate(site = "Matta LTER",
         station_name = "Matta LTER")

all2$Matta_LTER$sites <- all2$Matta_LTER$sites %>% 
  mutate(site = "Matta LTER")

# norway ~~

# weird dates with extra 0 in middle of month
# using lookbehind/look ahead to convert dates eg "2017-101-1" to "2017-11-1"
all2$Norway_2019$weather <- all1$Norway_2019$weather %>% 
  mutate(date = str_replace(date, "(?<=^\\d{4}-\\d)0(?=\\d-\\d{1,2}$)", ""),
         date = ymd(date))

# precip same between the two sheets, but ark1 has some impossible dates (eg nov 40th)
map2_lgl(all1$Norway_2019$Ark1, all1$Norway_2019$weather, function(x, y) {
  x <- x[!is.na(x)]
  y <- y[!is.na(y)]
  all(x==y)
})

all2$Norway_2019$Ark1 <- NULL



# check if all sheets present -----------------------------------------------

all2_sheets_check <- map_lgl(all2, check_names, 
                             names = "metadata,sites,station,weather") 
all2_sheets_check[!all2_sheets_check]

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
notes <- all3$ethabuka_2014_2019$sites[["...5"]]
notes

# adding note to station sheet instead
all3$ethabuka_2014_2019$station$note_station
all3$ethabuka_2014_2019$station$note_station <- 
  paste(all3$ethabuka_2014_2019$station$note_station, notes[1:2], sep = ". ")

all3$ethabuka_2014_2019$sites$...5 <- NULL
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

# sanpablo ~~~~~~~
# site name not entered in station table
all3$sanpablovaldes$sites
all3$sanpablovaldes$sites$site <- "San Pablo Valdes Drt"
all3$sanpablovaldes$station$site <- "San Pablo Valdes Drt"


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
  select(all_of(stn_col_names)) # just keeping normal columns

#GCN_Suihua

all4$GCN_Suihua$station <- all4$GCN_Suihua$station %>% 
  mutate(distance = `distance(m)`/1000) %>% 
  rename(elev = `elev(m)`) %>% 
  select(-`distance(m)`)


# check if all col names fixed
check_names_in_list(
  list = all4,  element_name = "station", names = station_cols,
  warning = "Not all station tables have the correct column names"
)

# check/fix station sheet contents ----------------------------------------
all5 <- all4

# convert all cols to character and remove example rows
all5 <- map(all5, function(x){
  x$station <- mutate_all(x$station, as.character) %>% 
    filter((is.na(site) |site != "Nowhere")  & 
             (is.na(station_name) | station_name != "example station"))
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
# map(all5[!station_rows_complete], function(x) x$station)

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

# bamboo drought ~~~
all5$Bamboo_drought_$sites$site <- 	"Bamboo drought China"
all5$Bamboo_drought_$station$site <- 	"Bamboo drought China"

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
stn1 <- extract_elements_2df(all5, "station")
stn1
stn1$station_latitud
stn1$station_longitud
stn2 <- stn1

# look at ones not in decimal degrees:
stn1$station_latitud[str_detect(stn1$station_latitud, "[°ºA-z]")]
stn1$station_longitud[str_detect(stn1$station_longitud, "[°ºA-z]")]

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
  select(all_of(stn_col_names), "file_name") # just keeping the main cols

# CHECK: shouldn't be any NAs in lat/lon if parsed correctly--other than the 8 GCN sites
stn3 %>% 
  filter(is.na(station_longitud) | is.na(station_latitud) | is.na(site) 
         | is.na(station_name))


# parsing distance/elv -------------------------------------------------------

stn3$distance # examine for characters e.g (miles etc)
stn3$elev

# redo if throws as.numeric parsing error
stn4 <- stn3 %>% 
  mutate(distance = str_replace(distance, "km", ""),
         distance = str_replace(distance, ",", "."),
         is_dist_m = str_detect(distance, "(?<![A-z])m(?![A-z])"), # just solitary m for meter
         distance = str_replace(distance, "(?<![A-z])m(?![A-z])", ""),
         distance = as.numeric(distance), 
         distance = ifelse(is_dist_m,
                           distance/1000, # m to km
                           distance),
         elev = str_replace(elev, "(?<![A-z])m(?![A-z])", ""), # ok to just remove m
         elev = str_replace(elev, "^138-176$", "157"), # just picking middle value
         elev = as.numeric(elev)
         ) %>% 
  select(-is_dist_m)

# now putting stn table back into the lists

all5 <- map2(all5, names(all5), function(x, name) {
  x$station <- stn4[stn4$file_name == name, ]
  x
})

# checking site names

# checking if all sites listed in the sites table are also in the station tables

# name incorrect in sites sheet (but coordinates correct):
all5$Pineta2014_2019$sites$site <- all5$Pineta2014_2019$station$site

# just written differently:
all5$Freiburg$sites$site <- all5$Freiburg$station$site

# from coords can see name mix up
all5$sev_blue_5_20$sites$site <- "sev_blue"
all5$sev_blue_5_20$station$site <- "sev_blue"

# from coordinates and other naming it is clear there was just a mix up in names
all5$Garraf_daylydata$station$site <- all5$Garraf_daylydata$sites$site

# norway --typo
all5$Norway_2019$sites$site[all5$Norway_2019$sites$site == "Haverøyaerøya"] <- "Haverøya"

site_name_present <- map_lgl(all5, function(x) {
  site_in_station <- x$sites$site %in% x$station$site
  station_in_site <- x$station$site %in% x$sites$site
  all(c(site_in_station, station_in_site))
})

site_name_present[!site_name_present]

if(!all(site_name_present)) warning("fix site name issues")

###########################################################################
############     Process Weather data   ###################################


# checking weather col names ----------------------------------------------

wthr_col_string <- "date,max_temp,min_temp,note_weather,precip,station_name"

wthr_names_good <- map_lgl(all5, function(x) {
  # do columns match these names?
  check_names(x$weather, names = wthr_col_string) 
}) 

# files that need fixing
names(all5[!wthr_names_good])


map(all5[!wthr_names_good], function(x) names(x$weather))


# fixing weather col names ------------------------------------------------

all6 <- all5

# some missing note column so adding--to make later checks easier
all6 <- map2(all6, names(all6), function(x, name){
  if (!"note_weather" %in% names(x$weather)) {
    x$weather[["note_weather"]] <-  NA
    message(paste("note_weather column added to:", name, "\n"))
  }
  x
})

# creating date columns
head(all6$Pineta2014_2019$weather)

all6$Pineta2014_2019$weather <- all6$Pineta2014_2019$weather %>% 
  mutate(date = ymd(paste(Year, month, day, sep = "-"))) %>% 
  select(wthr_col_names)

# head(all6$Torla_Ordesa2014_2019$weather)
all6$Torla_Ordesa2014_2019$weather <- all6$Torla_Ordesa2014_2019$weather %>% 
  mutate(date = ymd(paste(Year, month, day, sep = "-"))) %>% 
  select(wthr_col_names)

all6$cap_whitetank_4_20$weather <- all6$cap_whitetank_4_20$weather %>% 
  rename(precip = PRCP)

# Syferkuil --combining 2 note columns
all6$Syferkuil_South_Africa$weather$note_weather %>% unique()
all6$Syferkuil_South_Africa$weather$Note_treatments %>% unique()

all6$Syferkuil_South_Africa$weather <- 
  all6$Syferkuil_South_Africa$weather %>% 
  mutate(note_weather = paste(note_weather, Note_treatments, sep = ". ")) %>% 
  select(-Note_treatments) 

# potrok
all6$Potrok_Aike_Patagonia_Peri_Toledo_$weather <- 
  all6$Potrok_Aike_Patagonia_Peri_Toledo_$weather %>% 
  rename(date = DATAS,
         precip = Rainfall,
         min_temp = `Mín. Temp`,
         max_temp = `Máx. Temp`,
         mean_temp = `Temp Mean`) # keeping provided mean temp data for now

# IMGERS
all6$IMGERS$weather$...7 <- NULL

# OR
all6$OR_Byrne$weather <- all6$OR_Byrne$weather[, wthr_col_names]

# PassoGavia ~~~
# using on site station whe data present otherwise using off site station
# the on site station only has summer data.(but the off site station also has 
# some missing values)

passo_primary <- all6$PassoGavia$weather %>% 
  rename(date = `date...2`) %>% 
  select(wthr_col_names) %>% 
  mutate(mean_temp = NA)
  

all6$PassoGavia$station$note_station
passo_secondary <- all6$PassoGavia$weather %>% 
  select(note_weather:max_temp2) %>% 
  rename(station_name = station_name2,
         date = date...8,
         precip = precip_2,
         min_temp = min_temp2,
         max_temp = max_temp2) %>% 
  mutate(note_weather = NA) %>% 
  select(wthr_col_names) %>% 
  mutate(mean_temp = NA)

all6$PassoGavia$weather <- comb_primary_secondary_stns(passo_primary, 
                                                       passo_secondary)

# sonora ~~~~~~~~~~~~~~~~~~~~~~~
# fix col names and data:

# sub daily temp (F) and accumulated precip (in)
son_wthr1 <- all6$SonoraAgrilifeResearchStation$weather %>% 
  .[-1, ] # first row isn't data

#son_wthr1 
  
accum <- son_wthr1$precip_accum_set_1 %>% 
  as.numeric()
sum(is.na(accum)) # not many NAs so treating them as 0 rain

# converting accumulated rain (year to date) to rain at that time
previous <- accum[1]
precip <- rep(NA, length(accum))
for(i in 1:length(accum)) {
  precip[i] <- if(accum[i] == 0 | is.na(accum[i])) {
    0
  } else {
    accum[i] - previous
  }
  # amount accumulated precip of previous period (for next loop iteration)
  previous <- if (is.na(accum[i])) {
    previous
  } else {
    accum[i]
  }
}
precip_in_calc <- precip
all6$SonoraAgrilifeResearchStation$weather  <- son_wthr1 %>%
  mutate(precip_in = precip_in_calc,
         precip = precip_in*25.4, # in to mm
         Date_Time = mdy_hm(Date_Time),
         date = as.Date(Date_Time),
         station_name = Station_ID,# using ID for name
         air_temp_set_1 = as.numeric(air_temp_set_1),
         temp = (32 * air_temp_set_1 - 32) * 5/9  # F to C
         ) %>% 
  group_by(date, station_name) %>% 
  summarise(precip = sum(precip, na.rm = TRUE),  # converting sub daily to daily
            min_temp = min(temp, na.rm = TRUE),
            max_temp = max(temp, na.rm = TRUE),
            note_weather = NA, 
            .groups = "drop")

# cusack_panama (monthly data)

pan_wthr1 <- all6$Cusack_Panama$weather
names(pan_wthr1)
pan_wthr1$month %>% unique() #years
pan_wthr1$Year %>% unique()
pan_wthr1$note_weather %>% unique()
pan_wthr2 <- pan_wthr1 %>% 
  rename(month = Year, year = month) %>% 
  filter_all(any_vars(!is.na(.))) # two rows contained all NAs
pan_wthr3 <- monthly2daily_precip(pan_wthr2, "year", "month", "precip",
                                  station_name = TRUE)

# daily data taken as monthly mean value
# I think warnings here are ok (duplicated dates, because multiple stations)
pan_wthr3$min_temp  <- monthly2daily_temp(pan_wthr2, "year", "month", "min_temp")
pan_wthr3$max_temp  <- monthly2daily_temp(pan_wthr2, "year", "month", "max_temp")
pan_wthr3$note_weather <- "Values of temp and precip taken from Monthly values"

all6$Cusack_Panama$weather <- pan_wthr3

# yarramundi

all6$Yarramundi_on_site_met_data_2013_2019$weather <- 
  all6$Yarramundi_on_site_met_data_2013_2019$weather %>% 
  rename(date = Date,
         precip = Rain_mm_Tot,
         min_temp = Tmin,
         max_temp = Tmax,
         mean_temp = Tair) %>% 
  select(-matches("^RH"))


# checking column consistency: 

# adding in temp_mean column to all files that don't have it
# doing this for consistency b/ there are sites that provided temp_mean

all7 <- all6
all7 <- map2(all7, names(all6), function(x, name){
  if (!"mean_temp" %in% names(x$weather)) {
    x$weather$mean_temp <-  NA_real_
  } else {
        message(paste("mean_temp column already present in:", name, "\n"))
    }
  x
})

# all7$cap_whitetank_4_20$weather <- all7$cap_whitetank_4_20$weather %>% 
#   rename(precip = PRCP)

wthr_col_names2 <- c(wthr_col_names, "mean_temp")
wthr_col_string2 <- wthr_col_names2 %>% sort() %>% paste(collapse = ",")

# check if all col names fixed
check_names_in_list(
  list = all7,  element_name = "weather", names = wthr_col_string2,
  warning = "Not all weather tables have the correct column names"
)


# weather table--correct data types ------------------------------------------

# I'm not sure what the 'T' flag means
all7$BFL_SLP$weather$precip[all7$BFL_SLP$weather$precip == "T"] <- NA

# for converting col types
parse_wthr_cols <- function(x) {
  x$weather <- x$weather %>% 
    mutate_all(as.character) %>% # first all to character
    mutate_at(vars(precip, min_temp, max_temp, mean_temp),
              .funs = as.numeric)
  x
}

# so can get warnings on what which don't parse:
parse_wthr_cols_warn <- quietly(parse_wthr_cols) 

# try parsing and look at warnings
parse_warnings1 <- map(all7, function(x) {
  parse_wthr_cols_warn(x)$warning
})

# sites that had warnings (i.e need to look at these) [should be none]
discard(parse_warnings1, function(x) length(x) == 0)

all8 <- all7

all8 <- map(all8, parse_wthr_cols) # shouldn't be any parsing warnings. 

# remove example template rows -----------------------------------------------

# map(all8, function(x) x$weather[1:3, ]) %>% 
#   bind_rows() %>% View()

# rows where example station listed--
# check this because some sites may enter "example station" for actuall data rows
map(all8, function(x) {
  x$weather %>% 
    filter(station_name == 'example station')
}) %>% 
  discard(function(x) nrow(x) ==0)

# potrok [actual data rows--just no station_name given]
all8$Potrok_Aike_Patagonia_Peri_Toledo_$weather$station_name <- 
  all8$Potrok_Aike_Patagonia_Peri_Toledo_$station$station_name

# only run once checked above that ok to discard these rows
all8 <- map(all8, function(x) {
  x$weather <- x$weather %>% 
    filter(station_name != 'example station' |is.na(station_name))
  x
})

# check missing date/station_name -----------------------------------------

# all rows should have a station_name and a date

# combine all weather sheets into one df
wthr1 <- extract_elements_2df(all8, element = "weather")

# files where there are NAs in date/station_name
wthr1 %>% 
  filter(is.na(date)| is.na(station_name)) %>% 
  group_by(file_name) %>% 
  summarise(n = n(),
            .groups = "drop")


# cimpozuelos ~~~~~~~~~~~

# issue here is that half the dates were in date format in excel
# the others were in text, those didn't parse in readxl

ciemp_wthr_aranj <- read_xlsx(
  file_paths["Ciempozuelos"],
  sheet = "weather",
  # forcing date to read in as text
  col_types = c("text", "text", "numeric", "numeric", "numeric", "text"))  %>% 
  filter(station_name == "Aranjuez Station")

ciemp_wthr_vald <- all8$Ciempozuelos$weather %>% 
  filter(station_name == "Valdemoro Station")

all8$Ciempozuelos$weather <- bind_rows(ciemp_wthr_vald, ciemp_wthr_aranj) 
  

# Lamb ~~~~~~~~~~~~~~~~
# note no precip data from Saskatoon_INT station [PI may submit it in future]

all8$Lamb_Oct112_19$station$note_station

# saskatoon dates not parsed
lamb_wthr_sas <- read_xlsx(
  file_paths["Lamb_Oct112_19"],
  sheet = "weather",
  # forcing date to read in as text
  col_types = c("text", "text", "numeric", "numeric", "numeric", "text")) %>% 
  filter(station_name == "Saskatoon_INT")

lamb_wthr_other <- all8$Lamb_Oct112_19$weather %>% 
  filter(station_name != "Saskatoon_INT")

all8$Lamb_Oct112_19$weather <- bind_rows(lamb_wthr_other, lamb_wthr_sas)

# rhijnauwen ~~~~~~~~~~~~~

# only first couple dates provided, they said they were sequential
first_date <- all8$rhijnauwen$weather$date[1]
n <- length(all8$rhijnauwen$weather$precip)

all8$rhijnauwen$weather$date <- seq(from = ymd(first_date), 
                                    length.out = n,
                                    by = "day") %>% 
  as.character()

# Pineta
all8$Pineta2014_2019$weather <- all8$Pineta2014_2019$weather %>% 
  mutate(station_name = ifelse(is.na(station_name),
                               all8$Pineta2014_2019$station$station_name,
                               station_name))
# bamboo
all8$Bamboo_drought_$weather$station_name <- 
  all8$Bamboo_drought_$station$station_name

# matta
# summary stats given in additional row with "NA", discarding here
all8$Matta_LTER$weather <- all8$Matta_LTER$weather %>% 
  filter(!is.na(date))

# santacruz--some station names left black--but only one station given so filling in
all8[c("SantaCruzMiddle", "SantaCruzHigh")] <- map(
  all8[c("SantaCruzMiddle", "SantaCruzHigh")],
  function(x) {
    x$weather <- x$weather %>% 
      mutate(station_name = ifelse(is.na(station_name),
                                   x$station$station_name,
                                   station_name))
    x
  }
)

# yarramundi (station name missing for some rows)
all8$Yarramundi_on_site_met_data_2013_2019$weather$station_name <- 
  all8$Yarramundi_on_site_met_data_2013_2019$station$station_name

# some rows missing the station name
all8$gmdrc_4_20$weather$station_name <- unique(all8$gmdrc_4_20$station$station_name)

# see if all missing values fixed:
missing_date_name <- extract_elements_2df(all8, element = "weather") %>% 
  filter(is.na(date)| is.na(station_name)) %>% 
  group_by(file_name) %>% 
  summarise(n = n(),
            .groups = "drop") 

if(nrow(missing_date_name) > 0) {
  warning("some files still missing date/station_names")
}


# parse dates -------------------------------------------------------------

all9 <- all8

# checking date origin in excel spreadsheets 
# (some older mac versions of excel have different origin so good to check)
origins <- map_chr(file_paths, openxlsx::getDateOrigin)
if(!all(origins == "1900-01-01")){
  warning("not all origins in excel files the same")
}

# if date is 5 digits (days since origin) convert to date
all9 <- map(all9, function(x) {
  x$weather$date <- parse_if_5digit_date(x$weather$date)
  x
})

# warnings from parsing to date
parse_warnings2 <- map(all9, function(x){
  parse_date_warn(x$weather$date)
})

# look at sites that got warnings
bad_dates <- discard(parse_warnings2, function(x) length(x) == 0)
bad_dates
names(bad_dates)

# checking out what the non-standard date formats are looking like
map(all9[names(bad_dates)], function(x) {
  x$weather$date[!str_detect(x$weather$date, "\\d{4}-\\d{2}-\\d{2}")] %>% 
    unique() 
})


# GCN_Suihua ~~~
all9$GCN_Suihua$weather$date <- ymd_hms(all9$GCN_Suihua$weather$date) %>% 
  as_date() %>% 
  as.character()

# bamboo ~~~
bam_date <- all9$Bamboo_drought_$weather$date
# two feb 29 not leap years, discarding those rows
bam_not_parsed <- is.na(ymd(bam_date))
bam_date[bam_not_parsed] 

all9$Bamboo_drought_$weather <- all9$Bamboo_drought_$weather[!bam_not_parsed, ]

# matta ~~~ (2 date formats given)
# note: throwing a parse error but all are actually parsing
all9$Matta_LTER$weather <- all9$Matta_LTER$weather %>% 
  mutate(is_dmy = ifelse(str_detect(date, "\\d{2}/\\d{2}/\\d{4}"),
                         TRUE, FALSE),
         date = ifelse(is_dmy,
                       as.character(dmy(date)),
                       date)
         ) %>% 
  select(-is_dmy)
  

# check that all dates now parsable--

all_parsable <- map(all9, function(x){
  parse_date_warn(x$weather$date)
}) %>% 
  every(function(x) length(x) == 0)

if (!all_parsable) warning("some dates still can't parse")

# now convert to date
all9 <- map(all9, function(x){
  if(any(is.na(x$weather$date))) {
    stop("some NA dates")
  }
  x$weather$date <- ymd(x$weather$date)
  x
})


# fixing known temp/precip anomolies ---------------------------------------
all10 <- all9

# PI confirmed at this site NAs are 0. No actual missing data
all10$IMGERS$weather$precip[is.na(all10$IMGERS$weather$precip)] <- 0

# combine GCN precip from sites that supplied both daily and monthly data seperately
names(gcn1)

# Youyu ~~~~

range(all10$GCN_Youyu_1_$weather$date) # daily data
range(all10$GCN_Youyu$weather$date) # from submitted monthly data

# the data from monthly precip doesn't have any extra dates so can discard
all10$GCN_Youyu$weather %>% 
  filter(!date %in% all10$GCN_Youyu_1_$weather$date) %>% 
  nrow()
all10$GCN_Youyu <- NULL


# check station_name matching ---------------------------------------------

# first look for duplicated rows in station table
dup_stn_rows <- map_dbl(all10, function(x) {
  x$station %>% 
    select(site, station_name) %>% 
    .[duplicated(.), ] %>% 
    nrow()
})
dup_stn_rows[dup_stn_rows > 0]

# making distinct names for the two stations
all10$BadLauchstaedt_complete$station$note_station
all10$BadLauchstaedt_complete$station$station_name <- 
  c("Bad Lauchstaedt 1", "Bad Lauchstaedt 2") 

#two stations for two different date ranges
all10$BadLauchstaedt_complete$weather <- all10$BadLauchstaedt_complete$weather %>% 
  mutate(year = year(date),
         station_name = ifelse(year <= 2017,
                               "Bad Lauchstaedt 1", 
                               "Bad Lauchstaedt 2")
         ) %>% 
  select(-year)
  


# make sure all station names are present in both weather and station tables

station_name_present <-  check_station_names(all10)

# files with issues:
station_issues <- keep(station_name_present, function(x) !x$all_good)
station_issues
names(station_issues)

# fixing individual sites:

# site name used instead of station name so correcting
all10$baoku$weather$station_name <- all10$baoku$station$station_name 


# sites where just spelling between the two sheets 
# (put weather sheet version in station sheet).

# manually checked that these name swaps make sense
dif_spell <- c('IMGERS', 'EEA_Ufrgs', 'GCN_Suihua', 'Lamb_Oct112_19')

all10[dif_spell] <- 
  map2(all10[dif_spell], station_issues[dif_spell], function(x, y) {
    x$station$station_name[x$station$station_name == y$not_in_weather] <- 
      y$not_in_station
    x
}) 

# manually deal with the others:

# Kiskunsag ~~~

# site name put instead of station name
all10$Kiskunsag$weather$station_name %>% unique()
all10$Kiskunsag$weather$station_name <- all10$Kiskunsag$station$station_name

# santacruz
# spelling inconsistency
all10$SantaCruzMiddle$weather$station_name <- 
  all10$SantaCruzMiddle$station$station_name

# improper name given in weather sheet
all10$SantaCruzLow$weather$station_name %>% unique()

all10$SantaCruzLow$weather$station_name <- 
  all10$SantaCruzLow$station$station_name

# SERDP
all10$SERDP_clean$weather$station_name <- all10$SERDP_clean$weather$station_name %>% 
  str_replace("Boise - KBOI", "Boise_KBOI") %>% 
  str_replace("Jornada HQ", "Jornada Headquarters")

# check station names now rectified:

still_bad <- keep(check_station_names(all10), function(x) !x$all_good)

if(length(still_bad) > 0) {warning("still have station name inconsistencies")}

# check data from multiple stations ---------------------------------------

all11 <- all10
# sometimes data from multiple stations was provided for a given site
# not a problem if they have different date ranges
# need to properly deal with it when they have overlapping dates

# duplicated dates for a given station
dup_stn_dates <- map_dbl(all11, function(x) {
  # duplicated site/date combo
  num_duplicated <- x$weather %>% 
    select(station_name, date) %>% 
    .[duplicated(.), ] %>% 
    nrow()
  num_duplicated
})

dup_stn_dates[dup_stn_dates > 0]

# if just 1 duplicated date, just discard it
one_dup <- names(dup_stn_dates[dup_stn_dates == 1])
all11[one_dup] <- map(all11[one_dup], function(x) {
  # remove duplicated site/date combo
  x$weather <- x$weather %>% 
    .[!duplicated(x$weather[, c("station_name", "date")]), ]
  x
})

# North platte

# looks like month and day were switched for 10 consecutive dates
np_dup_dates <- all11$NP_north_platte$weather %>% 
  select(station_name, date) %>% 
  .[duplicated(.), ] %>% 
  pull(date)

index <- which(((all11$NP_north_platte$weather$date %in% np_dup_dates) & 
        all11$NP_north_platte$weather$station_name == "north_platte"))

# these are actually consecutive days in feb
all11$NP_north_platte$weather[index[1:10], ]$date <- ydm(np_dup_dates)

# duplicated sites/date (i.e. multiple stations/site causing duplication):

dup_site_dates1 <- map2_dbl(all11, names(all11), function(x, name) {
  out <- x
  out$weather <- left_join(x$weather, x$station, by = "station_name")
  if (any(is.na(out$weather$site))) {
    warning(paste(name, " had join issue"))
  }
  
  # duplicated site/date combo
  num_duplicated <- out$weather %>% 
    select(site, date) %>% 
    .[duplicated(.), ] %>% 
    nrow()
  num_duplicated
})

dup_site_dates1[dup_site_dates1 > 0]

# boulder ~~~
# two stations given, NWTC is closer, and includes all dates that ESRL
# has so NWTC only kept
all11$Boulder$station$note_station

all11$Boulder$weather<- all11$Boulder$weather %>% 
  filter(station_name == "NWTC") 

all11$Boulder$station <- all11$Boulder$station %>% 
  filter(station_name == "NWTC") 

# Cowichan ~~~~~

all11$Cowichan_2019_11_5$weather$note_weather %>% unique()
# all11$Cowichan_2019_11_5$weather %>% summary()

ncc_stn <- all11$Cowichan_2019_11_5$weather %>% 
  filter(station_name == "CowichanNCC") # preferred station

NCow_stn <- all11$Cowichan_2019_11_5$weather %>% 
  filter(station_name == "NCowichanStation")


# my thinking here is to use the station data from the site,
# unless it is NA, then use the other site:

all11$Cowichan_2019_11_5$weather <- comb_primary_secondary_stns(ncc_stn, NCow_stn) 

# cusack ~~~~~~

# see note:
all11$Cusack_Panama$station$note_station

gig_bci <- all11$Cusack_Panama$weather %>% 
  filter(station_name == 'BCI')

gig_barb <- all11$Cusack_Panama$weather %>% 
  filter(station_name == 'Barbacoa') %>%  # preferred station for gigant
  filter(!is.na(precip)) # no temp data anyways


gig_wthr1 <- left_join(gig_barb, gig_bci, by = "date", suffix = c("_1", "_2")) %>% 
  mutate(station_name = station_name_1,
         precip = precip_1,
         min_temp = min_temp_2,
         max_temp = max_temp_2,
         mean_temp = mean_temp_2,
         note_weather = paste("Temp from BCI Station. ", note_weather_1)) %>% 
  select(all_of(wthr_col_names2))

# adding back in the 'barbacoa' data with temp from BCI added in
all11$Cusack_Panama$weather <- all11$Cusack_Panama$weather %>%
  filter(station_name != 'Barbacoa') %>% 
  bind_rows(gig_wthr1) 

# HY_and_HO ~~~~

# two stations one sites (primary) and an offsite secondary station to fill missing data
all11$HY_and_HO$station$station_name %>% unique()

HY_obs <- all11$HY_and_HO$weather %>% 
  filter(station_name == "observation on site")

HY_hid <- all11$HY_and_HO$weather %>% 
  filter(station_name == "Hiddensee-Vitte") # secondary station

HY_wthr1 <- comb_primary_secondary_stns(HY_obs, HY_hid)

all11$HY_and_HO$weather <- HY_wthr1

# norway
# the skotsvaer site didn't have temp so pulled temp from stor buoya
# here hadding store buoya temp to skotsvaer as well, so not duplicated sites/dates

# all11$Norway_2019$station %>% View()

skots <- all11$Norway_2019$weather %>% 
  filter(station_name == "Skotsvær")

store <-all11$Norway_2019$weather %>% 
  filter(station_name == "Store Buøya") 

skots_comb <- comb_primary_secondary_stns(skots, store)

all11$Norway_2019$weather <- all11$Norway_2019$weather %>% 
  filter(station_name != "Skotsvær") %>% 
  bind_rows(skots_comb)

# b/ data combined above discarding this row--better for later joins
all11$Norway_2019$station <- all11$Norway_2019$station %>% 
  filter(!(site == "Skotsvær" & station_name == "Store Buøya"))

# merging in site codes ---------------------------------------------------

# so merging issues aren't caused by erronious spaces/all caps
# make sure stn4 is highest number stn object (i.e. if code changed above)
#STOP pull stn back out

stn4 <- extract_elements_2df(all11, "station")

stn4$site_name_4merge <- stn4$site %>% 
  str_to_lower() %>% 
  str_replace_all("\\s", "")

# some sites from siteelev aren't in sites_wthrdata and vice versa so combining
# to increase chance of getting a match
site_name_code2 <- bind_rows(sites_wthrdata2, site_name_code) %>% 
  .[!duplicated(.), ] %>% 
  arrange(site_code)

dup_codes <- with(site_name_code2, site_code[duplicated(site_code)])

# sites that have site_names spelled differently in different places. 
site_name_code2 %>% 
  filter(site_code %in% dup_codes) %>% 
  arrange(site_code) 

site_name_code2$site_name_4merge <- site_name_code2$site_name %>% 
  str_to_lower() %>% 
  str_replace_all("\\s", "") 

site_name_code2 <- site_name_code2 %>% 
  filter(!duplicated(site_name_4merge))

stn5 <- left_join(stn4, site_name_code2, by = "site_name_4merge")

# sites that matched
stn5 %>% 
  filter(!is.na(site_code)) %>% 
  pull(site) %>% 
  unique() %>% 
  sort()

# sites that did not match
not_matching <- stn5 %>% 
  filter(is.na(site_code)) %>% 
  pull(site) %>% 
  unique() %>% 
  sort()
not_matching

# manually looked for associated site_codes
# this will need to be updated as new site codes are created
not_matching_lookup <- c('AA' = 'oreaa.us',
                         'AC' = 'oreac.us',
                         'Ämtvik' = 'unknown', # unknown site
                         'Bad Lauchstaedt' = 'baddrt.de',
                         'cap_mcdowell' = 'capmcd.us',
                         'cap_whitetank' = 'capwhite.us',
                         'Cedar Creek sIDE' = 'cedarsav.us',
                         'Cedar Creek tIDE' = 'cedartrait.us',
                         'Chacra Patagones' = 'chacra.ar',
                         'Las Chilcas' = 'chilcasdrt.ar',
                         'San Pablo Valdes' = 'spvdrt.ar',
                         'EEA_Ufrgs' = 'eea.br',
                         "Elva" = "elvadrt.ee",
                         'GCN-Suihua' = "unknown",# biomass still needs to be entered
                         # 'Gigante' = 'gigante.pa',
                         'gmdrc_granitecove' = 'gmgranite.us',
                         'gmdrc_molarjunction' = 'gmmolar.us',
                         "Haverøya" = 'haver.no', 
                         'KAEFS-OK' = 'oklah.us',
                         'Kranzberg' = 'kranz.de', # haven't submitted bio data
                         "Lygra_young" = 'lygrayng.no',
                         "Lygra_intermediate" = 'lygraint.no',
                         "Lygra_old" = 'lygraold.no',
                         "meadow_Stubai" = "stubai.at",
                         "NP" = "nplatte.us",
                         'Prades' = 'prades.es',
                         'sev_black' = 'sevblack.us',
                         'sev_blue' = 'sevblue.us',
                         'sev_mixed' = "sevmixed.us",  
                         "Skotsvær" = "skotsvar.no", #norway
                         "Store Buøya" = 'buoya.no', #norway
                         'Syferkuil South Africa' = 'syferkuil.za',
                         'Tovetorp' = "unknown", # haven't sent in bio data
                         'Wupatki' = 'antelope.us'
)
# sites that I  couldn't find a code for
not_matching_lookup[not_matching_lookup == "unknown"] %>% 
  names() %>% 
  sort()

stn6 <- stn5 %>% 
  mutate(site_code = ifelse(is.na(site_code),
                            not_matching_lookup[site],
                            site_code),
         # now using 'official' site_name when I have it
         site_name = ifelse(is.na(site_name), 
                            site,
                            site_name)) %>% 
  select(-site_name_4merge) 

stn6 %>% 
  filter(is.na(site_code)) %>% 
  .$site

wrong_site_code <- stn6$site_code[!stn6$site_code %in% site_name_code$site_code] %>% 
  .[.!= "unknown"]
if(length(wrong_site_code) != 0) warning("some incorrect site codes")

if(any(is.na(stn6$site_code))) warning("some site codes NA")

# merge into master file df -------------------------------------------------
all12 <- all11

# now putting stn table back into the lists
all12 <- map2(all12, names(all12), function(x, name) {
  x$station <- stn6[stn6$file_name == name, ]
  x
})

# adding site_code etc to weather data so it is identifiable
all13 <- map2(all12, names(all12), function(x, name) {
  out <- x
  stn <- x$station %>% 
    select(site_name, site_code, station_name, site)
  out$weather <- left_join(x$weather, stn, by = "station_name")
  
  if (nrow(out$weather) > nrow(x$weather)) {
    # occurs when two sites have same station_code
    message(paste("left join added rows for:", name))
  }
  out
})

# STOP--guaribas also provided with argentina sites
# not sure which one to use--data don't match
# so removing this here
all13$brazil_clean$weather <- all13$brazil_clean$weather %>% 
  filter(station_name != "Guaribas")

# create wthr master file
all_wthr1 <- map(all13, function(x) x$weather) %>% 
  bind_rows()

# sites where duplicated dates of data are occuring
dup_site_dates2 <- all_wthr1 %>% 
  select(date, site_name, station_name, precip) %>% 
  nest(data = c(date, station_name, precip)) %>% 
  mutate(n_dup = map_dbl(data, function(x) sum(duplicated(x$date)))
         ) %>% 
  filter(n_dup > 0)



# Gigante ~~~~

# data from two stations provided (one of them is closer/better)
# the other station (BIC) is the best station for some other sites

# dates for which the better station has data
bar_dates <- all_wthr1 %>% 
  filter(station_name == "Barbacoa") %>% 
  pull(date)

# mar chiquita and other .ar sites
# data provided in seperate files, so keeping better data
# unless those dates missing from better source

# no na's in primary data set of interest so this works fine
sum(all_wthr1$date[all_wthr1$station_name == "Mar Chiquita"] %in% 
      all_wthr1$date[all_wthr1$station_name == "Mar_del_Plata_Aero_87692"])

# no na's in primary data set of interest so this works fine
sum(all_wthr1$date[all_wthr1$station_name == "Naposta"] %in% 
      all_wthr1$date[all_wthr1$station_name == "EMA_Naposta"])

sum(all_wthr1$date[all_wthr1$station_name == "Potrok Aike"] %in% 
      all_wthr1$date[all_wthr1$station_name == "Campo Exp. Potrok Aike Santa Cruz"])


# when both monthly and daily data submitted, take daily, unless only monthly
# available

all_wthr1b <- discard_dup_station_data(all_wthr1, primary = "EMA_Naposta", 
                                       secondary = "Naposta",
                                       other_cols = TRUE) %>% 
  discard_dup_station_data(primary = "Mar_del_Plata_Aero_87692", secondary = "Mar Chiquita",
                           other_cols = TRUE) %>% 
  discard_dup_station_data("Campo Exp. Potrok Aike Santa Cruz", "Potrok Aike",
                           other_cols = TRUE) %>% 
  # monthly and daily:
  # STOP: somehow duplicates still showing up here
  discard_dup_station_data(primary = "Puerto Piramides - EEA Chubut", 
                           secondary = "San Pablo Valdes",
                           other_cols = TRUE) %>%
  # we have both monthly and daily submitted data:
  discard_dup_station_data(primary = "Hongyuan", 
                           secondary = "GCN-Hongyuan",
                           other_cols = TRUE)


all_wthr1c <- all_wthr1b %>% 
  filter(!(station_name == 'BCI' & site == "Gigante" & date %in% bar_dates)) 

all_wthr2 <- all_wthr1c %>% 
  filter(site_code == "eea.br") %>% 
  arrange(desc(station_name), date) %>% # sorting b/ worse (monthly) data
  # station name comes first in alphabet--ie data from two sources
  .[!duplicated(.$date), ] %>% 
  # adding other sites back in
  bind_rows(filter(all_wthr1c, site_code != "eea.br")) 


nrow(all_wthr1)- nrow(all_wthr2)

dup_site_dates3 <- all_wthr2 %>% 
  filter(site_code!= "unknown") %>% 
  group_by(site_code) %>% 
  nest() %>% 
  mutate(n_dup = map_dbl(data, function(x) sum(duplicated(x$date)))
  ) %>% 
  filter(n_dup > 0)

# note one possible cause of failing this check is that
# new data was submitted by a PI but it includes the data range of the 
# previously submitted data. In that case move the older submitted
# data to the "old" folder so it isn't read in by this script
if(nrow(dup_site_dates3) > 0) {
  warning("duplicated dates still present") 
} else {
  message("check passed")
}



# saving CSVs -------------------------------------------------------------

all_wthr_2save <- all_wthr2 %>% 
  select(-site)

write_csv(all_wthr_2save,
          file.path(path_oct, "data/precip/submitted_daily_weather_2021-03-02.csv"))

stn2save <- stn6 %>% 
  select(site_code, site_name, everything(), -site, -file_name) %>% 
  rename(station_elev = elev)

write_csv(stn2save,
          file.path(path_oct, "data/precip/submitted_weather_station_info_2021-03-02.csv"))
  
