# load and process submitted weather station data

# this script is designed to pull together the submitted weather station data 
# and for starters output a large file containing all the weather data (when possible).

# script started 9/30/19

# WORK IN PROGRESS--NEXT load in GCN file list

# once finished this script is meant to:

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

path_may <- "E:/Dropbox/IDE Meeting_May2019"

path_oct <- "E:/Dropbox/IDE Meeting_Oct2019"


# site info ---------------------------------------------------------------

# make sure using most recent file
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

# only use BadLauchstaedt_weather_complete.xlsx (2nd file they sent)
file_names <- file_names[file_names != "BadLauchstaedt_weather.xlsx"]

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
    read_xlsx(path = path, sheet = sheet, 
              na = c("", "NA", "-", "Missing data, not registered", "ND")) 
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
  select(stn_col_names) # just keeping the main cols

# CHECK: shouldn't be any NAs in lat/lon if parsed correctly
stn3 %>% 
  filter(is.na(station_longitud) | is.na(station_latitud) | is.na(site) 
         | is.na(station_name))


# parsing distance/elv -------------------------------------------------------

stn3$distance # examine for characters e.g (miles etc)
stn3$elev

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


###########################################################################
############     Process Weather data   ###################################


# checking weather col names ----------------------------------------------

wthr_col_names <- c("station_name", "date", "precip", "min_temp", "max_temp", 
                   "note_weather")

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

all6$cap_whitetank$weather <- all6$cap_whitetank$weather %>% 
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
all6$IMGERS$weather$..7 <- NULL

# OR
all6$OR_Byrne$weather <- all6$OR_Byrne$weather[, wthr_col_names]

# PassoGavia ~~~
# keeping only data from the station with year round data
# not the on site summer only station. 

all6$PassoGavia$station$note_station
all6$PassoGavia$weather <- all6$PassoGavia$weather %>% 
  select(note_weather:max_temp2) %>% 
  rename(station_name = station_name2,
         date = date..8,
         precip = precip_2,
         min_temp = min_temp2,
         max_temp = max_temp2) %>% 
  select(wthr_col_names)

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
            note_weather = NA) %>% 
  ungroup()

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

wthr_col_names2 <- c(wthr_col_names, "mean_temp")
wthr_col_string2 <- wthr_col_names2 %>% sort() %>% paste(collapse = ",")

# check if all col names fixed
check_names_in_list(
  list = all7,  element_name = "weather", names = wthr_col_string2,
  warning = "Not all weather tables have the correct column names"
)


# weather table--correct data types ------------------------------------------

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
    filter(station_name != 'example station')
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
  summarise(n = n()) %>% 
  ungroup()


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

# see if all missing values fixed:
missing_date_name <- extract_elements_2df(all8, element = "weather") %>% 
  filter(is.na(date)| is.na(station_name)) %>% 
  group_by(file_name) %>% 
  summarise(n = n()) %>% 
  ungroup()

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

# yarramund ~~~~~~~~~~
# some dates in m/d/yyyy format
yar_date <- all9$Yarramundi_on_site_met_data_2014_2019$weather$date
yar_is_mdy <- str_detect(yar_date, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
yar_date[yar_is_mdy] <- mdy(yar_date[yar_is_mdy]) %>% 
  as.character()
all9$Yarramundi_on_site_met_data_2014_2019$weather$date <- yar_date

# check that all dates now parsable--

all_parsable <- map(all9, function(x){
  parse_date_warn(x$weather$date)
}) %>% 
  every(function(x) length(x) == 0)

if (!all_parsable) warning("some dates still can't parse")

# now convert to date
all9 <- map(all9, function(x){
  x$weather$date <- ymd(x$weather$date)
  x
})


# fixing known temp/precip anomolies ---------------------------------------
all10 <- all9

# PI confirmed at this site NAs are 0. No actual missing data
all10$IMGERS$weather$precip[is.na(all10$IMGERS$weather$precip)] <- 0

# STILL NEED TO DO:
# combine GCN precip from sites that supplied both daily and monthly data seperately



# merging in site codes ---------------------------------------------------

# so merging issues aren't caused by erronious spaces/all caps
# make sure stn4 is highest number stn object (i.e. if code changed above)
stn4$site_name_4merge <- stn4$site %>% 
  str_to_lower() %>% 
  str_replace_all("\\s", "")

site_name_code$site_name_4merge <- site_name_code$site_name %>% 
  str_to_lower() %>% 
  str_replace_all("\\s", "") 

stn5 <- stn4 %>% 
  left_join(site_name_code, by = "site_name_4merge")

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
not_matching %>% 
  paste(collapse = "' = ,'")

# manually looked for associated site_codes
not_matching_lookup <- c('AA' = 'oreaa.us',
                         'AC' = 'oreac.us',
                         'Ämtvik' = 'unknown',
                         'Bad Lauchstaedt' = 'baddrt.de',
                         'cap_mcdowell' = 'capmcd.us',
                         'cap_whitetank' = 'capwhite.us',
                         'EEA_Ufrgs' = 'eea.br',
                         'GCN-Xilinhot' = 'unknown',
                         'GCN-Youyu' = 'unknown',
                         'gmdrc_granitecove' = 'gmgranite.us',
                         'gmdrc_molarjunction' = 'gmmolar.us',
                         'Hongyuan' = 'unknown',
                         'KAEFS-OK' = 'oklah.us',
                         'Kranzberg' = 'unknown',
                         'Mar Chiquita' = 'marcdrt.ar',
                         'Potrok Aike' = "paike.ar",
                         'Prades' = 'prades.es',
                         'Puerto Pirámides-Estancia La Adela' = 'unknown',
                         'Swift Current' = 'swift.ca',
                         'Syferkuil South Africa' = 'syferkuil.za',
                         'Tovetorp' = "unknown",
                         'Yarramundi' = 'yarradrt.au')

# sites that I  couldn't find a code for
not_matching_lookup[not_matching_lookup == "unknown"] %>% 
  names() 

stn6 <- stn5
stn6 <- stn6 %>% 
  mutate(site_code = ifelse(is.na(site_code),
                            not_matching_lookup[site],
                            site_code)) %>% 
  select(-site_name_4merge, -site_name)

stn3 %>% names()
if(any(is.na(stn6$site_code))) warning("some site codes NA")

# next: merge weather and station info together, check for sites that aren't merging properly
#



