# load and process submitted weather station data

# this script is designed to pull together the submitted weather station data 
# and for starters output a large file containing all the weather data (when possible).

# script started 9/30/19

# WORK IN PROGRESS, more checks on whether values aren't matching in joins are needed
# Next steps
# load all sheets of all files
# work on files that don't have the normal sheets
# once all have proper sheets go through and work on site sheet, station sheet, then weather sheet. 

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
  str_replace_all("\\s+|[,)(]", "_") %>% 
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
    read_xlsx(path = path, sheet = sheet) # not parsing all dates correctly
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

# precip data:

# col names are year (year number only given for the first month of year)
rain_yrs1 <- names(all1$wayqecha_met_summary$`daily rainfall`) %>% 
  str_extract('\\d{4}')

# filling in the NAs of years
rain_yrs2 <- rain_yrs1
value <- NA
for (i in 2:length(rain_yrs1)) {
  if (!is.na(rain_yrs1[i])) {
    value <- rain_yrs1[i]
  }
  rain_yrs2[i] <- value
}
rain_yrs1
rain_yrs2

rain_mon <- all1$wayqecha_met_summary$`daily rainfall`[1, ] # months
rain_mon_yr <- paste(rain_mon, rain_yrs2, sep = "_")

wayqecha_rain <- all1$wayqecha_met_summary$`daily rainfall`[-1, ]

names(wayqecha_rain) <- rain_mon_yr

wayqecha_rain <- wayqecha_rain %>% 
  gather(key = "mon_yr", value = "precip", matches("[a-z]+_\\d{4}$")) %>% 
  mutate(month = str_extract(mon_yr, "^[a-z]+"),
         year = str_extract(mon_yr, "\\d{4}$"),
         date_char = paste(`Day of month_NA`, month, year, sep = "-"),
         date = dmy(date_char))

# dates that didn't parse are just impossible dates (e.g 30-feb-2008)
wayqecha_rain %>% 
  filter(is.na(date))

wayqecha_rain <- wayqecha_rain %>% 
  filter(!is.na(wayqecha_rain$date)) %>% 
  select(date, precip)

max(wayqecha_rain$date) # this is (2010) ~5 years prior to experiment so not useful
# STOP: if new data becomes available this will need to be completed for now discard

all2$wayqecha_met_summary <- NULL

anom_sheets1


# check if all sheets present -----------------------------------------------

all2_sheets_check <- map_lgl(all2, check_names, 
                             names = "metadata,sites,station,weather") 

if (!all(all2_sheets_check)) warning("Not all list elements have correct tables")

##########################################################################
################ process site tables #####################################

all3 <- all2

# map(all3, function(x) head(x$site))

# fix site column names ---------------------------------------------------

site_col_names <- map(all2, function(x) names(x$site))

# files were site table don't have usual col names ~~~
which_sites_anom <- map_lgl(all2, function(x) {
  # do columns match these names?
  check_names(x$site, names = "pi,site,site_latitud,site_longitud") 
}) %>% 
  .[.== FALSE]

anom_sites <- site_col_names[names(which_sites_anom)] # column names of those sites
anom_sites


# see remark
all3$Syferkuil_South_Africa$sites$Remarks

all3$Syferkuil_South_Africa$sites$Remarks <- NULL

all3$SonoraAgrilifeResearchStation$sites

all3$SonoraAgrilifeResearchStation$sites <- 
  all2$SonoraAgrilifeResearchStation$sites %>% 
  select(pi, site, latitude, longitude) %>% 
  rename(site_latitud = latitude, site_longitud = longitude)

# check if all col names fixed
if(!all(
    map_lgl(all3, function(x) {
      check_names(x$site, names = "pi,site,site_latitud,site_longitud") 
    }))
   ) {
  warning("Not all site tables have the correct column names")
}


# check site table contents -----------------------------------------------

map(all3, function(x) head(x$site))

all3 <- map(all3, function(x){
  x$site <- x$site[x$site$site != "Nowhere", ] # removing example row where necessary
  x
})

# all have non NA rows?
at_least_1row <- map_lgl(all3, function(x){
  nrow(x$site[!is.na(x$site$site), ]) >=1
})

if(!all(at_least_1row)) warning("some files have no site information")

##########################################################################
################ process station tables ##################################

all4 <- all3


# check/fix station table col names -----------------------------------------

station_col_names <- map(all3, function(x) names(x$station))

# files were station tables don't have usual col names ~~~
station_names_good <- map_lgl(all3, function(x) {
  # do columns match these names?
  check_names(x$station, names = "distance,elev,note_station,site,source,station_id,station_latitud,station_longitud,station_name,url") 
}) 

anom_col_names <- station_col_names[!station_names_good]
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
