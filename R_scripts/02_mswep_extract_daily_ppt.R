# Martin Holdrege

# Started: 12/22/2022

# Purpose: extract daily ppt data for all sites from the gridded ppt
# product 'mswep' (http://www.gloh2o.org/mswep/). I downloaded this
# data in the shell_scripts/01_mswep_download.sh script.


# dependencies -----------------------------------------------------------

library(terra)
library(tidyverse)
library(lubridate)
source("R_scripts/dropbox_path.R")
source("R_scripts/functions.R")

# params ------------------------------------------------------------------

date_string <- "2022-11-20" # for use in output files

# read in data ------------------------------------------------------------

# * mswep -----------------------------------------------------------------

# these files aren't on dropbox (b/ large)
r_paths <- list.files("D:/IDE_climate_rasters/MSWEP_V280/Daily/",
                      pattern = ".*\\.nc",
                      full.names = TRUE) %>% 
  sort()

# year and day of year from file name
ydoy <- basename(r_paths) %>% 
  str_replace("\\.nc", "")
names(r_paths) <- ydoy
# * site locations -------------------------------------------------------

site1 <- read_csv(
  # includes coordinates of all IDE sites, plus some new npkd sites
  file.path(path, "IDE/data_processed/Site_Elev-Disturb-npkd.csv"),
  show_col_types = FALSE)


# check file names --------------------------------------------------------
# checking if all dates are found in the file names or whether some dates
# are missing

# the year
y <- str_extract(ydoy, "^\\d{4}") %>% 
  as.numeric()
doy <- str_extract(ydoy, "\\d{3}$") %>% 
  as.numeric()
stopifnot(doy >=0 & doy <= 366)

# fake dates/ where each date is the first day of that year
dates <- ymd(paste0(y, "-01-01"))

# converting to actual date by setting day of year
yday(dates) <- doy

# differences between consecutive dates
d <- diff(dates)
min(dates); max(dates)
if(any(d !=1)) {
  stop('date range of mswep files is not complete\n',
       'problems around:\n', dates[d!=1])
}

# prepare data ------------------------------------------------------------

# convert to spatvector
site2 <- site1 %>% 
  select(site_code, longitude, latitude) %>% 
  vect(geom = c("longitude", "latitude"), crs = "EPSG:4326")

# extract ppt by site -----------------------------------------------------

years <- basename(r_paths) %>% 
  # extract the year from the filename
  str_extract("^\\d{4}") %>% 
  unique() %>% 
  sort()

# looping over years to break the task up into smaller chunks
# (i'm concerned about memory limitations otherwise)

out_name <- paste0(file.path(path, 
                   "IDE/data_raw/climate/mswep/mswep_ppt_daily_all-sites_"),
                   date_string, ".csv")

# first creating an empty csv files (with only column names)
dummy <- extract_mswep(rast(r_paths[1]), site2[1, ])

dummy <- dummy[c(), ]
write_csv(dummy, file = out_name)

# second appending data for each year (daily data) and site_code
# looping through years
for (yr in years) {
  
  path_names <- names(r_paths) %>% 
    str_subset(paste0("^", yr))
  
  paths_yr <- r_paths[path_names]
  
  r <- rast(paths_yr) # read in data just for the given year
  
  ppt_daily <- extract_mswep(r, site2)
  write_csv(ppt_daily, file = out_name, append = TRUE)
  message(yr, " complete")
}

