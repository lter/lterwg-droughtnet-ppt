# Script started 7/22/20 by martin holdrege

# Purpose: extract monthly precip from worldclim from 1960-2018 for each IDE site


# dependencies ------------------------------------------------------------

library(tidyverse)

# paths -------------------------------------------------------------------

# this is the only path that should need to be updated for a specific computer
path_dropbox <- '~/Dropbox'

path_oct <- file.path(path_dropbox, 'IDE Meeting_Oct2019')
path_ms <- file.path(path_dropbox, 'IDE MS_Single year extreme')


# load site_info ----------------------------------------------------------

site_info1 <- read.csv(file.path(path_ms, "Data\\Site_Elev-Disturb.csv"),
                      as.is = TRUE)

site_info2 <- site_info1 %>% 
  select(site_code, latitud, longitud)


# extract precip ----------------------------------------------------------

folders <- list.files(path = file.path(path_ms, "Data/precip"), 
                      full.names = TRUE) %>% 
  .[str_detect(., "wc2.1_2.5m_prec(?!.+zip$)")]

files <- list.files(folders, full.names = TRUE)

# year/month from file names
months <- str_extract(files, "\\d{4}-\\d{2}(?=.tif$)")
months
names(files) <- months

wc_ppt1 <- expand_grid(site_code = site_info2$site_code, yr_month = months) %>% 
  left_join(site_info2, by = "site_code") %>% 
  mutate(wc_ppt = NA_real_)

month <- months[1]
for (month in months) {
  print(month)
  # load raster
  x <- raster::raster(files[month])
  
  # extract ppt values from raster
  wc_ppt1[wc_ppt1$yr_month == month, ]$wc_ppt <- 
    raster::extract(x, y = wc_ppt1[wc_ppt1$yr_month == month, 
                                   c("longitud", "latitud")])
}

wc_ppt2 <- wc_ppt1 %>% 
  mutate(year = str_extract(yr_month, "^\\d{4}"),
         month = str_extract(yr_month, "\\d{2}$"),
         year = as.numeric(year),
         month = as.numeric(month)) %>% 
  select(site_code, year, month, wc_ppt)

wc_ppt2


# save files --------------------------------------------------------------

write_csv(wc_ppt2, 
          file.path(path_ms, "Data/precip", "worldclim_monthly_precip.csv"))

