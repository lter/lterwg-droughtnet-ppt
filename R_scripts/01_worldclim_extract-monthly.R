# Script started 7/22/20 by martin holdrege

# Purpose: extract monthly precip from worldclim from 1960-2018 for each IDE site


# dependencies ------------------------------------------------------------

library(tidyverse)

# paths -------------------------------------------------------------------

# Note I have removed the monthly worldclim data 
# from the dropbox folder because it is huge
# it is currently just living on my external drive.

path_dropbox <- '~/Dropbox'

path_ms <- file.path(path_dropbox, 'IDE MS_Single year extreme')


# load site_info ----------------------------------------------------------

site_info1 <- read.csv(file.path(path_ms, "Data\\Site_Elev-Disturb.csv"),
                      as.is = TRUE)

site_info2 <- site_info1 %>% 
  select(site_code, latitud, longitud)

# sites that Baoku emailed
new_site_df <- tribble(
  ~site_code, ~latitud, ~longitud,
  "maodeng.cn", 44.1, 116.28,
  "siziwang.cn", 41.8, 111.9,
  "taihang.cn", 37.5244, 114.155
)

# if any of new sites are in the site_info they don't need to be added
stopifnot(!new_site_df %in% site_info2$site_code)

site_info2 <- bind_rows(site_info2, new_site_df)

# extract precip ----------------------------------------------------------

# these files are on my toshiba external drive
folders <- list.files("D:/Not on computer/worldclim_can_delete", 
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

# data check

# no sites should have NAs
missing <- wc_ppt2 %>% 
  filter(is.na(wc_ppt)) %>% 
  pull(site_code) %>% 
  unique()
if(length(missing) > 0) {
  warning("some sites have missing data")
  print(missing)
}
