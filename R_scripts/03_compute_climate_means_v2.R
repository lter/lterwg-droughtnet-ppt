# Purpose: Compute mean precipitation climate metrics for all droughtnet
# and nutnet sites. These are metrics where there can be 1 value (row) 
# per site (e.g. not monthly averages)
# This specifically includes precipitation metrics from the

# Author: Martin Holdrege

# Date started 5/10/2023


# dependencies ------------------------------------------------------------

library(tidyverse)
library(terra)
source("R_scripts/dropbox_path.R")
source("R_scripts/functions_ppt_metrics.R")
source("R_scripts/functions.R")

# Read in data ------------------------------------------------------------

# params ------------------------------------------------------------------

# time period used to calculate MAP and other ppt metrics from CHIRPS
# and mswep gridded data products
min_date = "1981-01-01"
max_date = "2020-12-31"

# read in data ------------------------------------------------------------

# * site locations -------------------------------------------------------

# IDE and npdknet sites
site0 <- read_csv(
  # includes coordinates of all IDE sites, plus some new npkd sites
  file.path(path, "IDE/data_processed/Site_Elev-Disturb-npkd.csv"),
  show_col_types = FALSE)

# NutNet sites
p_nn <- newest_file_path(
  path = file.path(path,'IDE/data_processed/NutNet'),
  file_regex = 'sites-\\d{4}-\\d+-\\d+.csv'
)

site_nn1 <- read_csv(p_nn, show_col_types = FALSE)


# * mswep -------------------------------------------------------------------

# file created in 02_mswep_extract_daily_ppt.R
p_mswep <- newest_file_path(
  path = file.path(path,'IDE/data_raw/climate/mswep/'),
  file_regex = 'mswep_ppt_daily_all-sites_\\d{4}-\\d+-\\d+.csv' 
)

mswep1 <- read_csv(p_mswep, show_col_types = FALSE)

# * wc avg monthly precip ----------------------------------------------------

rast_paths <- list.files(
  file.path(path, "IDE Meeting_Oct2019/data/precip/wc2.0_30s_prec"),
  pattern = "wc2.0_30s_prec_\\d{2}.tif",
  full.names = TRUE)
names(rast_paths) <- str_extract(rast_paths, "\\d{2}(?=.tif)")

stopifnot(length(rast_paths) == 12) # should be 12 months

rasts <- map(rast_paths, terra::rast)

# * wc monthly avg temp ------------------------------------------------------

rast_paths_t <- list.files(
  file.path(path, "IDE MS_Single year extreme/Data/wc2.1_30s_tavg"),
  pattern = "wc2.1_30s_tavg_\\d{2}.tif",
  full.names = TRUE)
names(rast_paths_t) <- str_extract(rast_paths_t, "\\d{2}(?=.tif)")

stopifnot(length(rast_paths_t) == 12) # should be 12 months

rasts_t <- map(rast_paths_t, terra::rast)


# * aridity index ---------------------------------------------------------
# Global-AI_PET_v3 dataset from CGIAR-CSI
# data downloaded from:  https://doi.org/10.6084/m9.figshare.7504448.v5
# which is the data associated with this article:
# https://doi.org/10.1038/s41597-022-01493-1
# after downloading the data I only kep the ai .tif
# not the et0 .tif (to save space)

# mean annual arridity index
r_ai1 <- rast(file.path(
  path, "IDE/data_raw/climate/Global-AI_ET0_v3_annual/ai_v3_yr.tif"))

# prepare data ------------------------------------------------------------

cols <- c("site_code", "longitude", "latitude")

site_nn2 <- site_nn1 %>% 
  # avoiding duplicates (this only happened for xilin.cn)
  filter(!site_code %in% site0$site_code)

site2 <- bind_rows(site0[, cols], site_nn2[, cols])

stopifnot(all(!duplicated(site2$site_code))) # shouldn't be duplicates
stopifnot(all(!is.na(site2))) # shouldn't be any missing values

# Monthly average data -------------------------------------
# for now just using worldclim

# * precip ----------------------------------------------------------------

# mean monthly ppt extracted for each site
wc_mo <- map2_dfr(rasts, names(rasts), function(x, name) {
  out <- site2[ , "site_code"]
  out$ppt <- terra::extract(x, site2[, c("longitude", "latitude")])[[2]]
  out$month <- as.numeric(name)
  out
})

# stopifnot(all(!is.na(wc_mo))) # some sites have missing data

# * tavg ------------------------------------------------------------------

# mean monthly daily avg temp extracted for each site
wc_tavg_mo <- map2_dfr(rasts_t, names(rasts_t), function(x, name) {
  out <- site2[ , "site_code"]
  # the first column is just an ID column
  out$tavg <- terra::extract(x, site2[, c("longitude", "latitude")])[[2]]
  out$month <- as.numeric(name)
  out
})

# stopifnot(all(!is.na(wc_tavg_mo)))

# * calculate r -----------------------------------------------------------
# correlation between mean monthly precipitation and temperature
wc_r <- wc_mo %>% 
  left_join(wc_tavg_mo, by = c("site_code", "month")) %>% 
  group_by(site_code) %>% 
  summarize(r_monthly_t_p = cor(ppt, tavg))

# compute mean annual metrics ---------------------------------------------
# including seasonality metrics

# * mswep ------------------------------------------------------------------
cutoff <- 1 # excluding 1mm or less events

mswep2 <- mswep1 %>% 
  filter( date >= min_date,
                 date <= max_date) 

ann0 <- mswep2 %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(site_code, year) %>% 
  summarize(ppt_max_event = max(precip),
            ppt_mean_event = mean(precip[precip > cutoff]),
            # number of biggest event days in which half of precip fell
            days_half_ppt = days_half_ppt(precip),
            # measure of variability in event sizes (similar to CV)
            daily_ppt_d = d_variability(precip, cutoff = 1),
            # number of wet days per year
            n_wet_days = n_wet_days(precip),
            # average length of precip free periods
            avg_dryspell_length = avg_dryspell_length(precip),
            # size of the 95th percentile event size (on days with precip)
            ppt_95th_percentile_size = ppt_percentile_size(precip, prob = 0.95)
            ) %>% 
  group_by(site_code) %>% 
  select(-year) %>% 
  summarize(across(where(is.numeric), .fns = mean))

ann1 <- ppt_mean_annual(mswep1, min_date = min_date,
                             max_date = max_date) %>% 
  # not including dataperiod here b/ also including worldclim data
  # below which isn't the same period
  select(-data_period)

ann2 <- left_join(ann0, ann1, by = "site_code")

# * aridity index ---------------------------------------------------------

# as per documentation values need to be multiplied 
# by 0.0001 to retrieve the values in the correct units.
r_ai2 <- r_ai1*0.0001

ai_yr <- site2[ , "site_code"]

ai_yr$aridity_index <- terra::extract(r_ai2, site2[, c("longitude", "latitude")])[[2]]
ai_yr$data_source <- "Global-AI_PET_v3"


# * MAT -------------------------------------------------------------------

# days per month for weighted averages
dates <- paste("2021", 1:12, "1", sep = "-") 

days_per_mo <- lubridate::days_in_month(dates)
days_per_mo[2] <- 28.25 #avg num days in January

wc_tavg_yr <- wc_tavg_mo %>% 
  mutate(weight = days_per_mo[month]) %>% 
  group_by(site_code) %>% 
  summarize(MAT = weighted.mean(tavg, w = weight))

# * combine ---------------------------------------------------------------

ann3 <- left_join(ann2, ai_yr[, c("site_code", "aridity_index")], 
                  by = "site_code") %>% 
  left_join(wc_r, by = "site_code") %>% 
  left_join(wc_tavg_yr, by = "site_code") %>% 
  ungroup()

# rows that have NAs--some norwegion sites have worldclim data missing (perhaps
# because close to coast)
ann3[is.na(rowSums(ann3 %>% select(where(is.numeric)))), ]


# write output ------------------------------------------------------------

write_csv(ann3, file.path(path, "IDE/data_processed/climate/climate_mean_annual_by_site_v3.csv"))


