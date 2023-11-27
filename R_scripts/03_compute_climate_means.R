# Martin Holdrege

# Script started 3/25/21

# Purpose:
# compute MAP, MAT, inter and intrannual ppt cv and other climate metrics
# for IDE sites using multiple (gridded) climate products. To create
# master files that provide these values in one location

# dependencies ------------------------------------------------------------

library(tidyverse)
theme_set(theme_classic())
library(patchwork) # for combining plots together
library(terra)
source("R_scripts/functions.R") # where seasonality_index() defined
source("R_scripts/functions_ppt_metrics.R") # where seasonality_index() defined
source("R_scripts/dropbox_path.R") # set path to dropbox in this file as needed


# params ------------------------------------------------------------------

# time period used to calculate MAP and other ppt metrics from CHIRPS
# and mswep gridded data products
min_date = "1981-01-01"
max_date = "2020-12-31"

# read in data ------------------------------------------------------------

# * lat/lon ---------------------------------------------------------

site1 <- read_csv(file.path(
  path, "IDE/data_processed/Site_Elev-Disturb.csv"),
  show_col_types = FALSE)

# * mswep -------------------------------------------------------------------

# file created in 02_mswep_extract_daily_ppt.R
p_mswep <- newest_file_path(
  path = file.path(path,'IDE/data_raw/climate/mswep/'),
  file_regex = 'mswep_ppt_daily_all-sites_\\d{4}-\\d+-\\d+.csv' 
)

mswep1 <- read_csv(p_mswep, show_col_types = FALSE)

# * chirps ----------------------------------------------------------------

# data ppt data 1981, to present. from CHIRPS gridded product (downloaded
# in 01_CHIRPS_climate-data_download.R script). Note this data is only 
# available for sites with latitudes between -50 and 50 degrees.

paths_chirps <- list.files(
  path = file.path(path, "IDE/data_raw/climate/CHIRPS"),
  pattern = "^CHIRPS.+.csv$", full.names = TRUE)

# extracting site_code from file name
names(paths_chirps) <- str_extract(basename(paths_chirps), 
                                   "(?<=ppt_).+\\.[a-z]{2}(?=_\\d{4})")
chirps1 <- map(paths_chirps, read_csv, 
               col_select = c("date", "precipitation", "site_code"),
               show_col_types = FALSE, na = c("-9999", -9999))

# discarding sites for which all values are missing (this might
# occur if a sight is at the edge of the -50 or 50 deg latitude line)
is_missing <- map_lgl(chirps1, function(df) all(is.na(df$precipitation)))

# the noor.ir site has missing values b/ the grid cells don't go all
# the way to the inland sea it is located on
missing_names <- names(which(is_missing))

chirps2 <- bind_rows(chirps1[!is_missing]) %>% 
  rename(ppt = precipitation)


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


# Monthly average data -------------------------------------

site2 <- site1 %>% 
  select(site_code, latitud, longitud) %>% 
  mutate(latitud = as.numeric(latitud),
         longitud = as.numeric(longitud))

stopifnot(all(!is.na(site2))) # shouldn't be any missing values


# * precip ----------------------------------------------------------------

# **worldclim -------------------------------------------------------------

# mean monthly ppt extracted for each site
wc_mo <- map2_dfr(rasts, names(rasts), function(x, name) {
  out <- site2[ , "site_code"]
  out$ppt <- terra::extract(x, site2[, c("longitud", "latitud")])[[2]]
  out$month <- as.numeric(name)
  out
})

stopifnot(all(!is.na(wc_mo))) # check that all months/sites have data


# **mswep -----------------------------------------------------------------

mswep_mo <- ppt_mean_monthly(mswep1, min_date = min_date,
                             max_date = max_date) %>% 
  mutate(data_source = "mswep")

# **chirps ----------------------------------------------------------------

chirps_mo <- ppt_mean_monthly(chirps2, min_date = min_date,
                             max_date = max_date) %>% 
  # left join b/ some not all sites have chirps data, that 
  # whey they are preserved in the output, but with NAs
  right_join(site2[, "site_code"], by = "site_code") %>% 
  mutate(data_source = "chirps") 

# * tavg ------------------------------------------------------------------

# mean monthly daily avg temp extracted for each site
wc_tavg_mo <- map2_dfr(rasts_t, names(rasts_t), function(x, name) {
  out <- site2[ , "site_code"]
  # the first column is just an ID column
  out$tavg <- terra::extract(x, site2[, c("longitud", "latitud")])[[2]]
  out$month <- as.numeric(name)
  out
})

stopifnot(all(!is.na(wc_tavg_mo))) # check that all months/sites have data


# * combine -----------------------------------------------------------------

mo1 <- full_join(wc_mo, wc_tavg_mo, by = c("site_code", "month")) %>% 
  select(site_code, month, everything()) %>% # more intuitive column order
  mutate(data_source = "worldclim")

stopifnot(nrow(mo1) == 12*nrow(site2)) # confirm join was clean (not adding rows)

mo2 <- bind_rows(mo1, mswep_mo, chirps_mo) %>% 
  arrange(site_code, month)

# compute mean annual metrics ---------------------------------------------
# including seasonality metrics

# * mswep ------------------------------------------------------------------

mswep_ann <- ppt_mean_annual(mswep1, min_date = min_date,
                                max_date = max_date) %>% 
  mutate(data_source = 'mswep')

# * chirps ------------------------------------------------------------------

# monthly level stats
chirps_ann <- ppt_mean_annual(chirps2, min_date = min_date,
                             max_date = max_date) %>% 
  # left join b/ some not all sites have chirps data, that 
  # whey they are preserved in the output, but with NAs
  right_join(site2[, "site_code"], by = "site_code") %>% 
  mutate(data_source = 'chirps') 

# * worldclim -------------------------------------------------------------

# annual data--worldclim
wc_ann <- wc_mo %>% 
  group_by(site_code) %>% 
  summarize(MAP = sum(ppt), .groups = "drop") %>% 
  mutate(data_source = "worldclim")


# * aridity index ---------------------------------------------------------

# as per documentation values need to be multiplied 
# by 0.0001 to retrieve the values in the correct units.
r_ai2 <- r_ai1*0.0001

ai_yr <- site2[ , "site_code"]

ai_yr$aridity_index <- terra::extract(r_ai2, site2[, c("longitud", "latitud")])[[2]]
ai_yr$data_source <-  "Global-AI_PET_v3"
# * combine ---------------------------------------------------------------

ann1 <- site1 %>% 
  select(site_code, precip) %>%
  rename(MAP = precip) %>% 
  mutate(data_source = "submitted") %>% 
  bind_rows(., wc_ann, mswep_ann, chirps_ann, ai_yr) %>% 
  arrange(site_code, data_source)

# save files --------------------------------------------------------------

metadata <- c(
  "This is a description file for the following two files:\n",
  "climate_mean_annual_by_site.csv and climate_mean_monthly_by_site.csv",
  "Which are created by 03_compute_climate_means.R which can be found on github",
  "",
  "Description of columns for climate_mean_annual_by_site.csv:",
  "MAP -- mean annual precipitation (mm/yr)",
  "data_source -- the data source used for the climate data, these",
  "are gridded products (worldclim, mswep, chirps) and also values provided by",
  "site PIs which are labeled 'submitted'",
  "data_period--the period of time (years) which the average values are based on",
  "cv_ppt_intra -intra-annual precip variability calculated\n",
  "as the cv of monthly ppt in a given year, then averaged across years",
  "cv_ppt_inter--interannual precipitation variability (ie cv of annual precipitation totals",
  "seasonality_index--seasonality index (SI) as derived by Walsh and Lawler (1981)",
   "culculated as sum(abs(x - Ri/12))/Ri, where x is monthly ppt, and Ri", 
  "annual ppt\n",
  "mean of seasonality index was calculated across years",
  "map--mean annual precipitation (mm)",
  "aridity_index--PET/MAP\n",
  "",
  "Description of columns for climate_mean_monthly_by_site.csv:",
  "ppt--mean precipitation (mm) recieved in the given month",
  "mean temperature (C) in the given month",
  "\nOther columns definitions are same as those above"
)

write_lines(metadata,  file.path(
  path, "IDE/data_processed/climate",
  "climate_by_site_metadata.txt" 
))
         

write_csv(ann1, file.path(
  path, "IDE/data_processed/climate",
  "climate_mean_annual_by_site_v3.csv" 
))


write_csv(mo2, file.path(
  path, "IDE/data_processed/climate",
  "climate_mean_monthly_by_site.csv" 
))


# exploratory figures -----------------------------------------------------

# comparing MAP between data sources
map_wide <- ann1 %>% 
  select(site_code, MAP, data_source) %>% 
  filter(!is.na(MAP)) %>% 
  mutate(data_source = paste0(data_source, "_MAP")) %>% 
  pivot_wider(values_from = "MAP",
               names_from =  "data_source")

caption <- paste0("Figures created on ", lubridate::today(), 
                  " by the 03_compute_climate_means.R script")

# columns to plot
cols <- names(map_wide) %>% str_subset("_MAP")

# all combinations of columns to plot
cols_comb <- gtools::combinations(n = length(cols),
                      r = 2,
                      v = cols,
                      repeats.allowed = FALSE) 

# note if new data sources are used the following 2 lines
# won't be useful so can be commented out (this is being done
# so that the order plots is the figure is easier to read)
cols_comb2 <- cols_comb[c(2, 4, 6, 3, 5, 1) ,]
cols_comb2[3, ] <- cols_comb2[3, c(2, 1)] # flipping order of x and y
cols_comb <- cols_comb2

colnames(cols_comb) <- c("y", "x")
cols_comb <- as_tibble(cols_comb)


# creating the plots

map_range <- c(0, max(ann1$MAP, na.rm = TRUE)) # for axis limits
plots_l <- pmap(cols_comb, function(y, x) {
  print(x)
  ggplot(map_wide, aes(x = .data[[x]], y = .data[[y]])) +
    geom_abline(slope = 1, intercept = 0) +
    geom_point(alpha = 0.5) +
    coord_cartesian(xlim = map_range,
                    ylim = map_range)
})

pdf(file.path(
  path, "IDE/figures/climate/MAP_data-source-comparison.pdf"),
  width = 7, height = 5
)
  wrap_plots(plots_l) +
    plot_layout(nrow = 2) +
    plot_annotation(caption = caption)

dev.off()


