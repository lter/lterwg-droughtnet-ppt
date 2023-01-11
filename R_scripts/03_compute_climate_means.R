# Martin Holdrege

# Script started 3/25/21

# Purpose of this code is to pull mean monthly precipitation mean monthly temp
# (this is means of daily average temp) from each site
# and then calculate various precipitation seasonality metrics

# note this code could be re-done so that it uses the worldclim_monthly_precip.csv
# file (ppt for each month/site since the 1960s)

# dependencies ------------------------------------------------------------

library(tidyverse)
source("R_scripts/functions.R") # where seasonality_index() defined
source("R_scripts/dropbox_path.R")

# read in lat/lon ---------------------------------------------------------

site1 <- read_csv(file.path(
  path, "IDE MS_Single year extreme/Data/Site_Elev-Disturb.csv"),
  )

# read in raster data -----------------------------------------------------

# * avg monthly precip ----------------------------------------------------

rast_paths <- list.files(
  file.path(path, "IDE Meeting_Oct2019/data/precip/wc2.0_30s_prec"),
  pattern = "wc2.0_30s_prec_\\d{2}.tif",
  full.names = TRUE)
names(rast_paths) <- str_extract(rast_paths, "\\d{2}(?=.tif)")

stopifnot(length(rast_paths) == 12) # should be 12 months

rasts <- map(rast_paths, raster::raster)

# * monthly avg temp ------------------------------------------------------

rast_paths_t <- list.files(
  file.path(path, "IDE MS_Single year extreme/Data/wc2.1_30s_tavg"),
  pattern = "wc2.1_30s_tavg_\\d{2}.tif",
  full.names = TRUE)
names(rast_paths_t) <- str_extract(rast_paths_t, "\\d{2}(?=.tif)")

stopifnot(length(rast_paths_t) == 12) # should be 12 months

rasts_t <- map(rast_paths_t, raster::raster)


# extract monthly ppt/tavg each site -------------------------------------

site2 <- site1 %>% 
  select(site_code, latitud, longitud) %>% 
  mutate(latitud = as.numeric(latitud),
         longitud = as.numeric(longitud))

stopifnot(all(!is.na(site2))) # shouldn't be any missing values


# * precip ----------------------------------------------------------------

# mean monthly ppt extracted for each site
ppt_mly1 <- map2_dfr(rasts, names(rasts), function(x, name) {
  out <- site2[ , "site_code"]
  out$ppt <- raster::extract(x, site2[, c("longitud", "latitud")])
  out$month <- as.numeric(name)
  out
})

stopifnot(all(!is.na(ppt_mly1))) # check that all months/sites have data

# * tavg ------------------------------------------------------------------

# mean monthly daily avg temp extracted for each site
tavg_mly1 <- map2_dfr(rasts_t, names(rasts_t), function(x, name) {
  out <- site2[ , "site_code"]
  out$tavg <- raster::extract(x, site2[, c("longitud", "latitud")])
  out$month <- as.numeric(name)
  out
})

stopifnot(all(!is.na(ppt_mly1))) # check that all months/sites have data


# combine -----------------------------------------------------------------

mly2 <- full_join(ppt_mly1, tavg_mly1, by = c("site_code", "month")) %>% 
  select(site_code, month, everything()) # more intuitive column order

stopifnot(nrow(mly2) == 12*nrow(site2)) # confirm join was clean (not adding rows)

# calculate seasonality metrics -------------------------------------------


# mean annual ppt
map <- mly2 %>% 
  group_by(site_code) %>% 
  summarize(map = sum(ppt), .groups = "drop")

# seas
seas1 <- mly2 %>% 
  group_by(site_code) %>%  
  summarize(month_max_ppt = month[ppt == max(ppt)], 
            month_max_tavg = month[tavg == max(tavg)],
            ppt_cv = sd(ppt)/mean(ppt),
            # apparently applying seasonality index to mean monthly data not
            # recommended , would be better to apply to yearly monthly data
            # then take an average
            seasonality_index = seasonality_index(ppt),
            .groups = "drop") %>% 
  left_join(map, by = "site_code")

# cv and SI strongly correlated
plot(seas1$seasonality_index, seas1$ppt_cv)

plot(seas1$map, seas1$ppt_cv)
# save files --------------------------------------------------------------

metadata <- c(
  "This is a description file for worldclim_seasonality_by_site.csv\n",
  "This file was created in worlclim_seasonality.R which can be found on github",
  "",
  "Data used for this file is worldclim monthly temperature and precipitation\n",
  "Description of columns:",
  "month_max_ppt--wettest month of year\n",
  "month_max_tavg--hottest month of year\n",
  "ppt_cv--coefficient of variation of mean monthly precipitation\n",
  "seasonality_index--seasonality index (SI) as derived by Walsh and Lawler (1981)",
  "note that SI here was applied to mean monthly ppt, it this is best when",
  "calculated for each year, and then an average taken,  it is not as good to,",
  "calculate it with average monthly precipitation as done here.",
  "culculated as sum(abs(x - Ri/12))/Ri, where x is mean monthly ppt, and Ri", 
  "mean annual ppt\n",
  "map--mean annual precipitation (mm)\n"
)

write_lines(metadata,  file.path(
  path, "IDE MS_Single year extreme/Data/precip",
  "worldclim_seasonality_by_site_metadata.txt" 
))
         

write_csv(mly2, file.path(
  path, "IDE MS_Single year extreme/Data/precip",
  "worldclim_mean_monthly_ppt_tavg_by_site.csv" 
))

write_csv(seas1, file.path(
  path, "IDE MS_Single year extreme/Data/precip",
  "worldclim_seasonality_by_site.csv" 
))

