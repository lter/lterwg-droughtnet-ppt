# calculating drought severity seperately for the falls.au site, because
# the ppt data from ghcn (which was the data source previously used for calculating
# annual ppt at this site) is clearly mismatched with the submitted
# MAP. So using annual ppt and MAP from a gridded product (chirps)
# so it is an apples to apples comparison

# Author: Martin Holdrege 

# started 1/16/2023


# dependencies ------------------------------------------------------------

library(tidyverse)


# reading in data ---------------------------------------------------------

path <- "D:/Dropbox" # adjust this to where dropbox is located

# annual ppt data
ppt1 <- read_csv(file.path(path, "IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_365-0days_2023-01-02.csv"))

# climate date (this file contains MAP)
clim1 <- read_csv(file.path(path, "IDE/data_processed/climate/climate_mean_annual_by_site.csv"))


# filtering data ----------------------------------------------------------

s <- 'falls.au' # site of interest

# MAP for the site
MAP <- clim1 %>% 
  filter(site_code == s, 
         data_source == 'chirps') %>% 
  pull(MAP)

# ppt in drought plots for 
ppt2 <- ppt1 %>% 
  filter(site_code == s, !fake_bioDat) %>% 
  group_by(site_code, year, trt) %>% 
  # may be multiple biomass dates in a year,
  # just taking the last one (for control and drought)
  filter(biomass_date == max(biomass_date)) %>% 
  # ppt in drought plots
  select(site_code, year, trt, ppt_chirps) %>% 
  distinct() %>% 
  pivot_wider(values_from = "ppt_chirps",
              names_from = "trt") %>% 
  ungroup()

# calculating drought severity
drt_sev <- ppt2 %>% 
  mutate(drt_sev = (Drought - MAP)/MAP)

# values to use
drt_sev


# # A tibble: 3 x 5
# site_code  year Control Drought drt_sev
# <chr>     <dbl>   <dbl>   <dbl>   <dbl>
#   1 falls.au   2017   1644.   1630.   0.227
# 2 falls.au   2018   1339.    803.  -0.395
# 3 falls.au   2019   1128.    677.  -0.491

# hard coding a dataframe based on the values in
# drt_sev (in case that easer to just insert into existing code)
drt_sev_falls.au <- tibble(
  site_code = "falls.au",
  year = c(2017, 2018, 2019),
  drt_sev = c(0.227, -0.395, -0.491),
  MAP = 1328.513
)
