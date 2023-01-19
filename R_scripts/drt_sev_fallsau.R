# calculating drought severity seperately for the falls.au site, because
# the ppt data from ghcn (which was the data source previously used for calculating
# annual ppt at this site) is clearly mismatched with the submitted
# MAP. So using annual ppt and MAP from a gridded product (chirps)
# so it is an apples to apples comparison

# Note--this file, at least at the moment, isn't being used anywhere

# Author: Martin Holdrege 

# started 1/16/2023


# dependencies ------------------------------------------------------------

library(tidyverse)


# reading in data ---------------------------------------------------------

path <- "D:/Dropbox" # adjust this to where dropbox is located

# annual ppt data (lag1)
ppt1 <- read_csv(file.path(path, "IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_365-0days_2023-01-02.csv")) %>% 
  mutate(lag = "365-0days")
 

# annual ppt data (lag2)
ppt2 <- read_csv(file.path(path, "IDE Meeting_Oct2019/data/precip/anpp_clean_trt_ppt_no-perc_730-365days_2023-01-02.csv")) %>% 
  mutate(lag = "730-365days")

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
ppt3 <- bind_rows(ppt1, ppt2) %>% 
  filter(site_code == s, !fake_bioDat) %>% 
  group_by(site_code, year, trt, lag) %>% 
  # may be multiple biomass dates in a year,
  # just taking the last one (for control and drought)
  filter(biomass_date == max(biomass_date)) %>% 
  # ppt in drought plots
  select(site_code, year, trt, ppt_chirps, lag) %>% 
  distinct() %>% 
  pivot_wider(values_from = "ppt_chirps",
              names_from = "trt") %>% 
  ungroup() %>% 
  rename(ppt_drought = Drought,
         ppt_control = Control)

# calculating drought severity
drt_sev <- ppt3 %>% 
  mutate(drt_sev = (ppt_drought - MAP)/MAP,
         MAP = MAP)

# values to use
drt_sev



# hard coding a dataframe based on the values in
# drt_sev (in case that easier to just insert into existing code)

if (FALSE) {
  drt_sev <- tibble(site_code = "falls.au", 
                    year = c(2017, 2018, 2019, 
                             2017, 2018, 2019),
                    lag = c("365-0days", "365-0days", "365-0days", 
                            "730-365days", "730-365days", "730-365days"), 
                    ppt_control = c(1643.60153436661, 1338.52202033997, 1127.57486183195, 
                                    1247.40427708626, 1719.32654047012, 1331.11475610733), 
                    ppt_drought = c(1629.89029779434, 803.11321220398, 676.544917099173, 
                                    1247.40427708626, 1705.61530389786, 798.668853664398), 
                    drt_sev = c(0.226853467943901, -0.395479419150243, -0.490750095950525, 
                                -0.0610519828590129, 0.283853308039823, -0.398824783371658), 
                    MAP = c(1328.51260593158)) 
  
}

