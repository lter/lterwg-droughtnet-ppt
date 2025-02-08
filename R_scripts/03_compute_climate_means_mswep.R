# Purpose: Compute mean precipitation climate metrics for all sites
# using the mswep dataset
# this is an offshoot of the 03_compute_climate_means_v2.R
# script, except it has fewer data dependencies (just requires the 
# extracted daily mswep ppt data file)

# Author: Martin Holdrege

# Date started Jan 31, 2025


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
# calculating this as a 30 year normals
# (comporable to time-period used by NOAA etc for calculating normals)
# https://www.ncei.noaa.gov/access/us-climate-normals/
min_date = "1991-01-01" 
max_date = "2020-12-31" # dates are inclusive
output_figs <- FALSE # create and save figures?

# excluding 1mm or less events for precipt intensity/variability calculations
cutoff <- 1 

# read in data ------------------------------------------------------------

# * mswep -------------------------------------------------------------------

# file created in 02_mswep_extract_daily_ppt.R
p_mswep <- newest_file_path(
  path = file.path(path,'IDE/data_raw/climate/mswep/'),
  file_regex = 'mswep_ppt_daily_all-sites_\\d{4}-\\d+-\\d+.csv' 
)

mswep1 <- read_csv(p_mswep, show_col_types = FALSE)


# Monthly average data -------------------------------------
# calculating seasonality metrics

# * mswep ------------------------------------------------------------------


mswep2 <- mswep1 %>% 
  filter( date >= min_date,
          date <= max_date) 

ann_by_year <- mswep2 %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(site_code, year) %>% 
  summarize(ppt_max_event = max(precip),
            ppt_mean_event = mean(precip[precip > cutoff]),
            # number of biggest event days in which half of precip fell
            days_half_ppt = days_half_ppt(precip),
            # measure of variability in event sizes (similar to CV)
            daily_ppt_d = d_variability(precip, cutoff = 1),
            # number of wet days per year
            n_wet_days = n_wet_days(precip, cutoff = cutoff),
            # average length of precip free periods
            avg_dryspell_length = avg_dryspell_length(precip, cutoff = cutoff),
            # size of the 95th percentile event size (on days with precip)
            ppt_95th_percentile_size = ppt_percentile_size(precip, prob = 0.95,
                                                           cutoff = cutoff),
            ap = sum(precip), # annual precipitation
            .groups = 'drop'
  ) 

ann0 <- ann_by_year%>% 
  select(-ap) %>% 
  group_by(site_code) %>% 
  select(-year) %>% 
  summarize(across(where(is.numeric), .fns = mean),
            .groups = 'drop')


ann1 <- ppt_mean_annual(mswep1, min_date = min_date,
                        max_date = max_date) %>% 
  # not including dataperiod here b/ also including worldclim data
  # below which isn't the same period
  select(-data_period)

ann2 <- left_join(ann0, ann1, by = "site_code")


# calculate interannual correlations --------------------------------------
# correlations over time between annual ppt metrics

sitewise_cors1 <- ann_by_year %>% 
  select(site_code, year, ap, ppt_mean_event, n_wet_days,
         avg_dryspell_length) %>% 
  group_by(site_code) %>% 
  summarise(
    ap_vs_ppt_mean_event = cor(ap, ppt_mean_event),
    ap_vs_n_wet_days = cor(ap, n_wet_days),
    ap_vs_avg_dryspell_length = cor(ap, avg_dryspell_length),
    ppt_mean_event_vs_n_wet_days = cor(ppt_mean_event, n_wet_days),
    ppt_mean_event_vs_avg_dryspell_length = cor(ppt_mean_event, avg_dryspell_length),
    n_wet_days_vs_avg_dryspell_length = cor(n_wet_days, avg_dryspell_length),
  )

if (output_figs) {
  
  g <- sitewise_cors1 %>% 
    pivot_longer(cols = -site_code) %>% 
    ggplot(aes(x = value)) +
    geom_histogram() +
    facet_wrap(~name) +
    labs(x = 'Temporal correlation',
         subtitle = 'Correlations between annual precipitation (ap) and intra-annual precipitation variability metrics',
         caption = 'Correlations calculated for each site using MSWEP data') +
    theme_bw()
  
  png(file.path(path, "IDE/figures/climate/mswep_temporal-correlations_hist.png"),
                 units = 'in', width = 10, height = 6, res = 600)
  g
  dev.off()
}

# create metadata file ----------------------------------------------------

metadata <- c(
  "This is a description file for the following two files:\n",
  "IDE/data_processed/climate/mswep_mean_annual_by_site.csv \n",
  "Which are created by 03_compute_climate_means_mswep.R which can be found on github",
  paste('the values are metrics calculated from daily MSWEP precipitation data for',
        'period', lubridate::year(min_date), '-', lubridate::year(max_date)),
  'The metrics that rely on calculating the number of events (e.g. mean event size', 
  'and n_wet days)', 
  paste('defined "events" as days with > ', cutoff, 'mm of precipitation.'),
  "",
  "Description of:",
  'ppt_max_event -- mean size of the largest precipitation event in a year',
  'ppt_mean_event -- mean ppt event size',
  'days_half_ppt -- the mean number of days in year that it takes to receive half
  of annual precipitation',
  'daily_ppt_d -- D of daily precipitation, similar to CV (an intra-annual ppt variability metric)',
  'n_wet_days -- number of precipitation events per year',
  "MAP -- mean annual precipitation (mm/yr)",
  'avg_dryspell_length -- mean number of days between wet days',
  'ppt_95th_percentile_size -- the mean size of the 95th percentile event size
  (when only considering wet days)',
  "cv_ppt_intra -- intra-annual precip variability calculated ",
  "as the cv of monthly ppt in a given year, then averaged across years",
  "cv_ppt_inter --interannual precipitation variability (ie cv of annual precipitation totals)",
  "yearly_ppt_d -- D of annual precipitation (interannual precipitation variability metric)",
  "seasonality_index -- seasonality index (SI) as derived by Walsh and Lawler (1981)",
  "culculated as sum(abs(x - Ri/12))/Ri, where x is monthly ppt, and Ri", 
  "annual ppt,",
  "mean of seasonality index was calculated across years."
)

write_lines(metadata,  file.path(
  path, "IDE/data_processed/climate",
  "mswep_mean_annual_by_site_metadata.txt" 
))


# write output ------------------------------------------------------------

write_csv(ann2, file.path(path, "IDE/data_processed/climate/mswep_mean_annual_by_site.csv"))
write_csv(sitewise_cors1, file.path(path, "IDE/data_processed/climate/mswep_temporal-correlations_by_site.csv"))

