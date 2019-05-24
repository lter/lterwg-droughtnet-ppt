## Drought Sensitivity vs ppt_percentile

# this code is using preliminary drought sensitivity data
# and combining with control and drought precipitation data
# (mm and percentile) for the corresponding years 

# goal of this code is to compare drought sensitivity to 
# different metrics of how strong a drought was actually
# imposed in the given year. 

# this script started by martin holdrege (mcholdre@syr.edu) on 5/24/19

path <- c("C:\\Users\\grad\\Dropbox\\IDE Meeting_May2019")


library(tidyverse)

# script that preliminarily parses full biomasses to get 
# drought sensitivity metric 3:
source(file.path(path, "R Scripts\\Drought_Sensitivity.R"))
head(data_comb_yr) # the dataframe of interest


# ppt data ----------------------------------------------------------------

# data on drought/control precip during treatment year (i.e the 12 months prior to biomass harves)

site_ppt2 <- read.csv(file.path(path, "IDE Site Info\\Site-year precipitation by treatment_percentile_added.csv"), as.is = TRUE)

# parse_ppt2 data

# changing data frame so that there are two columns
# one for drt percentile one for control percentile
# on row for each site/year
site_perc_wide <- site_ppt2 %>% 
  dplyr::select(-ppt) %>% 
  nest(trt, percentile, .key = "data") %>% 
  mutate(data = map(data, spread, key = "trt", value = "percentile")) %>% 
  unnest() %>% 
  rename(Ctrl_percentile = Ctrl, Drt_percentile = Drt)

# making two columns (drt/control) for annual precip
site_ppt_wide <- site_ppt2 %>% 
  dplyr::select(-percentile) %>% 
  nest(trt, ppt, .key = "data") %>% 
  mutate(data = map(data, spread, key = "trt", value = "ppt")) %>% 
  unnest() %>% 
  rename(Ctrl_ppt = Ctrl, Drt_ppt = Drt)

# both annual precip and percentile in one dataframe:
site_wide <- full_join(site_perc_wide, site_ppt_wide,
                       by = c("site_code", "trt_year"))


# prepare data for merge with ppt data ------------------------------------

# days 30-100 and 100-365 are considered year 1 in ppt data
unique(data_comb_yr$days)
days2trt_yr <- c("30-365" = 1, "year 2" = 2) 

comb_yr2 <- data_comb_yr %>% 
  filter(days %in% c("30-365", "year 2")) %>% 
  mutate(site_code = as.character(site_code),
         trt_year = days2trt_yr[days],# new trt_year column
         trt_year = as.integer(trt_year)) 


# merge data --------------------------------------------------------------

comb_yr3 <- comb_yr2 %>% 
    left_join(site_wide, by = c("site_code", "trt_year")) 


# calculate precip (drought/treatment) metrics --------------------------------

comb_yr4 <- comb_yr3 %>% 
  mutate(ppt_diff = Ctrl_ppt - Drt_ppt, # difference in precip (mm)
         perc_ppt_reduct = ppt_diff/Ctrl_ppt*100, # actual percent reduction of ppt
         percentile_diff = Ctrl_percentile - Drt_percentile, # diff in percentiles
         perc_percentile_reduct = percentile_diff/Ctrl_percentile*100)
         


# figures -----------------------------------------------------------------

# biomass sensitivity vs precip metrics

# base of following plots
g1 <- ggplot(comb_yr4) +
  facet_wrap(~days) +
  labs(caption = paste("figure created in 'biomass-sensitivity_vs_ppt_reduction.r'",
                       "script, on", lubridate::today()),
       y = "Mean Sensitivity (metric 3)",
       subtitle = "1st and 2nd treatment year (precip data is from 12 months prior to harvest)") +
  theme_classic() +
  theme(plot.title = element_text(size = 13))

# function to following plots
point_smooth <- function(x, y = "mean_DS3"){
  list(geom_point(aes_string(x = x, y = y)),
       geom_smooth(aes_string(x = x, y = y), method = "lm", se = FALSE)
       )
}

pdf(file.path(path, "IDE Site Info/Plots/sensitivity_vs_ppt_reduction.pdf"),
    height = 5, width = 8)
g1 + 
  point_smooth(x = "ppt_diff") +
  labs(x = "precipitation reduction (mm) (ctrl - drt)",
       title = "Precipitation Reduction")

g1 + 
  point_smooth(x = "perc_ppt_reduct") +
  labs(x = "% precipitation reduction [(ctrl -drt)/ctrl]",
       title = "% Precipitation Reduction")

g1 + 
  point_smooth(x = "percentile_diff") +
  labs(x = "percentile difference (ctrl percentile - drt percentile)",
       title = "Percentile Difference")

g1 + 
  point_smooth(x = "perc_percentile_reduct") +
  labs(x = "% percentile reduction (percentile difference/ctrl percentile)",
       title = "% Percentile Reduction")

dev.off()

