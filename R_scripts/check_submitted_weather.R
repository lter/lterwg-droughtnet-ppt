# check submitted weather station data for IDE sites

# this script is designed to check the compiled submitted weather data
# the data is compiled in process_submitted_weather.R

# this is file is meant count missing values, check for unreasonabl values, etc. 
# for starters just checking the precip (not temp) data

# script started 11/27/19


# packages etc ------------------------------------------------------------

library(tidyverse)
path_oct <- "E:/Dropbox/IDE Meeting_Oct2019"


# read in submitted weather -----------------------------------------------

# grabbing most recent submitted daily weather file
dly_wthr_path <- list.files(
  file.path(path_oct, "data/precip"),
  pattern = "submitted_daily_weather_\\d{4}-\\d{2}-\\d{2}.csv", 
  full.names = TRUE) %>% 
  sort(decreasing = TRUE) %>% 
  .[1]

dly_wthr_path

wthr1 <- read_csv(dly_wthr_path,
                  col_types = "cDdddcdcc")



# counting NAs by site ----------------------------------------------------

num_nas <- wthr1 %>% 
  group_by(site_code, site_name) %>% 
  summarise(num_na = sum(is.na(precip)),
            n = n(),
            # time span in case not all dates provides
            time_span = as.numeric(max(date) - min(date)) + 1,
            prop_na = num_na/n) %>% 
  arrange(desc(prop_na))

# biddulph.ca, kernb.ca, and kernnu.ca don't have precip data
# (said so in submitting email also--issue downloading it)

num_nas

# sites with more that 5% missing data
num_nas %>% 
  filter(prop_na > 0.05) %>% 
  pull(site_code)
num_nas %>% 
  filter(n != time_span)

arrange(num_nas, n) # sites with fewest observations

# negative precip ---------------------------------------------------------

hist(wthr1$precip)

wthr1 %>% 
  filter(precip < 0) %>% 
  pull(precip) %>% 
  unique()

# sites with negative precip values
wthr1 %>% 
  group_by(site_code, site_name) %>% 
  summarize(num_neg = sum((precip < 0))) %>% 
  filter(num_neg > 0)

# branjberg--a number of -0.1 values
# but they don't seem to overlap with experiment dates historical data also given
wthr1 %>% 
  filter(site_code == "brandjberg.dk" & precip < 0) %>% 
  pull(precip) %>% 
  unique()

wthr2 <- wthr1 %>% 
  mutate(precip = ifelse(precip < 0, NA, precip))

hist(wthr2$precip)


# large positive precip ---------------------------------------------------

wthr2 %>% 
  filter(precip > 100) %>% 
  pull(precip) %>% 
  unique() %>% 
  sort()

# histograms of sites with suspiciously high precip values

sites_high <- wthr2 %>% 
  filter(precip > 200) %>% 
  pull(site_code) %>% 
  unique()

# histograms of precip > 0, for sites that have recorded precip > 200
for (code in sites_high) {
  print(
  hist(with(wthr2, precip[site_code == code & precip > 0]),
       main = code,
       breaks = 50)
  )
}

# I contacted Sally--she confirmed these are problematic values and is looking into it. 

# unclear what units these values are in
wthr2 %>% 
  filter(site_code == "yarradrt.au") %>% 
  group_by(lubridate::year(date)) %>% 
  summarize(AP = sum(precip, na.rm = TRUE),
            non_na = sum(!is.na(precip)))

# STOP--removing yarrardrt for now until better data is submitted. 

wthr3 <- wthr2
wthr3 <- wthr3 %>% 
  filter(site_code != "yarradrt.au")


# saving CSV --------------------------------------------------------------

write_csv(
  wthr3,
  file.path(path_oct, 
            "data/precip/submitted_daily_weather_bad_vals_removed_2019-12-02.csv")
  )
  