# check submitted weather station data for IDE sites

# this script is designed to check the compiled submitted weather data
# the data is compiled in process_submitted_weather.R

# this is file is meant count missing values, check for unreasonable values, etc. 
# for starters just checking the precip (not temp) data
# and fix obvious data issues where possible. 

# script started 11/27/19


# packages etc ------------------------------------------------------------

library(tidyverse, warn.conflicts = FALSE)
library(lubridate)
source("R_scripts/functions.R")
path_oct <- "~/Dropbox/IDE Meeting_Oct2019"
path <- "~/Dropbox"

# read in submitted weather -----------------------------------------------

# grabbing most recent submitted daily weather file
p1 <- newest_file_path(
  path = file.path(path_oct, "data/precip"),
  file_regex = "submitted_daily_weather_\\d{4}-\\d{2}-\\d{2}.csv")
p1

wthr1 <- read_csv(p1,col_types = "cDdddcdcc")

# so can get reported MAP (grabbing most recent file)
p2 <- file.path(path, "IDE MS_Single year extreme/Data/Site_Elev-Disturb.csv")

site_data <- read.csv(p2, as.is = TRUE, na.strings = c("","<NA>", "NA"))

site_map <- site_data %>% 
  select(site_code, precip) %>% 
  rename(map = precip)

# counting NAs by site ----------------------------------------------------

num_nas <- wthr1 %>% 
  group_by(site_code, site_name) %>% 
  summarise(num_na = sum(is.na(precip)),
            n = n(),
            # time span in case not all dates provides
            time_span = as.numeric(max(date) - min(date)) + 1,
            prop_na = num_na/n,
            num_dates_missing = time_span-n) %>% 
  arrange(desc(prop_na))


num_nas %>% 
  arrange(desc(num_dates_missing)) %>% 
  filter(num_dates_missing > 0) %>% 
  print(n = 40)
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

# sanity check against provided data
wthr1 %>% 
  filter(site_code == 'chilcasdrt.ar') %>% 
  mutate(month = month(date),
         year = year(date)) %>% 
  group_by(month, year) %>% 
  summarise(precip = sum(precip)) %>% 
  arrange(year, month) 

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

# initial yarramundi file had bad data--this new one looks good

# unclear what units these values are in
wthr2 %>% 
  filter(site_code == "yarradrt.au") %>% 
  group_by(lubridate::year(date)) %>% 
  summarize(AP = sum(precip, na.rm = TRUE),
            non_na = sum(!is.na(precip)))

wthr3 <- wthr2



# annual precip reasonable? -----------------------------------------------

annual <- wthr3 %>% 
  mutate(year = year(date)) %>% 
  group_by(year, site_code, site_name) %>% 
  summarise(n_vals = sum(!is.na(precip)),
            precip = sum(precip, na.rm = TRUE)) %>% 
  left_join(site_map, by = "site_code")

# cowichan has high annual precip (1812-on qiuck google search)--so it is reasonable
annual %>% 
  mutate(obs_v_map = precip/map) %>% 
  filter((obs_v_map > 2 | obs_v_map < 0.3) & n_vals > 300) %>% 
  arrange(site_code) %>% 
  print(n = 40)

annual

# annual %>% 
#   #filter(n_vals > 300) %>% 
#   arrange(site_code) 


# converting units --------------------------------------------------------

# sites suspected to be in inches--need to confirm

inches <- c("thompson.us", "oklah.us", "bfl.us", "slp.us")

#cm <- c("dang.cn") # originally I thought this site might be in cm, doesn't
# look like it based on other years

wthr4 <- wthr3 %>% 
  mutate(precip = ifelse(site_code %in% inches,
                         precip*25.4,
                         precip))


wthr4 %>% 
  mutate(year = year(date)) %>% 
  group_by(year, site_code, site_name) %>% 
  summarise(n_vals = sum(!is.na(precip)),
            precip = sum(precip, na.rm = TRUE)) %>% 
  left_join(site_map, by = "site_code") %>% 
  mutate(obs_v_map = precip/map) %>% 
  filter((obs_v_map > 2 | obs_v_map < 0.3) & n_vals > 300) %>% 
  arrange(site_code) %>% 
  print(n = 40)


# saving CSV --------------------------------------------------------------

dest <- file.path(
  path_oct,
  "data/precip/submitted_daily_weather_bad_vals_removed_2021-03-02.csv")

write_csv(wthr4, dest)
  