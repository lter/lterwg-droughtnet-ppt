#############################################################################################
###### Calculate precipitation reduction based on daily precip and shelter installation  ####
#############################################################################################

# packages etc ------------------------------------------------------------

library(tidyverse)
library(stringr)
library(lubridate)
source("R_scripts/functions.R")
source("R_scripts/dropbox_path.R") # where path to dropbox should be set

path_oct <- file.path(path, 'IDE Meeting_Oct2019')
path_ms <-  file.path(path, "IDE MS_Single year extreme")


# user defined parameters --------------------------------------------------

# the number of days before a given biomass date to start summing
# the precip, ie. 365 would mean you are calculating precip for 365-0 
# days before biomass date, 730 would mean 730 to 365 days before
# biomass treatment (should be a multiple of 365). 

days_before <-     365 #730 #1095 #1460 #

date_string <- "2023-01-02" # for use in output file names
days_string <- paste0("_",days_before, "-", days_before - 365, "days_")
# reading in precip data -----------------------------------------------------

# GHCN data--getting newest file based on file name
p1 <- newest_file_path(
  path = file.path(path_oct,'data/precip'),
  file_regex = 'GHCN_daily_precip_\\d{4}-\\d+-\\d+.csv' 
)
p1
precip <- read.csv(p1, as.is = TRUE)

# submitted data
# grabbing most recent submitted daily weather file
dly_wthr_path <- newest_file_path(
  path = file.path(path_oct, "data/precip"),
  file_regex =  "submitted_daily_weather_WC_supplemented_\\d{4}-\\d{2}-\\d{2}.csv" 
)
dly_wthr_path

wthr1 <- read_csv(dly_wthr_path, show_col_types = FALSE)
wthr1$X1 <- NULL

# sites where we have only GHCN data
precip$site_code[!precip$site_code %in% wthr1$site_code] %>% unique()


# * reading in CHIRPS datasets --------------------------------------------
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

if(!all(missing_names == 'noor.ir')) {
  stop('unexpected sites have missing values')
}

chirps2 <- bind_rows(chirps1[!is_missing]) %>% 
  rename(ppt = precipitation)

if (sum(is.na(chirps2$ppt)) > 0) {
  stop('some sites still have missing values')
}


# *reading in mswep dataset -----------------------------------------------
# this data is from the mswep gridded ppt product (http://www.gloh2o.org/mswep/). 
# this data is compiled in the "R_scripts/02_mswep_extract_daily_ppt.R" script
p_mswep <- newest_file_path(
  path = file.path(path,'IDE/data_raw/climate/mswep/'),
  file_regex = 'mswep_ppt_daily_all-sites_\\d{4}-\\d+-\\d+.csv' 
)

mswep1 <- read_csv(p_mswep, show_col_types = FALSE)

mswep2 <- mswep1 %>% 
  rename(ppt = precip)

# reading in site/anpp date data ---------------------------------------

siteDrt_A <- read.csv(file.path(path_ms, "Data/Site_Elev-Disturb.csv"), 
                      as.is = TRUE, na.strings = c("","<NA>", "NA")) %>% 
  as_tibble()

p6 <- newest_file_path(
  file.path(path_ms, "Data"),
  "full_biomass_\\d+-\\d+-\\d{4}.csv",
  mdy = TRUE)
p6
bio1 <- read.csv(p6, as.is = TRUE)

# survey info
p7 <- newest_file_path(
  file.path(path_ms, "Data"),
  "Survey_\\d+-\\d+-\\d{4}.csv",
  mdy = TRUE)
p7
survey1 <- read.csv(p7, as.is = TRUE)


# site reported annual ppt ------------------------------------------------
# sites that didn't provide precip data, but for which annual (calendar year)
# data was given.

# annual ppt
appt1 <- read_csv(file.path(path_ms, "Data/precip/SitesMissingPrecip_ReportedMAP.csv"))

# look at next line for explanation of parsing failurs
appt2 <- appt1 %>% 
  mutate(annual_ppt = as.numeric(annual_ppt)) 

# values that don't parse
no_parse <- appt1$annual_ppt[is.na(appt2$annual_ppt) & !is.na(appt1$annual_ppt)] %>% 
  unique()

# shouldn't be any numbers in unparsed values
stopifnot(!str_detect(no_parse, "\\d+")) 



# match exactly 4 digits (i.e. note made about the year)
# in all but one case these notes are about ppt not actually being for 
# the entire year
four_dig <- str_detect(appt2$note_climate, "(?<!\\d)\\d{4}(?!\\d+)") 

appt2$annual_ppt[four_dig & 
                   # excluding one site where note isn't about incomplete ppt
                   !str_detect(appt2$note_climate, "change that occurred in 2018") 
                 & !is.na(appt2$note_climate)] <- NA

appt2 <- appt2 %>% 
  # doing this b/ of some
  group_by(site_code, year) %>% 
  summarize(annual_ppt = mean(annual_ppt))
# extract biomass date ----------------------------------------------------

bio2 <- bio1 %>% 
  group_by(site_code, trt, block, plot, subplot, year) %>% 
  summarize(first_treatment_date = min(first_treatment_date),
            biomass_date = max(biomass_date)) %>% 
  # add in survey info
  left_join(survey1, by = "site_code")%>% 
  # filtering out brandjberg b/ it isn't used.
  # Also it has two treatment start dates which is causing problems
  # in 06_calculate_cdf.R
  filter(site_code != "brandjberg.dk") 


# extract drought treatment -----------------------------------------------

siteDrt_B <- siteDrt_A %>% 
  dplyr::select(site_code, drought_trt) %>% 
  mutate(drought_trt = str_replace(drought_trt, "%", ""),
         drought_trt = as.numeric(drought_trt)/100)

# Look at parsing failures
siteDrt_A %>% 
  filter(is.na(siteDrt_B$drought_trt)) %>% 
  select(site_code, drought_trt)

# b/ didn't parse above
siteDrt_B$drought_trt[siteDrt_B$site_code == "elizwood.us"] <- 0.5

# No drought treatment provided--update this when available
siteDrt_B$drought_trt[siteDrt_B$site_code == "hoelstein.ch"]

# No drought treatment provided--update this when available
siteDrt_B$drought_trt[siteDrt_B$site_code == "tovetorp.se"]
# 
siteDrt_A %>% 
  filter(is.na(siteDrt_B$drought_trt)) %>% 
  select(site_code, drought_trt)

# We're only using control data for brandjberg.dk and stubai.at
# so ok if they're NA
if(any(is.na(siteDrt_B$drought_trt) & 
       ! siteDrt_B$site_code %in% c("brandjberg.dk","stubai.at"))) {
  warning("some drought_trt values missing")
}

# cleaning anpp file -------------------------------------------------------

anpp2 <- bio2

is_empty <- function(x) {
  out <- is.na(x) | x == ""
  out
}

# if no indication given that shelter removed then i'm assuming it wasn't
anpp2$X365day.trt[is_empty(anpp2$X365day.trt) & 
                    is_empty(anpp2$IfNot365.WhenShelterRemove) &
                    is_empty(anpp2$IfNot365.WhenShelterSet)] <- "Yes"

is_mdy <- str_detect(anpp2$first_treatment_date, "\\d{1,2}/\\d{1,2}/\\d{4}")         

# converting dates to characters so subseting type issues don't arise
anpp2$dummy_date <- NA_character_ 
anpp2$dummy_date[is_mdy] <- mdy(anpp2$first_treatment_date[is_mdy]) %>% 
  as.character()
anpp2$dummy_date[!is_mdy] <- ymd(anpp2$first_treatment_date[!is_mdy]) %>% 
  as.character

# in the end results will be joined back in
anpp2 <- anpp2 %>% 
  select(-first_treatment_date) %>% 
  rename(first_treatment_date = dummy_date) %>% 
  mutate(biomass_date = ifelse(
    str_detect(biomass_date, "\\d{1,2}/\\d{1,2}/\\d{4}"),
    as.character(mdy(biomass_date)),
    ifelse(str_detect(biomass_date, "\\d{4}/\\d{1,2}/\\d{1,2}|\\d{4}-\\d{1,2}-\\d{1,2}"),
           as.character(ymd(biomass_date)),
           # other date pattern (e.g. just year)
           NA_character_)),
    biomass_date = as.Date(biomass_date),
    first_treatment_date = as.Date(first_treatment_date)) 

# check that date parsing failurs were really justified (ie year only given)
bad_dates <- bio2[is.na(anpp2$biomass_date), ]$biomass_date

if(!all(str_detect(bad_dates, "^\\d{4}$"))) {
  warning("unjustified date parsing issues")
  print(bad_dates)
}


# sites that don't have "Control"
no_control <- anpp2 %>% 
  group_by(site_code) %>% 
  nest() %>% 
  mutate(control = map_lgl(data, function(df) {
    "Control" %in% df$trt
  })
  ) %>% 
  filter(!control) %>% 
  pull(site_code) %>% 
  unique()

# if site doesn't have "control" use control_infrastructure instead
# run no_control again to test this was successful
anpp2 <- anpp2 %>% 
  mutate(trt = ifelse(site_code %in% no_control & trt == "Control_Infrastructure",
                      "Control",
                      trt))

# adding in drought vals,
# for now only using drought and control--for first paper
anpp3 <- siteDrt_B %>% 
  right_join(anpp2, by = "site_code") %>% 
  as_tibble() %>%
  #mutate(trt = str_replace(trt, "Control_Infrastructure", "Control"))
  filter(trt %in% c("Drought", "Control")) %>% 
  # key columns and data columns needed by calc_yearly_precip function
  select(site_code, block, plot, subplot, year, trt, biomass_date, first_treatment_date, 
         X365day.trt, IfNot365.WhenShelterRemove, IfNot365.WhenShelterSet,
         drought_trt) %>% 
  # renaming for function
  rename(bioDat = biomass_date,
         trtDat = first_treatment_date)


# on/off dates ------------------------------------------------------------

# sand.us dates emailed by Kate on 6/6/2022

# these sites just provided text in the set/remove fields
on_off_dates <- tibble(site_code = 
                         c("cedarsav.us", "cedarsav.us", "cedarsav.us", 
                           "cedartrait.us", "cedartrait.us", "cedartrait.us",
                           'sand.us', 'sand.us', 'sand.us'),
       on = c("2017-04-27", "2018-05-05", "2019-04-30", 
              "2017-04-27", "2018-05-05", "2019-05-2",
              '2020-6-22', '2021-5-27', '2022-4-30'),
       off = c("2017-09-21", "2018-09-25", "2019-09-25",
               "2017-09-20","2018-09-11", "2019-09-24",
               # Note the last date (2022-10-1) is my best
               # guess based on past years (at the time of 
               # writing this date is in the future--the place
               # holder is so that code doesn't break donw)
               '2020-10-3', '2021-9-26', '2022-10-1'),
       year = year(on))


# cedarsav.us has notes in place of dates in 2016 (pre trmt)
anpp3 <- anpp3 %>% 
  left_join(on_off_dates, by = c("site_code", "year")) %>% 
  mutate(
    IfNot365.WhenShelterSet = ifelse(is.na(on), IfNot365.WhenShelterSet, on),
    IfNot365.WhenShelterRemove = ifelse(is.na(off), IfNot365.WhenShelterRemove, off),
    # remove/set dates aren't meaningful before the first treatment date
    IfNot365.WhenShelterSet = ifelse(bioDat < trtDat, NA, IfNot365.WhenShelterSet),
    IfNot365.WhenShelterRemove = ifelse(bioDat < trtDat, NA, IfNot365.WhenShelterRemove)) %>% 
  select(-on, -off)

# reformatting some dates so that they can be used by the function 
set_date <- str_detect(anpp3$IfNot365.WhenShelterSet, "\\d{4}-\\d+-\\d+")
rem_date <- str_detect(anpp3$IfNot365.WhenShelterRemove, "\\d{4}-\\d+-\\d+")

set_date[is.na(set_date)] <- FALSE
rem_date[is.na(rem_date)] <- FALSE

anpp3$IfNot365.WhenShelterSet[set_date] <- 
  anpp3$IfNot365.WhenShelterSet[set_date] %>% 
  ymd() %>% 
  paste(day(.), month(., label = TRUE)) %>% 
  str_extract("\\d+\\s[A-z]+$")

anpp3$IfNot365.WhenShelterRemove[rem_date] <- 
  anpp3$IfNot365.WhenShelterRemove[rem_date] %>% 
  ymd() %>% 
  paste(day(.), month(., label = TRUE)) %>% 
  str_extract("\\d+\\s[A-z]+$")



# ghcn data ---------------------------------------------------------------

precip1 <- precip %>% 
  mutate(date = as.Date(date),
         # change this if interpolate ghcn data (ie this is number of interpolated values)
         # (needed by function below)
         wc = NA) %>% 
  as_tibble()


# remove duplicates for  calc ---------------------------------------------

# removing duplicate date/site combos so runs quicker

sites1 <- anpp3 %>% 
  select(site_code, year, bioDat, trtDat, X365day.trt,  matches("IfNot365"),
         drought_trt) %>% 
  .[!duplicated(.), ]

sites2a <- sites1 %>% 
  mutate(ppt_drought = NA_real_,
         ppt_ambient = NA_real_,
         ppt_num_NA = NA_real_, 
         num_drought_days = NA_real_,
         ppt_num_wc_interp = NA_real_,
         # whether the biomass data was in the original data 
         # (i.e. a date biomass was actually harvested on)
         fake_bioDat = FALSE)


# add in fake biomass dates to get earlier ppt ----------------------------
# putting in biomass dates that are 1, 2, 3, 4, years before the
# earliest provided biomass date so that the code calculates ppt
# for previous years (note--in this case the data being used will be mostly
# chirps b/ most sites didn't provide that much data)

# data from the first year available
sites_yr1 <- sites2a %>% 
  group_by(site_code) %>% 
  filter(year == min(year)) %>% 
  # choosing to use the last biomass date of the year as the 
  # reference date to use down the road
  filter(bioDat == max(bioDat))

# testing for duplicates
test <- sites_yr1[, c("site_code", "year")][
  duplicated(sites_yr1[, c("site_code", "year")]), ]
if(nrow(test) > 0) {
  stop('duplicate rows present, this can occur if for example there are
       multiple dates of first treatment')
}

yrs_before <- 1:4
# creating fake biomass that go 1-4 years before first actual year
pre_years <- map_dfr(yrs_before, function(x) {
  out <- sites_yr1
  out$bioDat <- out$bioDat - years(x)
  out$year <- out$year -x
  out$fake_bioDat <- TRUE
  stopifnot(is.Date(out$bioDat))
  out
})

sites2b <- bind_rows(pre_years, sites2a)


# fill in missing years ---------------------------------------------------
# Note--an issue is when sites provided biomass dates for say 2016 and 2018,
# but not for 2017, this code creates a fake biomass date for 2017,
# so ppt will be extracted for that year. 
# Importantly the fake biomass date is the maximum of the biomass dates
# for that site the year before. Note--that choice may not always be optimal

# this data frame provides site, year, and the number of years
# missing prior to that year
n_prev_missing1 <- sites2b %>% 
  ungroup() %>% 
  select(site_code, year) %>% 
  arrange(site_code, year) %>% 
  filter(!duplicated(.)) %>% 
  group_by(site_code) %>% 
  mutate(n = n()) %>% 
  filter(n > 1) %>% 
  mutate(diffs_yr = diff_na(year)) %>% 
  filter(diffs_yr > 1) %>% 
  # this doesn't currently account for the fact
  # that could have multiple consecutive yrs missing
  mutate(n_prev_missing = diffs_yr - 1) %>% 
  select(-n, -diffs_yr)

n_prev_missing2 <- n_prev_missing1 %>% 
  left_join(sites2b, by = c("site_code", "year")) %>% 
  group_by(site_code, year) %>% 
  filter(bioDat == max(bioDat)) 

check <- n_prev_missing2 %>% 
  summarise(n = n()) %>% 
  filter(n > 1)
if(nrow(check) > 0) {
  stop("Duplicates present in n_prev_missing2")
}

# creates rows and biomass dates for the missing years
imputed_years <- n_prev_missing2 %>% 
  group_by(site_code, year) %>% 
  nest() %>% 
  mutate(data2 = pmap(list(site_code, year, data), function(site, yr, df) {
    # repeat once for each missing years
    out <- df[rep(1, df$n_prev_missing), ]  
    out$year <-  yr - 1:df$n_prev_missing # calculating previous years
    out$bioDat <- out$bioDat - years(1:df$n_prev_missing) # fake biomass dates
    out$fake_bioDat <- TRUE
    out$site_code <- site
    out
  })) %>% 
  pull(data2) %>% 
  bind_rows() %>% 
  select(site_code, year, everything(), -n_prev_missing)

sites2 <- bind_rows(sites2b, imputed_years)

# yearly control/drt precip -----------------------------------------------


# * ghcn ------------------------------------------------------------------

# first calculating using ghcn data
sites2_forghcn <- sites2 %>% 
  filter(site_code %in% precip1$site_code)

#options(warn=1) # print warnings as they occur
sites3_ghcn <- calc_yearly_precip(site_data = sites2_forghcn,
                                  precip_data = precip1,
                                  days_before = days_before)

# * submitted data --------------------------------------------------------

# calculating using submitted data
wthr2 <- wthr1 %>% 
  rename(ppt = precip)

# STOP: temporary fix! (year not date provided for bio date for pozos.ar) 
sites2_forsubmitted <- sites2 %>%
  filter(site_code %in% wthr2$site_code,
        !is.na(bioDat))

sites3_submitted <- calc_yearly_precip(site_data = sites2_forsubmitted,
                                       precip_data = wthr2,
                                       days_before = days_before)


# * chirps data -----------------------------------------------------------
sites2_forchirps <- sites2 %>% 
  filter(site_code %in% chirps2$site_code)

sites3_chirps <- calc_yearly_precip(site_data = sites2_forchirps,
                                  precip_data = chirps2,
                                  days_before = days_before)

sites4_chirps <- sites3_chirps 

# adding col name suffix's for joining
col_names <- names(sites3_chirps)
new_col_names <- ifelse(col_names %in% names(sites1),
                        col_names,
                        paste0(col_names, "_chirps"))
names(sites4_chirps) <- new_col_names


# * mswep -----------------------------------------------------------------
sites2_formswep <- sites2 %>% 
  filter(site_code %in% mswep2$site_code)

sites3_mswep <- calc_yearly_precip(site_data = sites2_formswep,
                                    precip_data = mswep2,
                                    days_before = days_before)

sites4_mswep <- sites3_mswep

# adding col name suffix's for joining
col_names <- names(sites3_chirps)
new_col_names <- ifelse(col_names %in% names(sites1),
                        col_names,
                        paste0(col_names, "_mswep"))
names(sites4_mswep) <- new_col_names

# combining ghcn and submitted results ------------------------------------

sites4 <- full_join(sites3_ghcn, sites3_submitted, 
                    by = c(names(sites1)),
                    suffix = c("_ghcn", "_sub")) %>% 
  full_join(sites4_chirps, by = names(sites1)) %>% 
  full_join(sites4_mswep, by = names(sites1)) %>% 
  rowwise() %>% 
  mutate(fake_bioDat = mean(c(fake_bioDat_sub, fake_bioDat_ghcn, 
                           fake_bioDat_chirps, fake_bioDat_mswep), na.rm = TRUE),
         fake_bioDat = as.logical(fake_bioDat)) %>% 
  ungroup() %>% 
  select(-matches("fake_bioDat_"))

sites5 <- sites4 %>% 
  select(-ppt_num_wc_interp_ghcn, -ppt_num_wc_interp_chirps,
         -ppt_num_wc_interp_mswep)

# merge back to main anpp file --------------------------------------------
anpp3$fake_bioDat <- FALSE
fake_years <- bind_rows(pre_years, imputed_years)
pre_years_ctrl <- fake_years[names(fake_years) %in% names(anpp3)] %>% 
  mutate(trt = 'Control')

pre_years_drt <- pre_years_ctrl %>% 
  mutate(trt = 'Drought')


# adding in fake bioDat rows for both control and drought trmt
anpp4 <- bind_rows(pre_years_ctrl, pre_years_drt, anpp3) 
  

sites6 <- anpp4 %>% 
  left_join(select(sites5, -fake_bioDat), 
            by = c("site_code", "year", "bioDat", "trtDat", 
                   "X365day.trt", "IfNot365.WhenShelterRemove", 
                   "IfNot365.WhenShelterSet", "drought_trt")) %>% 
  mutate(ppt_ghcn = ifelse(trt == "Control", ppt_ambient_ghcn, 
                           ppt_drought_ghcn),
         ppt_sub = ifelse(trt == "Control", ppt_ambient_sub, 
                           ppt_drought_sub),
         ppt_chirps = ifelse(trt == "Control", ppt_ambient_chirps, 
                            ppt_drought_chirps),
         ppt_mswep = ifelse(trt == "Control", ppt_ambient_mswep, 
                             ppt_drought_mswep)) 
# should be 0:
test <- sum(sites6$num_drought_days_ghcn != sites6$num_drought_days_sub, 
            na.rm = TRUE)
stopifnot(test == 0)


# use submitted data if it has fewer than 30 missing values, 
# else use GHCN data, if both have >30 missing values than using
# chirps data (if available) 
# Also don't use submitted data if more than 30 days were interpolated
sites7 <- sites6 %>% 
  arrange(site_code, year) %>% 
  mutate(num_drought_days = ifelse(is.na(num_drought_days_sub), 
                                   num_drought_days_ghcn, 
                                   num_drought_days_sub)) %>% 
  select(-num_drought_days_sub, -num_drought_days_ghcn, 
         -matches("_(drought)|(ambient)_"), num_drought_days) %>%
  rowwise() %>% 
  mutate(
    ppt = case_when(
      sum(c(ppt_num_NA_sub, ppt_num_wc_interp_sub), na.rm = TRUE) < 30 
      & !is.na(ppt_sub) ~ppt_sub,
      ppt_num_NA_ghcn < 30 & !is.na(ppt_ghcn) ~ ppt_ghcn,
      !is.na(ppt_chirps) & ppt_num_NA_chirps < 30 ~ ppt_chirps,
      !is.na(ppt_mswep) & ppt_num_NA_mswep < 30 ~ ppt_mswep),
    ppt_source = case_when(
      sum(c(ppt_num_NA_sub, ppt_num_wc_interp_sub), na.rm = TRUE) < 30 
      & !is.na(ppt_sub) ~ 'submitted',
      ppt_num_NA_ghcn < 30 & !is.na(ppt_ghcn) ~ 'ghcn',
      !is.na(ppt_chirps) & ppt_num_NA_chirps < 30 ~ "chirps",
      !is.na(ppt_mswep) & ppt_num_NA_mswep < 30 ~ "mswep")
  ) %>% 
  ungroup()

# * assign trt to fake_bioDat rows ----------------------------------------

test <- sites7 %>% 
  filter(fake_bioDat, bioDat > trtDat)
test

# * join in anpp2 ---------------------------------------------------------

# this code causing problems because of imperfect join
sites_full1 <- sites7 %>% 
  rename(biomass_date = bioDat,
         first_treatment_date = trtDat) %>% 
  left_join(anpp2, by = c("site_code", "block", "plot", "subplot", "year", "trt", 
                          "biomass_date", "first_treatment_date", "X365day.trt")) %>% 
  # solving issue of adjustments above causing join incompatibility with cedartrait
  mutate(IfNot365.WhenShelterRemove = ifelse(is.na(IfNot365.WhenShelterRemove.x),
                                             IfNot365.WhenShelterRemove.y,
                                             IfNot365.WhenShelterRemove.x),
         IfNot365.WhenShelterSet = ifelse(is.na(IfNot365.WhenShelterSet.x),
                                             IfNot365.WhenShelterSet.y,
                                             IfNot365.WhenShelterSet.x)) %>% 
  select(-matches("\\.(x|y)$")) 

test <- sites_full1 %>% 
  filter(is.na(fake_bioDat) & !is.na(ppt))
if(nrow(test) > 0) {
  stop("likely a join issue, missing fake_bioDat but ppt calculated")
}

# comparing ghcn & chirps to submitted data ----------------------------------------

theme_set(theme_classic())



pdf(file.path(path_oct,
              paste0("figures/precip/ghcn_vs_submitted_precip", days_string,
                      date_string, ".pdf")
             ))

# ghcn data for this site is bad
sites7 %>% 
  filter(ppt_sub == 0 | ppt_ghcn == 0) %>% 
  pull(site_code) %>% 
  unique()


df <- sites7 %>% 
  filter((ppt_num_NA_sub + ppt_num_wc_interp_sub) < 30 & ppt_num_NA_ghcn < 30,
         trt == "Control") %>% 
  group_by(site_code, year) %>% 
  mutate(ppt_sub = mean(ppt_sub),
         ppt_ghcn = mean(ppt_ghcn),
         ppt_chirps = mean(ppt_chirps)) 


g <- ggplot(df, aes(ppt_sub, ppt_ghcn)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(y = "365 day precip sum from GHCN (mm)",
       x = "365 day precip sum from submitted data (mm)")
print(g)

g <- ggplot(df, aes(ppt_sub, ppt_chirps)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(y = "365 day precip sum from chirps (mm)",
       x = "365 day precip sum from submitted data (mm)")
print(g)

g <- ggplot(df, aes(ppt_mswep, ppt_chirps)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(y = "365 day precip sum from chirps (mm)",
       x = "365 day precip sum from mswep data (mm)")
print(g)
dev.off()

# these are sites worth looking at b/ data sources give very different
# results
check <- df %>% 
  filter(ppt_sub/ppt_chirps >2)
# check

# adding in annual ppt ----------------------------------------------------
# if no daily data available using the site submitted calendar
# year precip. 

sites_full2 <- sites_full1 %>% 
  # calendar year that best describes prior precip
  mutate(cal_yr = ifelse(month(biomass_date, abbr = FALSE) > 7, year, year -1),
         # were treatments applied starting the begining of the calendar year
         cal_yr_post_trt = ifelse(cal_yr >= year(first_treatment_date) & (biomass_date - first_treatment_date) > 365 , TRUE, FALSE),
         # is the whole calendar year pre trmt? (or is the important part pre-trt)
         cal_yr_pre_trt = ifelse(cal_yr < year(first_treatment_date) | biomass_date < first_treatment_date, TRUE, FALSE)) %>% 
  left_join(appt2[ , c("site_code", "annual_ppt", "year")], 
            by = c("site_code", "cal_yr" = "year")) %>% 
  mutate(annual_ppt = ifelse(trt == "Control", annual_ppt, 
                             ifelse(cal_yr_pre_trt, annual_ppt,
                                    ifelse(cal_yr_post_trt & trt == "Drought" & (X365day.trt == "Yes"|is.na(X365day.trt)), annual_ppt*(1 - drought_trt),
                                           NA_real_))),
         annual_ppt_used = ifelse(is.na(ppt) & !is.na(annual_ppt), TRUE, FALSE),
         ppt = ifelse(annual_ppt_used, annual_ppt, ppt),
  )
  
test <- sites_full2 %>% 
  group_by(site_code, trt, plot, block, year) %>% 
  filter(biomass_date == max(biomass_date)) %>% 
  summarize(n = n()) %>% 
  filter(n > 1) %>% 
  pull(site_code)  
  
if (length(test > 0)) {
  stop('duplicated rows present in sites_full2')
} 
# I think this is a problem, using annual ppt is questionable at best
# use a gridded product instead?
sites_full2 %>% 
    filter(annual_ppt_used) %>% 
  pull(site_code) %>% 
  unique()

sites_full3 <- sites_full2 %>% 
  select(all_of(names(sites_full1)), annual_ppt_used)

# saving CSV --------------------------------------------------------------

write_csv(sites_full3, file.path(
  path_oct, 
  paste0('data/precip/anpp_clean_trt_ppt_no-perc', days_string,
         date_string,'.csv')
  ))


# checks ------------------------------------------------------------------

sites5 %>% 
  filter(X365day.trt != "Yes") %>% 
  pull(site_code) %>% 
  unique()


