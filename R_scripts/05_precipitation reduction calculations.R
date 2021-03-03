#############################################################################################
###### Calculate precipitation reduction based on daily precip and shelter installation  ####
#############################################################################################


# packages etc ------------------------------------------------------------


library(tidyverse)
library(stringr)
library(lubridate)
source("R_scripts/functions.R")

path <- '~/Dropbox/IDE Meeting_May2019'
path_oct <- '~/Dropbox/IDE Meeting_Oct2019'
path_ms <-  "~/Dropbox/IDE MS_Single year extreme"


# reading in precip data -----------------------------------------------------

# GHCN data--getting newest file based on file name
p1 <- newest_file_path(
  path = file.path(path_oct,'data/precip'),
  file_regex = 'GHCN_daily_precip_\\d{4}-\\d+-\\d+.csv' 
)
p1
precip <- read.csv(p1,as.is = TRUE)

# submitted data
# grabbing most recent submitted daily weather file
dly_wthr_path <- newest_file_path(
  path = file.path(path_oct, "data/precip"),
  file_regex =  "submitted_daily_weather_WC_supplemented_\\d{4}-\\d{2}-\\d{2}.csv" 
)
dly_wthr_path

wthr1 <- read_csv(dly_wthr_path)
wthr1$X1 <- NULL

# sites where we have only GHCN data
precip$site_code[!precip$site_code %in% wthr1$site_code] %>% unique()

# reading in site/anpp date data ---------------------------------------

siteDrt_A <- read.csv(file.path(path_ms, "Data/Site_Elev-Disturb.csv"), 
                      as.is = TRUE, na.strings = c("","<NA>", "NA")) %>% 
  as_tibble()


# p5 <- newest_file_path(
#   file.path(path_ms, "Data"),
#   "anpp_clean_\\d+-\\d+-\\d{4}.csv",
#   mdy = TRUE)
# 
# anpp1 <- read.csv(p5, as.is = TRUE)
# head(anpp1)

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
appt1$annual_ppt[is.na(appt2$annual_ppt) & !is.na(appt1$annual_ppt)] %>% 
  unique()

# extract biomass date ----------------------------------------------------

bio2 <- bio1 %>% 
  group_by(site_code, trt, block, plot, subplot, year) %>% 
  summarize(first_treatment_date = min(first_treatment_date),
            biomass_date = max(biomass_date)) %>% 
  # add in survey info
  left_join(survey1, by = "site_code")


# extract drought treatment -----------------------------------------------

siteDrt_B <- siteDrt_A %>% 
  dplyr::select(site_code, drought_trt) %>% 
  mutate(drought_trt = str_replace(drought_trt, "%", ""),
         drought_trt = as.numeric(drought_trt)/100)

# 
siteDrt_A %>% 
  filter(is.na(siteDrt_B$drought_trt)) %>% 
  select(site_code, drought_trt)

# b/ didn't parse above
siteDrt_B$drought_trt[siteDrt_B$site_code == "elizwood.us"] <- 0.5

# b/ not yet updated in data by time of running this code
siteDrt_B$drought_trt[siteDrt_B$site_code == "sand.us"] <- 0.3

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

anpp2$dummy_date <- ymd("1900-01-01")      
anpp2$dummy_date[is_mdy] <- mdy(anpp2$first_treatment_date[is_mdy])
anpp2$dummy_date[!is_mdy] <- ymd(anpp2$first_treatment_date[!is_mdy])

# in the end results will be joined back in
anpp2 <- anpp2 %>% 
  select(-first_treatment_date) %>% 
  rename(first_treatment_date = dummy_date) %>% 
  # parsing failures because one biomass date entered as "2017"
  mutate(biomass_date = ymd(biomass_date))

filter(anpp2, is.na(biomass_date))
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

# these sites just provided text in the set/remove fields
on_off_dates <- tibble(site_code = c("cedarsav.us", "cedarsav.us", "cedarsav.us", "cedartrait.us", 
                     "cedartrait.us", "cedartrait.us"),
       on = c("2017-04-27", "2018-05-05", "2019-04-30", 
              "2017-04-27", "2018-05-05", "2019-05-2"),
       off = c("2017-09-21", "2018-09-25", "2019-09-25",
               "2017-09-20","2018-09-11", "2019-09-24"),
       year = year(on))


# cedarsav.us has notes in place of dates in 2016 (pre trmt)
anpp3 <- anpp3 %>% 
  left_join(on_off_dates, by = c("site_code", "year")) %>% 
  mutate(
    IfNot365.WhenShelterSet = ifelse(is.na(on), IfNot365.WhenShelterSet, on),
    IfNot365.WhenShelterRemove = ifelse(is.na(off), IfNot365.WhenShelterRemove, off),
    IfNot365.WhenShelterSet = ifelse(
    year == 2016 & site_code == "cedarsav.us",
    "",
    IfNot365.WhenShelterSet),
    IfNot365.WhenShelterRemove = ifelse(
      year == 2016 & site_code == "cedarsav.us",
      "",
      IfNot365.WhenShelterRemove)) %>% 
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
sites2 <- sites1 %>% 
  mutate(ppt_drought = NA_real_,
         ppt_ambient = NA_real_,
         ppt_num_NA = NA_real_, 
         num_drought_days = NA_real_,
         ppt_num_wc_interp = NA_real_)


# yearly control/drt precip -----------------------------------------------

# first calculating using ghcn data
sites2_forghcn <- sites2 %>% 
  filter(site_code %in% precip1$site_code)

options(warn=1) # print warnings as they occur
sites3_ghcn <- calc_yearly_precip(site_data = sites2_forghcn,
                                  precip_data = precip1)

# for diagnosing problems
# sites with parsing failures when calculating yearly ppt from ghcn data
ghcn_probs <- c("antelope.us", "bfl.us", "elizwood.us", "jrnchi.us", 
                "nnss.us", "octc.us", "sevmixed.us",  "slp.us")


site_data = sites2_forghcn %>% 
  filter(site_code %in% ghcn_probs[1])
precip_data = precip1 %>% 
  filter(site_code %in% ghcn_probs[1])

anpp3 %>% 
  filter(site_code %in% ghcn_probs, !is.na(X365day.trt)) 
# end diagnosing problems

# calculating using submitted data
wthr2 <- wthr1 %>% 
  rename(ppt = precip)

# STOP: temporary fix! (year not date provided for bio date for pozos.ar) 
sites2_forsubmitted <- sites2 %>%
  filter(site_code %in% wthr2$site_code,
        !is.na(bioDat))

sites3_submitted <- calc_yearly_precip(site_data = sites2_forsubmitted,
                                       precip_data = wthr2)

# combining ghcn and submitted results ------------------------------------


sites4 <- full_join(sites3_ghcn, sites3_submitted, 
                    by = c(names(sites1)),
                    suffix = c("_ghcn", "_sub"))

nrow(sites4)
nrow(sites2)

sites5 <- sites4 %>% 
  select(-ppt_num_wc_interp_ghcn)


# merge back to main anpp file --------------------------------------------

sites6 <- anpp3 %>% 
  left_join(sites5, by = c("site_code", "year", "bioDat", "trtDat", 
                           "X365day.trt", "IfNot365.WhenShelterRemove", 
                           "IfNot365.WhenShelterSet", "drought_trt")) %>% 
  mutate(ppt_ghcn = ifelse(trt == "Control", ppt_ambient_ghcn, 
                           ppt_drought_ghcn),
         ppt_sub = ifelse(trt == "Control", ppt_ambient_sub, 
                           ppt_drought_sub)) 
# should be 0:
sum(sites6$num_drought_days_ghcn != sites6$num_drought_days_sub, na.rm = TRUE)


# use submitted data if it has fewer than 30 missing values, 
# else use GHCN data, if both have >30 missing values than put NA and not using
# sumbitted data if more than 30 days were interpolated
sites7 <- sites6 %>% 
  mutate(num_drought_days = ifelse(is.na(num_drought_days_sub), 
                                   num_drought_days_ghcn, 
                                   num_drought_days_sub)) %>% 
  select(-num_drought_days_sub, -num_drought_days_ghcn, 
         -matches("_(drought)|(ambient)_"), num_drought_days) %>%
  mutate(ppt = ifelse(rowSums(.[, c("ppt_num_NA_sub", "ppt_num_wc_interp_sub")], na.rm = TRUE) < 30 & !is.na(ppt_sub),
                      ppt_sub,
                      ifelse(ppt_num_NA_ghcn < 30,
                             ppt_ghcn,
                             NA))
  )

sites7

# this code causing problems because of imperfect join
sites_full1 <- sites7 %>% 
  rename(biomass_date = bioDat,
         first_treatment_date = trtDat) %>% 
  full_join(anpp2, by = c("site_code", "block", "plot", "subplot", "year", "trt", 
                          "biomass_date", "first_treatment_date", "X365day.trt")) %>% 
  # solving issue of adjustments above causing join incompatibility with cedartrait
  mutate(IfNot365.WhenShelterRemove = ifelse(is.na(IfNot365.WhenShelterRemove.x),
                                             IfNot365.WhenShelterRemove.y,
                                             IfNot365.WhenShelterRemove.x),
         IfNot365.WhenShelterSet = ifelse(is.na(IfNot365.WhenShelterSet.x),
                                             IfNot365.WhenShelterSet.y,
                                             IfNot365.WhenShelterSet.x)) %>% 
  select(-matches("\\.(x|y)$")) 

# should be true 
nrow(sites_full1)==nrow(anpp2)

# sites where GHCN used for at least 1 year
sites_full1 %>% 
  filter(!is.na(ppt) & ppt != ppt_sub) %>% 
  pull(site_code) %>% 
  unique()

nrow(sites_full1) # shouldn't have added rows with join
nrow(anpp2)

# comparing ghcn to submitted data ----------------------------------------

theme_set(theme_classic())



# pdf(file.path(path_oct,
#               paste0("figures/precip/ghcn_vs_submitted_precip_", today(), ".pdf")
#              ))


# ghcn data for this site is bad
sites7 %>% 
  filter(ppt_sub == 0 | ppt_ghcn == 0) %>% 
  pull(site_code) %>% 
  unique()

sites7 %>% 
  filter((ppt_num_NA_sub + ppt_num_wc_interp_sub) < 30 & ppt_num_NA_ghcn < 30,
         trt == "Control") %>% 
  group_by(site_code, year) %>% 
  mutate(ppt_sub = mean(ppt_sub),
         ppt_ghcn = mean(ppt_ghcn)) %>% 
  ggplot(aes(ppt_sub, ppt_ghcn)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(y = "365 day precip sum from GHCN (mm)",
       x = "365 day precip sum from submitted data (mm)")
  
dev.off()


# adding in annual ppt ----------------------------------------------------

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
  
  
  
# I think this is a problem, using annual ppt is questionable at best
# use a gridded product instead?
sites_full2 %>% 
    filter(annual_ppt_used) %>% 
  pull(site_code) %>% 
  unique()

sites_full3 <- sites_full2 %>% 
  select(names(sites_full1), annual_ppt_used)
# saving CSV --------------------------------------------------------------

write_csv(sites_full3,
          file.path(path_oct, 'data/precip/anpp_clean_trt_ppt_no-perc_2021-03-02.csv'))


# checks ------------------------------------------------------------------


sites5 %>% 
  filter(X365day.trt != "Yes") %>% 
  pull(site_code) %>% 
  unique()


