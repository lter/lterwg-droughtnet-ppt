#############################################################################################
###### Calculate precipitation reduction based on daily precip and shelter installation  ####
#############################################################################################


# packages etc ------------------------------------------------------------


library(tidyverse)
library(stringr)
library(lubridate)
source("R_scripts/functions.R")

path <- 'E:/Dropbox/IDE Meeting_May2019'
path_oct <- 'E:/Dropbox/IDE Meeting_Oct2019'


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

# reading in site/biomass date data ---------------------------------------

# may not need this?
p3 <- newest_file_path(
  file.path(path_oct, "IDE Site Info"),
  "Site_Elev-Disturb_UPDATED_\\d+-\\d+-\\d{4}.csv",
  mdy = TRUE
)
siteDrt_A <- read.csv(p3, as.is = TRUE, na.strings = c("","<NA>", "NA"))

p4 <- newest_file_path(
  file.path(path_oct, "Full biomass"),
  "Full_Biomass-SurveyResults_\\d+-\\d+-\\d{4}.csv",
  mdy = TRUE
)
siteBio_A <- read.csv(p4, as.is = TRUE, na.strings = c("NULL"))

"stubai.at" %in% siteBio_A$site_code
siteBio_A <- siteBio_A[!siteBio_A$trt %in% c('NPK','NPK_Drought'),]


# extract drought treatment -----------------------------------------------

siteDrt_B <- siteDrt_A %>% 
  dplyr::select(site_code, drought_trt) %>% 
  mutate(drought_trt = str_replace(drought_trt, "%", ""),
         drought_trt = as.numeric(drought_trt)/100,
         # chinese sites used 50% drt--and it is na in the source file
         cn_site = str_detect(site_code, "\\.cn$"),
         drought_trt = ifelse(cn_site & is.na(drought_trt),
                              0.5,
                              drought_trt)
         ) %>% 
  select(-cn_site)

siteDrt_B # stubai.at has NA for drt treatment


# siteBio--process dates --------------------------------------------------

### convert site biomass harvest date to POSIX (date) format

### remove weird \n in treatment dates

siteBio_A$first_treatment_date <- gsub('\n','',siteBio_A$first_treatment_date)
siteBio_A$biomass_date <- gsub('\n','',siteBio_A$biomass_date)


### convert to POSIX (date) format
siteBio_A$bioDat <- as.POSIXct(siteBio_A$biomass_date,format = '%m/%d/%Y')
siteBio_A$trtDat <- as.POSIXct(siteBio_A$first_treatment_date,format = '%m/%d/%Y')
#### some sites are randomly in other format
siteBio_A[is.na(siteBio_A$trtDat),]$trtDat <- as.POSIXct(siteBio_A[is.na(siteBio_A$trtDat),]$first_treatment_date,format = '%Y-%m-%d')

sum(is.na(siteBio_A$trtDat))
sum(is.na(siteBio_A$bioDat)) #some didn't parse


# STOP: some didn't parse, because just 2017 was given as date--needs to be resolved
siteBio_A[is.na(siteBio_A$bioDat), ]

# site level information --------------------------------------------------
siteBio_B <- siteBio_A


lu <- function(x) length(unique(x)) # length (number of) unique values

first <- function(x){
  stopifnot(
    is.vector(x),
    length(unique(x)) == 1
  )
  x[1]
}

# sites with more than one start treatment year/date etc:

# STOP: NEED TO FIGURE OUT HOW TO DEAL WITH SITES WITH MULTIPLE START DATES
siteBio_B %>% 
  group_by(site_code) %>% 
  summarize_at(vars(trtDat, first_treatment_year, 
                    IfNot365.WhenShelterSet, IfNot365.WhenShelterRemove),
               .funs = lu) %>% 
  filter(trtDat >1  | first_treatment_year > 1 | IfNot365.WhenShelterSet > 1 |
         IfNot365.WhenShelterRemove > 1)

# sites with multiple biomass harvests per year
siteBio_B %>% 
  group_by(site_code, year) %>% 
  summarise(min_date = min(bioDat),
            max_date = max(bioDat),
            diff = as.numeric(max_date - min_date),
            n_bioDat = lu(bioDat),
            n_treat_days = lu(n_treat_days)
            ) %>% 
  filter(n_bioDat > 1, diff > 7) %>% 
  arrange(site_code, year) %>% 
  print(n = 100)

# STOP: here I am taking the min first_treatment date--
# consider if this is a good decision
sites1 <- siteBio_B %>% 
  filter(site_code %in% c(wthr1$site_code, precip$site_code)) %>% #only sites with precip data
  filter(!is.na(bioDat)) %>% # stop: rows when missing biodat (eg. year given only)
  group_by(site_code, year) %>% 
  summarize(first_treatment_year = min(first_treatment_year, na.rm = TRUE), 
            # should calculated n treat days-don't rely on n_treat years
            n_treat_years = min(n_treat_years, na.rm = TRUE), 
            bioDat = max(bioDat, na.rm = TRUE), # using the 2nd biomass date is better (decided on skype call)
            trtDat = min(trtDat), # first treat date
            IfNot365.WhenShelterSet = first(IfNot365.WhenShelterSet),
            IfNot365.WhenShelterRemove = first(IfNot365.WhenShelterRemove),
            X365day.trt = first(X365day.trt)) %>% 
  left_join(siteDrt_B, by = "site_code") %>% 
  ungroup()


# reviewing sites with multiple start dates etc ---------------------------

# bayrdrt.de ~~~~
# the first 2015-08-01 should be used here (checked w/ PW)--so code above fine
siteBio_B %>% 
  filter(site_code == "bayrdrt.de") %>% 
  pull(trtDat) %>% 
  unique()

# "brandbjerg.dk has another experiment nearby which started earlier. 
# Drought-net treatments however started “2016-06-21” however, looks like they
# never sent data" so leaving this for now..
siteBio_B %>% 
  filter(site_code == "brandjberg.dk") %>% 
  pull(trtDat) %>% 
  unique()

# ethadn.au--leaving for now
siteBio_B %>% 
  filter(site_code == "ethadn.au") 

# for both pne sites pw recommended 2018-04-28 (ie min  is ok)
siteBio_B %>% 
  filter(site_code == "pneunburn.br") %>% 
  pull(trtDat) %>% 
  unique()

# pw recommended min data here so leaving
siteBio_B %>% 
  filter(site_code == "sevforest.us") %>% 
  pull(trtDat) %>% 
  unique()

siteBio_A$site_code[!siteBio_A$site_code %in% siteDrt_B$site_code] %>%  # should be none
  unique()

sum(is.na(siteDrt_B$drought_trt)) # NAs--some sites haven't provided drt trmt values

sites2 <- sites1 %>% 
  mutate(bioDat = as.Date(bioDat),
         trtDat = as.Date(trtDat),
         ppt_drought = NA_real_,
         ppt_ambient = NA_real_,
         ppt_num_NA = NA_real_, 
         num_drought_days = NA_real_,
         ppt_num_wc_interp = NA_real_)
precip1 <- precip %>% 
  mutate(date = as.Date(date))

filter(sites2, X365day.trt == "No") %>% 
  select(site_code, IfNot365.WhenShelterSet, IfNot365.WhenShelterRemove) %>% 
  print(n = 30)




# yearly control/drt precip -----------------------------------------------

# first calculating using ghcn data
sites2_forghcn <- sites2 %>% 
  filter(site_code %in% precip1$site_code)

options(warn=1) # print warnings as they occur
sites3_ghcn <- calc_yearly_precip(site_data = sites2_forghcn,
                                  precip_data = precip1)


# calculating using submitted data
wthr2 <- wthr1 %>% 
  rename(ppt = precip)

sites2_forsubmitted <- sites2 %>% 
  filter(site_code %in% wthr2$site_code) 

# reformatting some dates so that they can be used by the function 
set_date <- str_detect(sites2_forsubmitted$IfNot365.WhenShelterSet, "\\d+/\\d+/\\d{4}")

sites2_forsubmitted$IfNot365.WhenShelterSet[set_date] <- 
  sites2_forsubmitted$IfNot365.WhenShelterSet[set_date] %>% 
  mdy() %>% 
  paste(day(.), month(., label = TRUE)) %>% 
  str_extract("\\d+\\s[A-z]+$")

rem_date <- str_detect(sites2_forsubmitted$IfNot365.WhenShelterRemove, "\\d+/\\d+/\\d{4}")

sites2_forsubmitted$IfNot365.WhenShelterRemove[rem_date] <- 
  sites2_forsubmitted$IfNot365.WhenShelterRemove[rem_date] %>% 
  mdy() %>% 
  paste(day(.), month(., label = TRUE)) %>% 
  str_extract("\\d+\\s[A-z]+$")

names(wthr2)

sites3_submitted <- calc_yearly_precip(site_data = sites2_forsubmitted,
                                       precip_data = wthr2)


# combining ghcn and submitted results ------------------------------------
sites4 <- full_join(sites3_ghcn, sites3_submitted, 
                    by = c(names(sites1)),
                    suffix = c("_ghcn", "_sub"))

nrow(sites4)
nrow(sites2)

sites5 <- sites4 %>% 
  rename(biomass_date = bioDat,
         first_treatment_date = trtDat) %>% 
  select(-ppt_num_wc_interp_ghcn)


# comparing ghcn to submitted data ----------------------------------------

theme_set(theme_classic())

# pdf(file.path(path_oct,
#               paste0("figures/precip/ghcn_vs_submitted_precip_", today(), ".pdf")
#              ))
sites5 %>% 
  filter(ppt_num_NA_sub < 30 & ppt_num_NA_ghcn < 30) %>% 
  ggplot(aes(ppt_ambient_sub, ppt_ambient_ghcn)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0) +
  labs(y = "365 day precip sum from GHCN (mm)",
       x = "365 day precip sum from submitted data (mm)")
  
dev.off()


# combining ghcn and submitted ----------------------------------------------

# use submitted data if it has fewer than 30 missing values, 
# else use GHCN data, if both have >30 missing values than put NA and not using
# sumbitted data if more than 30 days were interpolated

sites5 <- sites5 %>% 
  mutate(ppt_ambient = ifelse(ppt_num_NA_sub < 30 & !is.na(ppt_ambient_sub) & 
                                ppt_num_wc_interp_sub < 30,
                              ppt_ambient_sub,
                              ifelse(ppt_num_NA_ghcn < 30,
                                     ppt_ambient_ghcn,
                                     NA)
                              ),
         ppt_drought = ifelse(ppt_num_NA_sub < 30 & !is.na(ppt_drought_sub)  & 
                                ppt_num_wc_interp_sub < 30,
                              ppt_drought_sub,
                              ifelse(ppt_num_NA_ghcn < 30,
                                     ppt_drought_ghcn,
                                     NA)
                              )
         )

# saving CSV --------------------------------------------------------------

# write_csv(sites5,
#           file.path(path_oct, 'data/precip/precip_by_trmt_year_2019-12-15.csv'))


sites5 %>% 
  filter(X365day.trt != "Yes") %>% 
  pull(site_code) %>% 
  unique()
