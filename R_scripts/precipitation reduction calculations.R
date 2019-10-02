#############################################################################################
###### Calculate precipitation reduction based on daily precip and shelter installation  ####
#############################################################################################

# NOTE: THIS SCRIPT HAS NOT YET BEEN SUFFICIENTLY UPDATED TO WORK WITH NEW (OCT 2019) DATA


# packages etc ------------------------------------------------------------


library(tidyverse)
library(stringr)
library(lubridate)

path <- 'E:/Dropbox/IDE Meeting_May2019'
path_oct <- 'E:/Dropbox/IDE Meeting_Oct2019'


# reading in data ---------------------------------------------------------

# GHCN data
precip <- read.csv(file.path(path_oct,'data/precip/GHCN_daily_precip_2019-10-01.csv'),
                   as.is = TRUE)


# may not need this?
siteDrt_A <- read.csv(file.path(path_oct,"IDE Site Info/Site_Elev-Disturb_UPDATED_10-01-2019.csv"),
                      as.is = TRUE, na.strings = c("", "NA", "<NA>"))

siteBio_A <- read.csv(
  file.path(path_oct, "Full biomass\\Full_Biomass-SurveyResults_10-01-2019.csv"),
  as.is = TRUE, na.strings = c("NULL"))

"matta.il" %in% siteBio_A$site_code
siteBio_A <- siteBio_A[!siteBio_A$trt %in% c('NPK','NPK_Drought'),]


# extract drought treatment -----------------------------------------------

siteDrt_B <- siteDrt_A %>% 
  select(site_code, drought_trt) %>% 
  mutate(drought_trt = str_replace(drought_trt, "%", ""),
         drought_trt = as.numeric(drought_trt)/100)

siteDrt_B


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

siteBio_B %>% 
  filter(site_code == "bayrdrt.de")

siteBio_B %>% 
  filter(site_code == "brandjberg.dk") 

# STOP: here I am taking the min first_treatment date--should more carefully
# consider fi this is a good decision
sites1 <- siteBio_B %>% 
  filter(site_code %in% precip$site_code) %>% #only sites with precip data
  filter(!is.na(bioDat)) %>% # stop: rows when missing biodat (eg. year given only)
  group_by(site_code, year) %>% 
  summarize(first_treatment_year = min(first_treatment_year, na.rm = TRUE), 
            n_treat_years = min(n_treat_years, na.rm = TRUE), # STOP--min is temporary fix 
            bioDat = min(bioDat, na.rm = TRUE), 
            trtDat = min(trtDat), # first treat date
            IfNot365.WhenShelterSet = first(IfNot365.WhenShelterSet),
            IfNot365.WhenShelterRemove = first(IfNot365.WhenShelterRemove),
            X365day.trt = first(X365day.trt)) %>% 
  left_join(siteDrt_B, by = "site_code") %>% 
  ungroup()




siteBio_A$site_code[!siteBio_A$site_code %in% siteDrt_B$site_code] # should be none

sum(is.na(siteDrt_B$drought_trt)) # 2 NAs--not sure why

sites2 <- sites1 %>% 
  mutate(bioDat = as.Date(bioDat),
         trtDat = as.Date(trtDat),
         ppt_drought = NA_real_,
         ppt_ambient = NA_real_,
         ppt_num_NA = NA_real_, 
         num_drought_days = NA_real_)
precip1 <- precip %>% 
  mutate(date = as.Date(date))

# calculating precip/year:

for (i in 1:nrow(sites2)) {
  print(i)
  row <- sites2[i, ]
  print(row$site_code)
  site_ppt <- precip1[precip1$site_code == row$site_code,]
  start_date <- as.Date(row$bioDat-365)
  site_ppt2 <- site_ppt %>% 
    filter(date >= start_date & date < row$bioDat) %>% 
    mutate(n_treat_days = difftime(date, row$trtDat, units="days"))
  
  # when did the shelter come off:
  shelter_off_start <- if (row$X365day.trt == "No") {
    min_year <- min(year(site_ppt2$date))
    dmy(paste(row$IfNot365.WhenShelterRemove, min_year))
  } else {
    NA
  }
  # when did the shelter go back off
  shelter_off_end <- if (row$X365day.trt == "No") {
    max_year <- max(year(site_ppt2$date))
    dmy(paste(row$IfNot365.WhenShelterSet, max_year))
  } else {
    NA
  }
  is_trt365 <- rep(row$X365day.trt == "Yes", nrow(site_ppt2))
  site_ppt2 <- site_ppt2 %>% 
    mutate(
      # is drought occuring
      is_drought = ifelse(n_treat_days < 0,
                                 0,
                                 ifelse(is_trt365,
                                        1,
                                        ifelse(date < shelter_off_start | date > shelter_off_end | is.na(shelter_off_start),
                                        1,
                                        0)
                                 )),
      drought_ppt = ifelse(is_drought,
                           (ppt*(1 - row$drought_trt)),
                           ppt)
    )
  sites2[i,]$ppt_num_NA <-  sum(is.na(site_ppt2$ppt))
  sites2[i,]$num_drought_days <- sum(site_ppt2$is_drought)
  
  sites2[i,]$ppt_ambient <-  sum(site_ppt2$ppt, na.rm = TRUE)
  sites2[i,]$ppt_drought <-  sum(site_ppt2$drought_ppt, na.rm = TRUE)
  
}


sites3 <- sites2 %>% 
  rename(biomass_date = bioDat,
         first_treatment_date = trtDat)
dim(sites3)
write_csv(sites3,
          file.path(path_oct, 'data/precip/precip_by_trmt_year_2019-10-02.csv'))
