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

# submitted data
precip_submitted <- read.csv(file.path(path_oct,'data/precip/submitted_daily_weather_2019-10-01.csv'),
                   as.is = TRUE)

# may not need this?
siteDrt <- read.csv(file.path(path,'IDE Site Info/Sites_Loc_DrtTrt.csv'))

siteBio_A <- read.csv(
  file.path(path_oct, "Full biomass\\Full_Biomass-SurveyResults_10-01-2019.csv"),
  as.is = TRUE, na.strings = c("NULL"))

siteBio_A <- siteBio_A[!siteBio_A$trt %in% c('NPK','NPK_Drought'),]


# combine precip ----------------------------------------------------------

names(precip_submitted)

precip_submitted2 <- precip_submitted %>% 
  rename(station_elevation = elev,
         ppt = precip) %>% 
  mutate(pi_submitted = TRUE) %>% #PI submitted data
  select(-min_temp, -max_temp, -note_station, -Note_treatments, -source, -url) 
names(precip)

precip2 <- precip %>% 
  mutate(pi_submitted = FALSE, # not pi submitted
         station_name = NA) %>% 
  rename(station_id = id) %>% 
  select(-site_elevation, -year, -month, - diff_elevation, "day_of_month")
names(precip2)

# both submitted and GHCN
precip3 <- bind_rows(precip2, precip_submitted2) %>% 
  select(site_code, station_id, pi_submitted, ppt, everything())
names(precip3)


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
  group_by(site_code) %>% 
  summarize(first_treatment_year = min(first_treatment_year, na.rm = TRUE), 
            trtDat = min(trtDat), 
            IfNot365.WhenShelterSet = first(IfNot365.WhenShelterSet),
            IfNot365.WhenShelterRemove = first(IfNot365.WhenShelterRemove),
            X365day.trt = first(X365day.trt))




# trying different method--joining with precip data -----------------------
names(sites1)
precip4 <- precip3 %>% 
  left_join(sites1, by = "site_code") %>% 
  mutate(n_treat_days = difftime(date, trtDat, units="days"))


str(precip4)



#running until this point....

# ## filter out sites without precip data
# siteBio <- siteBio[siteBio$site_code %in% precip$site_code,]
#### Rewrite number of treatment years based on n_treat_days for early years

siteBio$nTrtYr <- siteBio$n_treat_years
siteBio$nTrtYr <- ifelse(siteBio$n_treat_days < 370 & siteBio$n_treat_days >= 30, 1, siteBio$nTrtYr) #### slight extension to ensure sites have a 'year 1' - some are a few days past 365 for firsts sampling
siteBio$nTrtYr <- ifelse(siteBio$n_treat_days < 731 & siteBio$n_treat_days >= 370, 2, siteBio$nTrtYr) #### slight extension to ensure sites have a 'year 2' - one site at 730 for firsts sampling
#### call everything past 3, 3 for now as we're only working with first two treatment years
siteBio$nTrtYr <- ifelse(siteBio$n_treat_days >= 731, 3, siteBio$nTrtYr)

### ORE and santa cruz sites are slightly later than 370, so we will adjust there first year independently
siteBio$nTrtYr <- ifelse(siteBio$site_code %in% c('oreaa.us','oreac.us', 'scruzm.us') & siteBio$n_treat_days < 390 & siteBio$n_treat_days >= 30,
                         1, siteBio$nTrtYr) 
siteBio$nTrtYr <- ifelse(siteBio$site_code %in% c('oreaa.us','oreac.us') & siteBio$n_treat_days  < 760 & siteBio$n_treat_days >= 390,
                         2, siteBio$nTrtYr) 

### wytham sampled late in 2nd year, adjusting treatment year
siteBio$nTrtYr <- ifelse(siteBio$site_code %in% c('wytham.uk') & siteBio$year  ==2018,
                         2, siteBio$nTrtYr) 

#### remove site (pozos.ar) with no date for now
siteBio <- siteBio[!is.na(siteBio$bioDat),]


### calculate reduction per site in for loop
reduct <- NULL  ### data frame for holding treatment effect - precipitation reduction
ppt_by_trt <- NULL ### data frame for holding precipitation amount in first two treatment years for drought and ctrl
pptAmb <- NULL ### data frame for holding ambient precipitation 12 months prior to every sampling date
for(i in unique(siteBio$site_code)){
  precipTmp <- precip[precip$site_code == i,]
  bioTmp <- siteBio[siteBio$site_code == i,]
  
  #### some sites have multiple biomass harvests, so always use the first one to designate treatment year (EUROPE ONLY - CAUSES PROBLEMS FOR SOME DRY DRY SITES THAT HAVE SPORADIC SAMPLING)
  
  if(siteDrt[siteDrt$site_code==i,]$continent == 'Europe') for(j in unique(bioTmp$year)){
    x <- bioTmp[bioTmp$year == j,]$nTrtYr
    x <- min(x)
    bioTmp[bioTmp$year == j & bioTmp$site_code == i,]$nTrtYr <- x
  }
  
  for(j in unique(bioTmp$nTrtYr)){
      dat1 <- unique(bioTmp[bioTmp$nTrtYr == j,]$bioDat)
      dat0 <- dat1 - dyears(1)
      pptTmp <- precipTmp[precipTmp$date <dat1 & precipTmp$date >= dat0,]
      paTmp <- data.frame(site_code = i, treatment_year = j, year = max(pptTmp$year), harvest_date = max(pptTmp$date), annual_precip = sum(pptTmp$ppt,na.rm=T))
      pptAmb <- rbind(pptAmb,paTmp)
    }
  
  bioDat1 <- unique(bioTmp[bioTmp$nTrtYr == 1,]$bioDat)
  ### take latest date for sites with multiple sample dates
  bioDat1 <- max(bioDat1)
  bioDat0 <- bioDat1 - dyears(1)
  
  ### repeat for year 2
  bioDat2 <- unique(bioTmp[bioTmp$nTrtYr == 2,]$bioDat)
  bioDat2 <- max(bioDat2)
  
  trtTmp <- bioTmp$trtDat[1]
  trtTmp <- as.POSIXct(trtTmp)
  trtRed <- siteDrt[siteDrt$site_code == i,]$drought_trt
  
  ### pare preciptation down to needed dates splitting between pre shelter and post shelter
  precipyr1_pre <- precipTmp[precipTmp$date  <= trtTmp & precipTmp$date >= bioDat0,]
  precipyr1_post <- precipTmp[precipTmp$date  > trtTmp & precipTmp$date <= bioDat1,]
  
  
  ### calculate control precipitation for year 1
  pptCtrl1 <- sum(precipyr1_pre$ppt,na.rm=T) + sum(precipyr1_post$ppt,na.rm=T)
  
  ### sum full precip before shelters, and reduced precip for after shelter installation
  pptDrt1 <- sum(precipyr1_pre$ppt,na.rm=T) + sum(precipyr1_post$ppt,na.rm=T)*(1-trtRed/100)
  
  
  ### pare preciptation down to needed dates splitting between pre shelter and post shelter
  precipyr2 <- precipTmp[precipTmp$date  >= bioDat1 & precipTmp$date < bioDat2,]
  
  ### calculate control precipitation for year 2
  pptCtrl2 <- sum(precipyr2$ppt,na.rm=T)
  
  ### sum full precip before shelters, and reduced precip for after shelter installation
  pptDrt2 <- sum(precipyr2$ppt,na.rm=T)*(1-trtRed/100)
  
  dfTmp <- data.frame(site_code=i,trt=c('Ctrl','Drt','Ctrl','Drt'),trt_year=c(1,1,2,2),ppt=c(pptCtrl1,pptDrt1,pptCtrl2,pptDrt2))
  ppt_by_trt <- rbind(ppt_by_trt,dfTmp)
  
  diffTmp <- data.frame(site_code=i,trt_year=c(1,2),ppt_drt_diff=c(pptDrt1-pptCtrl1,pptDrt2-pptCtrl2))
  reduct <- rbind(reduct,diffTmp)
  
  }  


reduct<-reduct[reduct$ppt_drt_diff != 0,]
ppt_by_trt <- ppt_by_trt[ppt_by_trt$ppt != 0,]
pptAmb <- pptAmb[pptAmb$annual_precip !=0,]

#write.csv(reduct,file.path(path,'IDE Site Info/Site drought precipitation reduction.csv'))
#write.csv(ppt_by_trt,file.path(path,'IDE Site Info/Site-year precipitation by treatment.csv'))
#write.csv(pptAmb,file.path(path,'IDE Site Info/Annual precip 12 months preceding harvest by site.csv'))

range(siteBio[siteBio$site_code == 'wytham.uk',]$year)
i <-'wytham.uk'


#### baddrt.de is missing precip data
#### bivensarm.us starts treatment in 2013 but doesn't report biomass until 2015
#### broken hill has sporadic sampling, for now data is based on installation of shelter
#### brookdale has only one year sampling date
#### falls.au only has one year treatment
#### hoide had 2017 data not entered, should correct when new biomass is output
#### hyide is still missing 2016 and 2017 data and needs to be entered
#### Jena biomass went missing in 2016 - no year one data
#### JIlpanger only has two years of data
#### kiskun.hu weather station is reporting zeros for precip after a certain date - need to find better data
#### ORE sites treatment years were adjusted due to timing of shelter installation
#### scruz.us has sporadic sampling - year one was adjusted to have a longer period post treatment installation
#### sevforest only has one year of data
#### SGS only has one year of data (despite pretreatment occurring in 2014)
#### thompson.us only has one year of data
#### plattev.us only has pre-treatment year
