#############################################################################################
###### Calculate precipitation reduction based on daily precip and shelter installation  ####
#############################################################################################

library(tidyverse)
library(stringr)
library(lubridate)

path <- 'C:/Users/peter/Dropbox/IDE Meeting_May2019'

precip <- read.csv(file.path(path,'IDE Site Info/GHCN_daily_precip_20190523.csv'))
precip$date <- as.Date(as.character(precip$date))

siteDrt <- read.csv(file.path(path,'IDE Site Info/Sites_Loc_DrtTrt.csv'))
siteBio <- read.csv(file.path(path, "Full biomass\\full_biomass_5-21-2019.csv"),
                            as.is = TRUE, na.strings = c("NULL"))
siteBio <- siteBio[!siteBio$trt %in% c('NPK','NPK_Drought'),]

## filter out sites without precip data
siteBio <- siteBio[siteBio$site_code %in% precip$site_code,]


### convert site biomass harvest date to POSIX (date) format

### remove weird \n in treatment dates

siteBio$first_treatment_date <- gsub('\n','',siteBio$first_treatment_date)
siteBio$biomass_date <- gsub('\n','',siteBio$biomass_date)

### convert to POSIX (date) format
siteBio$bioDat <- as.POSIXct(siteBio$biomass_date,format = '%m/%d/%Y')
siteBio$trtDat <- as.POSIXct(siteBio$first_treatment_date,format = '%m/%d/%Y')
#### some sites are randomly in other format
siteBio[is.na(siteBio$trtDat),]$trtDat <- as.POSIXct(siteBio[is.na(siteBio$trtDat),]$first_treatment_date,format = '%Y-%m-%d')

#### Rewrite number of treatment days based on n_treat_days

siteBio$nTrtYr <- 0
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
reduct <- NULL
ppt_by_trt <- NULL
for(i in unique(siteBio$site_code)){
  precipTmp <- precip[precip$site_code == i,]
  bioTmp <- siteBio[siteBio$site_code == i,]
  
  #### some sites have multiple biomass harvests, so always use the first one to designate treatment year (EUROPE ONLY - CAUSES PROBLEMS FOR SOME DRY DRY SITES THAT HAVE SPORADIC SAMPLING)
  
  if(siteDrt[siteDrt$site_code==i,]$continent == 'Europe') for(j in unique(bioTmp$year)){
    x <- bioTmp[bioTmp$year == j,]$nTrtYr
    x <- min(x)
    bioTmp[bioTmp$year == j & bioTmp$site_code == i,]$nTrtYr <- x
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

#write.csv(reduct,file.path(path,'IDE Site Info/Site drought precipitation reduction.csv'))
#write.csv(ppt_by_trt,file.path(path,'IDE Site Info/Site-year precipitation by treatment.csv'))

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
