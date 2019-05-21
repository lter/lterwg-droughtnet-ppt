############################################################################
######## EXTRACT CLIMATE DATA FROM HGCN AND CALCULATE HISTORICAL CV AND MAP ACROSS AT LEAST 30 YEARS #######
############################################################################

library(rnoaa)
library(tidyverse)
library(lubridate)

source('R_scripts/functions.R')
CV <- function(x) sd(x)/mean(x) * 100

# path to data folders
path <- 'C:/Users/peter/Dropbox/IDE Meeting_May2019'

# site locations
site <- read.csv(file.path(path, 'IDE Site Info/Sites_Loc_DrtTrt.csv'), as.is = TRUE)

# function(s) used below


site <- site[,c('site_code','lat','long')]
colnames(site) <- c('id','lat','long')

# pull list of all GHCND stations (takes a while to run)
#stations <- ghcnd_stations() #

#write.csv(stations, file.path(path, 'IDE Site Info/GHCND_Stations.csv'))

# load csv:
stations <- read.csv(file.path(path, 'IDE Site Info/GHCND_Stations.csv'), as.is = TRUE)

stations$X <- NULL # unnecessary column created in csv

stationsPpt <- stations[stations$element == 'PRCP',]
stationsPpt <- stationsPpt[stationsPpt$first_year <= 1980 & stationsPpt$last_year >= 2000,]


nearStation <- meteo_nearby_stations(site,lat_colname='lat',lon_colname = 'long',station_data = stationsPpt,limit=3)

# extracting just the nearest station for each site
nearest <- lapply(nearStation, function(x){
  x[1, ]
})

nearest_df <- bind_rows(nearest)
nearest_df$site_code <- names(nearest)

nearest_df[nearest_df$distance > 100,]
nearest_df[nearest_df$distance < 20,]


#nearest_test <- nearest_df[1:5,]

precip <- NULL

# for each site, getting data from nearest station, and calculating monthly
# precip sums, if a month has > 15 NAs then it's value is NA
for(i in 1:nrow(nearest_df)){
  staID <- as.character(nearest_df[i,'id'])
  sc <- as.character(nearest_df[i,'site_code'])
  
  tmp <- ghcnd(staID)
  
  df_long <- tmp %>% 
    filter(element == "PRCP") %>% # precip data only
    select(id, year, month, contains("VALUE")) %>% # discarding other fields
    # converting data to long form:
    gather(key = "day_of_month", value = "ppt", matches("VALUE")) %>% 
    # extracting number from eg value21:
    mutate(day_of_month = str_extract(day_of_month, "\\d+$"),
           day_of_month = as.numeric(day_of_month),
           site_code = sc) 
  
  precip <- rbind(precip, df_long)
  print(i)
}

# maybe we should save this raw monthly (precip) data file?

# this is the full parsed data set since 1964
precipFull <- precip %>% 
  arrange(site_code, year, month, day_of_month) %>% 
  filter(year >= 1964) %>%  # only keeping data that matches TPA time frame
  mutate(date = make_date(year, month, day_of_month),
         ppt = ppt/10) %>% # convert 10ths of mm to mm
  filter(!is.na(date)) # e.g 30th day of february

ppt_annual <- precipFull %>% 
  group_by(id, site_code, year) %>% 
  summarise(n_days = n(),# number of rows per year/site (number of observations (missing data or not))
            n_NA = sum(is.na(ppt)), # number of days with missing data
            ap = sum(ppt, na.rm = TRUE) #annual precipitation
  ) 

summary(ppt_annual)

# just "good years"
ppt_annual2 <- ppt_annual %>% 
  mutate(n_good = n_days - n_NA) %>%  # number of good observations/year
  filter(n_good > 334)

ppt_summary <- ppt_annual2 %>% 
  group_by(id, site_code) %>% 
  summarize(n_good_yrs = n(), # number of good years per stations
            year_start = min(year), # first "good year"
            year_end = max(year),# last good year
            map = mean(ap),
            cv = CV(ap)) 

sum(ppt_summary$n_good_yrs > 30)

ppt_summary %>% 
  filter(n_good_yrs > 20) %>% 
  ggplot(aes(map, cv)) + 
  geom_point() +
  theme_classic()

plot(ppt_summary$map, ppt_summary$cv)
hist(ppt_summary$n_good_yrs)

# sites with good wx data
good_sites <- ppt_summary$site_code[ppt_summary$n_good_yrs > 30]

# sites with not sufficient good years
bad_sites <- site$id[!site$id %in% good_sites]

# saveRDS(bad_sites, "site_names_needWx_data.rds")
#write.csv(ppt_summary, file.path(path, 'IDE Site Info/GHCN MAP-CV data PRELIMINARY 20190521.csv'))


############ REPEAT PROCESS WITH NEW STATIONS BASED ON ABOVE NA VALUE PROBLEMS AND LARGE ELEVATIONAL DIFFERENCES ############
#### New station selection occurred in script "DN site - GHCN station option". Some problem sites did not have suitably close stations to try replacing

head(nearest_df)

### replace target sites with new station

stationNA <- readRDS('new stations NA values')
stationElev <- readRDS('new stations elevation matches')
stationAll <- rbind(stationNA,stationElev)

stationReps <- merge(nearest_df,stationAll,by='site_code',all.x=T)
stationReps$id <- ifelse(is.na(stationReps$Station_ID),stationReps$id,stationReps$Station_ID)

precipV2 <- NULL

# for each site, getting data from nearest station, and calculating monthly
# precip sums, if a month has > 15 NAs then it's value is NA
for(i in 1:nrow(stationReps)){
  staID <- as.character(stationReps[i,'id'])
  sc <- as.character(stationReps[i,'site_code'])
  
  tmp <- ghcnd(staID)
  
  df_long <- tmp %>% 
    filter(element == "PRCP") %>% # precip data only
    select(id, year, month, contains("VALUE")) %>% # discarding other fields
    # converting data to long form:
    gather(key = "day_of_month", value = "ppt", matches("VALUE")) %>% 
    # extracting number from eg value21:
    mutate(day_of_month = str_extract(day_of_month, "\\d+$"),
           day_of_month = as.numeric(day_of_month),
           site_code = sc) 
  
  precipV2 <- rbind(precipV2, df_long)
  print(i)
}

# maybe we should save this raw monthly (precip) data file?

# this is the full parsed data set since 1964
precipFullV2 <- precipV2 %>% 
  arrange(site_code, year, month, day_of_month) %>% 
  filter(year >= 1964) %>%  # only keeping data that matches TPA time frame
  mutate(date = make_date(year, month, day_of_month),
         ppt = ppt/10) %>% # convert 10ths of mm to mm
  filter(!is.na(date)) # e.g 30th day of february

ppt_annualV2 <- precipFullV2 %>% 
  group_by(id, site_code, year) %>% 
  summarise(n_days = n(),# number of rows per year/site (number of observations (missing data or not))
            n_NA = sum(is.na(ppt)), # number of days with missing data
            ap = sum(ppt, na.rm = TRUE) #annual precipitation
  ) 

summary(ppt_annualV2)

# just "good years"
ppt_annual2V2 <- ppt_annualV2 %>% 
  mutate(n_good = n_days - n_NA) %>%  # number of good observations/year
  filter(n_good > 334)

ppt_summaryV2 <- ppt_annual2V2 %>% 
  group_by(id, site_code) %>% 
  summarize(n_good_yrs = n(), # number of good years per stations
            year_start = min(year), # first "good year"
            year_end = max(year),# last good year
            map = mean(ap),
            cv = CV(ap)) 


sum(ppt_summaryV2$n_good_yrs >= 30)  ### 7 sites have 'good' climate data that didn't before

ppt_summaryV2 %>% 
  filter(n_good_yrs >= 30) %>% 
  ggplot(aes(map, cv)) + 
  geom_point() +
  theme_classic()

plot(ppt_summaryV2$map, ppt_summaryV2$cv)
hist(ppt_summaryV2$n_good_yrs)

# sites with good wx data
good_sitesV2 <- ppt_summaryV2$site_code[ppt_summaryV2$n_good_yrs >= 30]

# sites with not sufficient good years
bad_sitesV2 <- site$id[!site$id %in% good_sitesV2]
bad_sitesV2
bad_sitesV2[!bad_sitesV2 %in% bad_sites]  #### two sites where we changed elevation are now 'bad' that weren't before
bad_sites[!bad_sites %in% bad_sitesV2]  #### nine sites were removed from 'bad' list


#### reload site to get to site names
sites <- read.csv(file.path(path, 'IDE Site Info/Sites_Loc_DrtTrt.csv'), as.is = TRUE)

sitesNoClim <- sites[sites$site_code %in% bad_sitesV2,]

##### Add in elevation and distance to nearest climate station THIS PULLS FROM OBJECT CREATED IN SCRIPT "DN site - GHCN station options"
##### and export sites that we feel comfortable with the data

precipOut <- merge(ppt_summaryV2, near5[,c('Station_ID','site_code','Distance','elev_diff')],by.y=c('site_code','Station_ID'),by.x=c('site_code','id'),all.x=T,all.y=F)
precipOut <- precipOut[!precipOut$site_code %in% bad_sitesV2,]

#write.csv(precipOut,file.path(path,'IDE Site Info/GHCN MAP-CV data 20190521.csv'))
