############################################################################
######## EXTRACT CLIMATE DATA FROM HGCN AND CALCULATE CV ACROSS YEAR #######
############################################################################

# path to data folders
path <- 'C:/Users/peter/Dropbox/IDE Meeting_May2019'

# site locations
site <- read.csv(file.path(path, 'IDE Site Info/Sites_Loc_DrtTrt.csv'))

# function(s) used below
source('R_scripts/functions.R')

library(rnoaa)
library(dplyr)

site <- site[,c('site_code','lat','long')]
colnames(site) <- c('id','lat','long')

# pull list of all GHCND stations (takes a while to run)
#stations <- ghcnd_stations()

#write.csv(stations, file.path(path, 'IDE Site Info/GHCND_Stations.csv'))

# un-comment to load csv:
stations <- read.csv(file.path(path, 'IDE Site Info/GHCND_Stations.csv'), 
                     as.is = TRUE)

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

  tmp <- tmp[tmp$element == 'PRCP',]
  dVal <- tmp[substr(colnames(tmp),1,5) == 'VALUE']
  mSums <- apply(dVal,1,sum_na)
    
  
  # dVal$total <- rowSums(dVal,na.rm=T)
  
  dfOut <- tmp[,c('id','year','month')]
  dfOut$ppt <- mSums/10
  dfOut$site_code <- sc
  
  # tmp3 <- tmp[,c('id','year','month')]
  # 
  # tmp4 <- cbind(dVal,tmp3$total)
  # tmp4$site_code <- sc
  precip <- rbind(precip,dfOut)
  print(i)
}

# maybe we should save this raw monthly (precip) data file?

precipFull <- precip
precip <- na.omit(precip)

# annual precip by year and site
precipAgg <- aggregate(list(ppt = precip$ppt),by=list(site_code = precip$site_code,year=precip$year),FUN='sum')

# mean annual precip by site
MAP <- aggregate(list(MAP = precipAgg$ppt),by=list(site_code = precipAgg$site_code),FUN='mean', na.action = na.omit)

CV <- function(x) sd(x)/mean(x) * 100

# interannual CV
pptCV <- aggregate(list(CV = precipAgg$ppt),by=list(site_code = precipAgg$site_code),FUN='CV')

dfPpt <- merge(MAP,pptCV,by='site_code')
#write.csv(dfPpt, file.path(path, 'IDE Site Info/GHCN MAP-CV data PRELIMINARY 20190520.csv'))

plot(dfPpt$CV~dfPpt$MAP)

sum(tapply(precip$year,precip$site_code,'max') >= 2016)

# the number of months where precip is NA (data checking)
pptNA <- precipFull %>% 
  group_by(site_code) %>% 
  summarise(nacount = sum(is.na(ppt))) %>% 
  arrange(desc(nacount))



pptNA <- merge(pptNA,data.frame(months=tapply(precipFull$id,precipFull$site_code,'length')),by.x='site_code',by.y=0)

pptNA$percentNA <- with(pptNA, nacount/months)
### list of weather stations to omit
sitesNA <- pptNA[pptNA$percentNA > 0.1, ]$site_code
sitesNA
