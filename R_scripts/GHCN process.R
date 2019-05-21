############################################################################
######## EXTRACT CLIMATE DATA FROM HGCN AND CALCULATE CV ACROSS YEAR #######
############################################################################

site <- read.csv('C:/Users/peter/Dropbox/IDE Meeting_May2019/IDE Site Info/Sites_Loc_DrtTrt.csv')
source('R_scripts/functions.R')
<<<<<<< HEAD
CV <- function(x) sd(x)/mean(x) * 100

# path to data folders
path <- 'C:/Users/grad/Dropbox/IDE Meeting_May2019'

# site locations
site <- read.csv(file.path(path, 'IDE Site Info/Sites_Loc_DrtTrt.csv'), as.is = TRUE)

# function(s) used below
=======
>>>>>>> 17a84a01ce0952053c5f4fbcaa00f7c5d7e1cd40

library(rnoaa)
library(dplyr)

site <- site[,c('site_code','lat','long')]
colnames(site) <- c('id','lat','long')

<<<<<<< HEAD
# pull list of all GHCND stations (takes a while to run)
#stations <- ghcnd_stations() #

#write.csv(stations, file.path(path, 'IDE Site Info/GHCND_Stations.csv'))

# load csv:
stations <- read.csv(file.path(path, 'IDE Site Info/GHCND_Stations.csv'), as.is = TRUE)
=======

?meteo_nearby_stations()
>>>>>>> 17a84a01ce0952053c5f4fbcaa00f7c5d7e1cd40

#stations <- ghcnd_stations()
#write.csv(stations,'GHCND_Stations.csv')

stationsPpt <- stations[stations$element == 'PRCP',]
stationsPpt <- stationsPpt[stationsPpt$first_year <= 1980 & stationsPpt$last_year >= 2000,]


nearStation <- meteo_nearby_stations(site,lat_colname='lat',lon_colname = 'long',station_data = stationsPpt,limit=3)

nearest <- lapply(nearStation, function(x){
  x[1, ]
})

nearest_df <- bind_rows(nearest)
nearest_df$site_code <- names(nearest)

nearest_df[nearest_df$distance > 100,]
nearest_df[nearest_df$distance < 20,]


#nearest_test <- nearest_df[1:5,]

precip <- NULL
for(i in 1:nrow(nearest_df)){
  staID <- as.character(nearest_df[i,'id'])
  sc <- as.character(nearest_df[i,'site_code'])
  distance <- as.numeric(as.character(nearest_df[i,'distance']))
  tmp <- ghcnd(staID)

  tmp <- tmp[tmp$element == 'PRCP',]
  dVal <- tmp[substr(colnames(tmp),1,5) == 'VALUE']
  mSums <- apply(dVal,1,sum_na)
    
  
  # dVal$total <- rowSums(dVal,na.rm=T)
  
  dfOut <- tmp[,c('id','year','month')]
  dfOut$ppt <- mSums/10
  dfOut$site_code <- sc
  dfOut$distance <- distance
  # tmp3 <- tmp[,c('id','year','month')]
  # 
  # tmp4 <- cbind(dVal,tmp3$total)
  # tmp4$site_code <- sc
  precip <- rbind(precip,dfOut)
  print(i)
}

precipFull <- precip

#### retain only records from the last 100 years prior to beginning of DN
precip <- precipFull[precipFull$year >= 1915,]

### remove months with NA for now
precip <- na.omit(precip)

precip

precipAgg <- aggregate(list(ppt = precip$ppt),by=list(site_code = precip$site_code,year=precip$year),FUN='sum')
MAP <- aggregate(list(MAP = precipAgg$ppt),by=list(site_code = precipAgg$site_code),FUN='mean', na.action = na.omit)

CV <- function(x) sd(x)/mean(x) * 100
pptCV <- aggregate(list(CV = precipAgg$ppt),by=list(site_code = precipAgg$site_code),FUN='CV')

dfPpt <- merge(MAP,pptCV,by='site_code')
#write.csv(dfPpt, 'C:/Users/peter/Dropbox/IDE Meeting_May2019/IDE Site Info/GHCN MAP-CV data PRELIMINARY 20190520.csv')

plot(dfPpt$CV~dfPpt$MAP)

<<<<<<< HEAD
saveRDS(bad_sites, "site_names_needWx_data.rds")
#write.csv(ppt_summary, file.path(path, 'IDE Site Info/GHCN MAP-CV data PRELIMINARY 20190521.csv'))
=======
sum(tapply(precip$year,precip$site_code,'max') >= 2016)
pptNA <- precipFull %>% group_by(site_code) %>% 
  summarise(nacount = sum(is.na(ppt))) %>% 
  arrange(desc(nacount))

pptNA <- merge(pptNA,data.frame(months=tapply(precipFull$id,precipFull$site_code,'length')),by.x='site_code',by.y=0)
>>>>>>> 17a84a01ce0952053c5f4fbcaa00f7c5d7e1cd40

pptNA$percentNA <- pptNA$nacount/pptNA$months

### list of weather stations to omit
sitesNA <- pptNA[pptNA$percentNA > 0.1,]$site_code

