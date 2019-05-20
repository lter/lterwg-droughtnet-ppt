############################################################################
######## EXTRACT CLIMATE DATA FROM HGCN AND CALCULATE CV ACROSS YEAR #######
############################################################################

site <- read.csv('C:/Users/peter/Dropbox/IDE Meeting_May2019/IDE Site Info/Sites_Loc_DrtTrt.csv')
library(rnoaa)
library(dplyr)

site <- site[,c('site_code','lat','long')]
colnames(site) <- c('id','lat','long')


?meteo_nearby_stations()

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
  tmp <- ghcnd(staID)
  xx <- tmp[tmp$month==2,'VALUE28']
  
  tmp <- tmp[tmp$element == 'PRCP',]
  dVal <- tmp[substr(colnames(tmp),1,5) == 'VALUE']
    for(j in 1:nrow(dVal)){
      tmp2 <- dVal[j,]
      
    
      } 
  
  dVal$total <- rowSums(dVal,na.rm=T)
  
  
  tmp3 <- tmp[,c('id','year','month')]
  
  tmp4 <- cbind(dVal,tmp3$total)
  tmp4$site_code <- sc
  precip <- rbind(precip,tmp4)
}

precipAgg <- aggregate(list(precip = precip$VALUE1),by=list(site = precip$site_code,year=precip$year),FUN='sum')