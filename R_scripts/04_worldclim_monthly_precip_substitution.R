#library(raincpc)
library(raster)
library(rgdal)

source("R_scripts/functions.R")
source("R_scripts/dropbox_path.R") # where path to dropbox should be set
path_oct <- file.path(path, "IDE Meeting_Oct2019")

old_dir <- getwd()

setwd(file.path(path_oct, "data/precip/wc2.0_30s_prec"))

#cpc_get_rawdata(2014,1,1,2018,12,31) ### downloads data into folder. Each day is about 2 Mb

### world clim approach

p1 <- newest_file_path(
  path = file.path(path_oct, "data/precip"),
  file_regex = "submitted_weather_station_info_\\d{4}-\\d{2}-\\d{2}.csv")
p1
sta <- read.csv(p1)


# this is the combined precip file that has bad values (e.g. negative) removed:
p2 <- newest_file_path(
  path = file.path(path_oct, "data/precip"),
  file_regex = "submitted_daily_weather_bad_vals_removed_\\d{4}-\\d{2}-\\d{2}.csv")

precip <- read.csv(p2)



sta <- sta[!sta$site_code=='unknown',]
### convert to 0 to 360 longitude that CPC uses
#sta$station_longitud <- ifelse(sta$station_longitud < 0, sta$station_longitud + 360,sta$station_longitud)
latlon <- sta[,c('site_code','station_longitud','station_latitud')]
###remove duplicate sites for now
latlon <- latlon[!duplicated(latlon$site_code),]



for(i in c('01','02','03','04','05','06','07','08','09','10','11','12')){
  tmp = raster(paste("wc2.0_30s_prec_",i,'.tif',sep=''))
    assign(paste('p',i,sep=''),tmp)
}

latlon$Jan <- raster::extract(p01,latlon[,2:3])
latlon$Feb <- raster::extract(p02,latlon[,2:3])
latlon$Mar <- raster::extract(p03,latlon[,2:3])
latlon$Apr <- raster::extract(p04,latlon[,2:3])
latlon$May <- raster::extract(p05,latlon[,2:3])
latlon$Jun <- raster::extract(p06,latlon[,2:3])
latlon$Jul <- raster::extract(p07,latlon[,2:3])
latlon$Aug <- raster::extract(p08,latlon[,2:3])
latlon$Sep <- raster::extract(p09,latlon[,2:3])
latlon$Oct <- raster::extract(p10,latlon[,2:3])
latlon$Nov <- raster::extract(p11,latlon[,2:3])
latlon$Dec <- raster::extract(p12,latlon[,2:3])

latlon$map <- rowSums(latlon[,4:15])


wcOut <- NULL
for(j in unique(latlon$site_code)){
  site <- precip[precip$site_code == j,]
  site <- site[order(site$date),]
  
  siteY <- site[!is.na(site$precip),]
  if(nrow(siteY) == 0) next 
  siteY$wc <- 'N'
  siteNA <- site[is.na(site$precip),]
  if(nrow(siteNA) == 0) siteOut <- siteY else {
  
  siteNA$wc <- 'Y'
  siteNA$month <- substr(siteNA$date,6,7)
  siteNA$year <- substr(siteNA$date,1,4)
  
  
  siteNA$precip <- ifelse(siteNA$month == '01',latlon[latlon$site_code==j,'Jan']/31,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '02' & siteNA$year %in% c('2004','2008','2012','2016'),
                          latlon[latlon$site_code==j,'Feb']/29,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '02' & !siteNA$year %in% c('2004','2008','2012','2016'),
                          latlon[latlon$site_code==j,'Feb']/28,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '03',latlon[latlon$site_code==j,'Mar']/31,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '04',latlon[latlon$site_code==j,'Apr']/30,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '05',latlon[latlon$site_code==j,'May']/31,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '06',latlon[latlon$site_code==j,'Jun']/30,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '07',latlon[latlon$site_code==j,'Jul']/31,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '08',latlon[latlon$site_code==j,'Aug']/31,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '09',latlon[latlon$site_code==j,'Sep']/30,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '10',latlon[latlon$site_code==j,'Oct']/31,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '11',latlon[latlon$site_code==j,'Nov']/30,siteNA$precip)
  siteNA$precip <- ifelse(siteNA$month == '12',latlon[latlon$site_code==j,'Dec']/31,siteNA$precip)
  
  siteOut <- rbind(siteY,siteNA[,1:10])
  siteOut <- siteOut[order(siteOut$date),]
  }
  wcOut <- rbind(wcOut,siteOut)
  
  }
  
write.csv(
  wcOut, 
  file.path(path_oct, 'data/precip/submitted_daily_weather_WC_supplemented_2022-04-06.csv')
  )

setwd(old_dir)
