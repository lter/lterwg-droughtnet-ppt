###############################################################################################
######## Create list of optional stations for each site based on distance and elevation #######
###############################################################################################

library(rnoaa)  ### NOAA weather station package


# path to data folders
path <- 'C:/Users/peter/Dropbox/IDE Meeting_May2019'

# site locations
site <- read.csv(file.path(path, 'IDE Site Info/Sites_Loc_DrtTrt.csv'))
siteElev <-read.csv(file.path(path, 'IDE Site Info/Site_Elev-Disturb.csv'))
siteElev <- siteElev[,c('site_code','elev')]

# function(s) used below
source('R_scripts/functions.R')

# NOAA function requires specific column names (site_code must be called 'id')
site <- site[,c('site_code','lat','long')]
colnames(site) <- c('id','lat','long')

# pull list of all GHCND stations (takes a while to run - output saved in csv below, better option is to load that)
#stations <- ghcnd_stations()

#write.csv(stations, file.path(path, 'IDE Site Info/GHCND_Stations.csv'))

# un-comment to load csv:
stations <- read.csv(file.path(path, 'IDE Site Info/GHCND_Stations.csv'), 
                     as.is = TRUE)

stations$X <- NULL # unnecessary column created in csv

### only want stations with ppt data and more than 50 years of data that are somewhat current
stationsPpt <- stations[stations$element == 'PRCP',]
stationsPpt <- stationsPpt[stationsPpt$first_year <= 1970 & stationsPpt$last_year >= 2000,]

### select the five nearest stations
nearStation <- meteo_nearby_stations(site,lat_colname='lat',lon_colname = 'long',station_data = stationsPpt,limit=5)

# convert to data.frame
near5 <- NULL
for(i in names(nearStation)){
  tmp <- as.data.frame(nearStation[i])
  tmp$rank <- seq(1,nrow(tmp))
  tmp$site_code <- i
  colnames(tmp) <- c('Station_ID','Station_name','Station_latitude','Station_longitutde','Distance','rank','site_code')  
  near5 <- rbind(near5,tmp)
}

## merge in site and station elevations
near5 <- merge(near5,siteElev,by='site_code',all=T)
colnames(near5) <- gsub('elev','site_elev',colnames(near5))
near5 <- merge(near5,stationsPpt[,c('id','elevation')],by.x='Station_ID',by.y='id',all.x=T,all.y=F)
colnames(near5) <- gsub('elevation','station_elev',colnames(near5))
near5$site_elev <- gsub('1 800', '1800',near5$site_elev) ### fix typo from csv
near5$elev_diff <- as.numeric(as.character(near5$site_elev)) - as.numeric(as.character(near5$station_elev)) ### relative to site (i.e. positive means site is above station)

near5[is.na(near5$elev_diff),]  ### Ethabuka missing elevation

### look for nearest station that may have problematic elevations (greater than 500 m elevation)

problemSites <- near5[near5$rank == 1 & abs(near5$elev_diff) > 500,]$site_code
near5[near5$site_code %in% problemSites,]

#### going to make subjective calls on better stations for each site
newStation <- NULL  ### create empty object to add new stations to

problemSites <- na.omit(problemSites)
near5[near5$site_code == problemSites[1],]
#### 5th nearest station to Stubai is 72 km away but has lowest elev diff (359 m lower)
newStation <- rbind(newStation, data.frame(site_code=problemSites[1],newStation = 'ITM00016008'))

near5[near5$site_code == problemSites[2],]
#### 3rd nearest station to Hong doubles distance (136 v 65) but lowers elevation diff to 59 m
newStation <- rbind(newStation, data.frame(site_code=problemSites[1],newStation = 'ITM00016008'))


near5[near5$site_code == 'stubai.at',]

nearest_df <- bind_rows(nearest)
nearest_df$site_code <- names(nearest)

nearest_df[nearest_df$distance > 100,]
nearest_df[nearest_df$distance < 20,]