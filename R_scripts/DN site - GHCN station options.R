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
nearStation <- meteo_nearby_stations(site,lat_colname='lat',lon_colname = 'long',station_data = stationsPpt,limit=10)

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
#### 7th nearest station to Stubai is 82 km away but has lowest elev diff (12 m higher)
newStation <- rbind(newStation, data.frame(site_code=problemSites[1],
                                           newStation = near5[near5$site_code == problemSites[2] & near5$rank==7,]$Station_ID))

near5[near5$site_code == problemSites[2],]
#### 3rd nearest station to Hong doubles distance (136 v 65) but lowers elevation diff to 59 m
newStation <- rbind(newStation, data.frame(site_code=problemSites[2],
                                           newStation = near5[near5$site_code == problemSites[2] & near5$rank==3,]$Station_ID))

near5[near5$site_code == problemSites[3],]
#### 5th nearest station to Validate reduces elevation difference to 10 m (big increase in distance though - 12km to 157km)
newStation <- rbind(newStation, data.frame(site_code=problemSites[3],
                                           newStation = near5[near5$site_code == problemSites[3] & near5$rank==5,]$Station_ID))

near5[near5$site_code == problemSites[4],]
#### No appreciable differences in elevation in 10 nearest stations. Don't add new station, first station is 879 m lower though!
#newStation <- rbind(newStation, data.frame(site_code=problemSites[3],
#                                           newStation = near5[near5$site_code == problemSites[3] & near5$rank==5,]$Station_ID))
#### CONTACT PINETA.ES ABOUT BETTER STATION OPTION ####

near5[near5$site_code == problemSites[5],]
#### No appreciable differences in elevation in 10 nearest stations. Don't add new station, first station is 1003 m lower though!
#newStation <- rbind(newStation, data.frame(site_code=problemSites[3],
#                                           newStation = near5[near5$site_code == problemSites[3] & near5$rank==5,]$Station_ID))
#### CONTACT TORLA.ES ABOUT BETTER STATION OPTION ####

near5[near5$site_code == problemSites[6],]
#### 8th nearest station to Passogavia reduces elevation difference to 179 m (132 km away)
newStation <- rbind(newStation, data.frame(site_code=problemSites[6],
                                           newStation = near5[near5$site_code == problemSites[6] & near5$rank==8,]$Station_ID))

near5[near5$site_code == problemSites[7],]
#### No appreciatvle improvement in elevation for QDTNORTH and elev difference is JUST on the cusp anyways (535 m)
#newStation <- rbind(newStation, data.frame(site_code=problemSites[6],
#                                           newStation = near5[near5$site_code == problemSites[6] & near5$rank==8,]$Station_ID))

near5[near5$site_code == problemSites[8],]
#### No appreciatvle improvement in elevation for QDTSOUTH and elev difference is JUST on the cusp anyways (535 m)
#newStation <- rbind(newStation, data.frame(site_code=problemSites[6],
#                                           newStation = near5[near5$site_code == problemSites[6] & near5$rank==8,]$Station_ID))

near5[near5$site_code == problemSites[9],]
#### 10th nearest station to Prades reduces elevation difference to 179 m but increases distance to > 200 km, no other good options
#### CONTACT PRADES.ES ABOUT BETTER STATION OPTION ####

near5[near5$site_code == problemSites[10],]
#### 5th nearest station to Validate reduces elevation difference to 10 m (big increase in distance though - 12km to 157km)
newStation <- rbind(newStation, data.frame(site_code=problemSites[3],
                                           newStation = near5[near5$site_code == problemSites[3] & near5$rank==5,]$Station_ID))


near5[near5$site_code == 'stubai.at',]

nearest_df <- bind_rows(nearest)
nearest_df$site_code <- names(nearest)

nearest_df[nearest_df$distance > 100,]
nearest_df[nearest_df$distance < 20,]