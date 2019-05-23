#############################################################################################
###### Calculate precipitation reduction based on daily precip and shelter installation  ####
#############################################################################################

path <- 'C:/Users/peter/Dropbox/IDE Meeting_May2019'

precip <- read.csv(file.path(path,'IDE Site Info/GHCN_daily_precip_preliminary20190522.csv'))

siteDrt <- read.csv(file.path(path,'IDE Site Info/Sites_Loc_DrtTrt.csv'))
siteDate <- read.csv(file.path(path,''))