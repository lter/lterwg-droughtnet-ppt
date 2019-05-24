#####################################################################
### script for estimating cumulative distribution functions by site ##
### and calculating percentiles of annual precipitation of treatments#
### during experiment years. 
#######################################################################

# script started 5/23/19

library(tidyverse)
library(spatstat)

# path to data folders
path <- 'C:/Users/grad/Dropbox/IDE Meeting_May2019'


# load tpa precip data -----------------------------------------------------

# tpa data is stored in two folders:

# folder path (fist folder)
precip_folder1 <- file.path(path, "IDE Site Info/May_2019_TPA_uploads_annual_precip")

# 2nd folder 
precip_folder2 <- "C:\\Users\\grad\\Dropbox\\droughtnet_2018_felton\\droughtnet_ppt_deviations_material\\drought_net_trends_tpa"

file_names1 <- list.files(precip_folder1)
file_names2 <- list.files(precip_folder2)

precip1 <- lapply(file_names1, function(file_name){
  read.csv(file.path(precip_folder1, file_name), as.is = TRUE) %>% 
    select(site_code, year, totalPRE)
})

precip2 <- lapply(file_names2, function(file_name){
  read.csv(file.path(precip_folder2, file_name), as.is = TRUE) %>% 
    select(site_code, year, totalPRE)
})

precip3 <- c(precip1, precip2) # list of dataframes from both sites

# extracting site codes from each file
site_codes <- sapply(precip3, function(df){
  # check: all should have one unique site code
  stopifnot(length(unique(df$site_code)) == 1)
  
  sc <- df$site_code[[1]]
  
  sc
})

# nameing list elements
names(precip3) <- site_codes


#   -----------------------------------------------------------------------

# compute probability density function for each site, based on last 50 years

# density functions
# this is like a smoothed histogram
density <- lapply(precip3, function(df){
  ppt <- df %>% 
    filter(year > 1964) %>% 
    .$totalPRE
  d <- density(ppt)
  d
})

par(mfrow = c(2, 2))
walk2(density, site_codes, function(x, y){
  plot(x, main = y)
})

# compute cumulative density function -------------------------------------

# this ~ takes the area under the curve of the density function
# a smoothed version of the emperical cumulative density function (as one would get with ecdf())
cdf1 <- lapply(density, function(x){
  cdf <- CDF(x)
  cdf
})


# observed precipitation by site/year/trmt --------------------------------

site_ppt <- read.csv(file.path(path, "IDE Site Info\\Site-year precipitation by treatment.csv"), as.is = TRUE)

site_ppt2 <- site_ppt


# calculating percentiles of ap given annual precip -----------------------

site_ppt2$percentile <- NA
# "scruzh.us" still need
for (i in 1:nrow(site_ppt2)){
  site_code <- site_ppt2$site_code[i]
  ppt <- site_ppt2$ppt[i] # precip for the site/year
  print(site_code)
  # percentile based on cdf for the site
  site_ppt2$percentile[i] <- cdf1[[site_code]](ppt)
}

site_ppt2 %>% 
  filter(!(site_code %in% site_codes)) %>% 
  .$site_code %>% 
  unique()
