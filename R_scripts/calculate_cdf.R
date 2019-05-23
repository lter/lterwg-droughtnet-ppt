#####################################################################
### script for estimating cumulative distribution functions by site ##
#######################################################################

# script started 5/23/19

library(tidyverse)
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

# data check
sapply(precip3, function(x){
  length(unique(x$site_code) != 1)
}) %>% sum() # should be zero

site_codes <- sapply(precip3, function(df){
  sc <- df$site_code[[1]]
  print(sc)
  stopifnot(length(unique(df$site_code)) == 1)
  
  sc
})

# ppt_df1 <- bind_rows(precip3)


