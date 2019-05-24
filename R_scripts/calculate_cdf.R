#####################################################################
### script for estimating cumulative distribution functions by site ##
### and calculating percentiles of annual precipitation of treatments#
### during experiment years. 
#######################################################################

# specifically the aim is if a site (drought or control) received x mm of 
# precip during a 12 month period, what percentile is that relative to the 
# long term (50 years) tpa data set for the site. 


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
  print(file_name)
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

# nameing list elements (important for indexing later)
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

# run the following code if you want to visualize the density functions
# for each site:
# par(mfrow = c(2, 2))
# walk2(density, site_codes, function(x, y){
#   plot(x, main = y)
# })

# compute cumulative density function -------------------------------------

# this ~ takes the area under the curve of the density function
# a smoothed version of the emperical cumulative density function (which one would get with ecdf())
cdf1 <- lapply(density, function(x){
  cdf <- CDF(x)
  cdf
})


# observed precipitation by site/year/trmt --------------------------------

site_ppt <- read.csv(file.path(path, "IDE Site Info\\Site-year precipitation by treatment.csv"), as.is = TRUE)

site_ppt2 <- site_ppt


# calculating percentiles given annual precip --------------------------------

site_ppt2$percentile <- NA
# "scruzh.us" still need
for (i in 1:nrow(site_ppt2)){
  site_code <- site_ppt2$site_code[i]
  ppt <- site_ppt2$ppt[i] # precip for the site/year of interest
  
  # percentile based on cdf for the site
  f <-cdf1[[site_code]]
  
  if(is.null(f)){
    stop(paste("no tpa data was available for:", site_code))
  }
  site_ppt2$percentile[i] <- f(ppt)
}

site_ppt2$X <- NULL
# write.csv(site_ppt2, file.path(path, "IDE Site Info\\Site-year precipitation by treatment_percentile_added.csv"), row.names = FALSE)

# visualizing drought vs control ppt percentiles

# changing data frame so that there are two columns
# one for drt percentile one for control percentile
# on row for each site/year
site_perc_wide <- site_ppt2 %>% 
  select(-(ppt)) %>% 
  nest(trt, percentile, .key = "data") %>% 
  mutate(data = map(data, spread, key = "trt", value = "percentile")) %>% 
  unnest() %>% 
  rename(Ctrl_percentile = Ctrl, Drt_percentile = Drt)

# making two columns (drt/control) for annual precip
site_ppt_wide <- site_ppt2 %>% 
  select(-(percentile)) %>% 
  nest(trt, ppt, .key = "data") %>% 
  mutate(data = map(data, spread, key = "trt", value = "ppt")) %>% 
  unnest() %>% 
  rename(Ctrl_ppt = Ctrl, Drt_ppt = Drt)

# both annual precip and percentile in one dataframe:
site_wide <- full_join(site_perc_wide, site_ppt_wide,
                       by = c("site_code", "trt_year"))


# figures -----------------------------------------------------------------

g1 <- ggplot(site_wide) +
  facet_wrap(~trt_year) +
  theme_classic() +
  geom_abline (slope = 1, intercept = 0) +
  labs(subtitle = "Control vs Drought treatment by treatment year \n (where treatment year 1 has 30 - 365 days of treatment)",
       caption = paste("figure generated in 'calculate_cdf.R' script (on github) on",
                       lubridate::today())) + 
  theme(plot.title = element_text(size = 13))

# saves following figures (those before dev.off())
pdf(file.path(path, "IDE Site Info/Plots/trmt_vs_drt_precip20190524.pdf"), 
    height = 5, width = 8)

# ctrl vs drt percentiles
g1 + 
  geom_point(aes(Ctrl_percentile, Drt_percentile)) +
  labs(x = "Control precipitation percentile",
       y = "Drought precipitation percentile",
       title = "Percentile of annual precipitation")

# ctrl vs drt annual precip
g1 + 
  geom_point(aes(Ctrl_ppt, Drt_ppt)) +
  labs(x = "Control 12 month precipitation (mm)",
       y = "Drought 12 month precipitation (mm)",
       title = "Annual precipitation")

dev.off()
