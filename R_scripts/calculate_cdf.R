#####################################################################
### script for estimating cumulative distribution functions by site ##
### and calculating percentiles of annual precipitation of treatments#
### during experiment years. 
#######################################################################

# specifically the aim is if a site (drought or control) received x mm of 
# precip during a 12 month period, what percentile is that relative to the 
# long term (50 years) tpa data set for the site. 
# percentiles calculated using emperical cdf and normal approximated cdf

# script started 5/23/19


# packages ----------------------------------------------------------------

library(tidyverse)
library(spatstat)

# path to data folders
path_may <- 'E:/Dropbox/IDE Meeting_May2019'
path_oct <- 'E:/Dropbox/IDE Meeting_Oct2019'

# load tpa precip data -----------------------------------------------------

# tpa data is stored in two folders:

# folder path (fist folder)
precip_folder1 <- file.path(path_may, "IDE Site Info/May_2019_TPA_uploads_annual_precip")

# 2nd folder 
precip_folder2 <- "E:\\Dropbox\\droughtnet_2018_felton\\droughtnet_ppt_deviations_material\\drought_net_trends_tpa"

file_names1 <- list.files(precip_folder1)
file_names2 <- list.files(precip_folder2)

precip1 <- lapply(file_names1, function(file_name){
  print(file_name)
  read.csv(file.path(precip_folder1, file_name), as.is = TRUE) %>% 
    dplyr::select(site_code, year, totalPRE)
})

precip2 <- lapply(file_names2, function(file_name){
  read.csv(file.path(precip_folder2, file_name), as.is = TRUE) %>% 
    dplyr::select(site_code, year, totalPRE)
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

site_ppt <- read.csv(
  file.path(path_oct, 'data/precip/precip_by_trmt_year_2019-12-03.csv'), 
  as.is = TRUE)

site_ppt2 <- site_ppt


# calculating percentiles given annual precip --------------------------------

# "scruzh.us" still need
for (i in 1:nrow(site_ppt2)){
  print(i)
  site_code <- site_ppt2$site_code[i]
  print(site_code)
  ppt_ambient <- site_ppt2$ppt_ambient[i] # precip for the site/year of interest
  ppt_drought <- site_ppt2$ppt_drought[i]
  # percentile based on cdf for the site
  f <-cdf1[[site_code]]
  
  if(is.null(f)){
    warning(paste("no tpa data was available for:", site_code))
    site_ppt2$perc_ambient_obs[i] <- NA
    site_ppt2$perc_drought_obs[i] <- NA
    site_ppt2$perc_ambient_norm[i] <- NA
    site_ppt2$perc_drought_norm[i] <- NA
  } else {
    
    tpa_ppt <- precip3[[site_code]]%>% 
      filter(year > 1964) %>% 
      .$totalPRE
    
    site_ppt2$perc_ambient_obs[i] <- f(ppt_ambient)*100 # convert quantile to percentile
    site_ppt2$perc_drought_obs[i] <- f(ppt_drought)*100 # convert quantile to percentile
    # normal approximations
    site_ppt2$perc_ambient_norm[i] <- pnorm(ppt_ambient, 
                                            mean = mean(tpa_ppt,na.rm=T),
                                            sd = sd(tpa_ppt,na.rm=T))*100
    site_ppt2$perc_drought_norm[i] <- pnorm(ppt_drought, 
                                            mean = mean(tpa_ppt,na.rm=T),
                                            sd = sd(tpa_ppt,na.rm=T))*100
    
  }
}


# figures -----------------------------------------------------------------

# percentiles normal approx vs imperical

# pdf(file.path(path_oct,
#               paste0("figures/precip/percentiles_emperical_vs_normal", today(), ".pdf")
#               ))

plot(site_ppt2$perc_ambient_norm, site_ppt2$perc_ambient_obs,
     xlab = "Ambient precip percentile from normal CDF",
     ylab = "Ambeint precip percentile from emperical CDF")
abline(0, 1)
dev.off()

# since normal and emperical percentiles are nearly the same, just keeping the normal

site_ppt3 <- site_ppt2 %>% 
  rename(perc_ambient = perc_ambient_norm,
         perc_drought = perc_drought_norm) %>% 
  dplyr::select(-perc_ambient_obs, -perc_drought_obs)

# ~~~

g1 <- site_ppt3 %>% 
  filter(n_treat_years > 0) %>% 
  mutate(treat_year_bin = ifelse(n_treat_years == 1,
                                 "First treatment year",
                                 "Second + treatment years")
  ) %>% 
  ggplot() +
  theme_classic() +
  facet_wrap(~treat_year_bin) +
  geom_abline (slope = 1, intercept = 0) +
  labs(subtitle = "Control vs Drought treatment by treatment years \n (where treatment year 1 has 30 - 365 days of treatment)",
       caption = "figure generated in 'calculate_cdf.R' script ") + 
  theme(plot.title = element_text(size = 13))


# pdf(file.path(path_may, "IDE Site Info/Plots/trmt_vs_drt_precip20190524.pdf"), 
#     height = 5, width = 8)

image_path <- file.path(
  path_oct,
  paste0("figures/precip/ambient_vs_drought_precip_", today(), ".pdf"))

# pdf(image_path,  height = 5, width = 8)

# ctrl vs drt percentiles
g1 + 
  geom_point(aes(perc_ambient, perc_drought)) +
  labs(x = "Control precipitation percentile",
       y = "Drought precipitation percentile",
       title = "Percentiles of annual precipitation")

# ctrl vs drt annual precip
g1 + 
  geom_point(aes(ppt_ambient, ppt_drought)) +
  labs(x = "Control 12 month precipitation (mm)",
       y = "Drought 12 month precipitation (mm)",
       title = "Annual precipitation")

dev.off()


# saving the data (csv) ---------------------------------------------------

# write_csv(site_ppt3,
#           file.path(path_oct, 'data/precip/precip_by_trmt_year_with_percentiles_2019-12-03.csv'))
