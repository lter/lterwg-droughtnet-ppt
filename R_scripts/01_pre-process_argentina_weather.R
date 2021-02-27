# Script for pre-processing argentina submitted weather, so that it fits into the
# processing pipeline in process_submitted_weather.R script.


# Script started 1/26/20


# The purpose here is to convert monthly precip values into average daily precip 
# and make other formatting changes etc. 


# packages etc ------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
source("R_scripts/functions.R")


path_may <- "~/Dropbox/IDE Meeting_May2019"
path_oct <- '~/Dropbox/IDE Meeting_Oct2019'

# load data ---------------------------------------------------------------

p1 <- file.path(path_may, "IDE_weather\\submitted_data\\argentina\\ANPP+PPT_Sitios_Arg.xlsx")


df1 <- read_xlsx(p1, sheet = "compilacion total sitios SA")

names(df1)

temp_sheets <- c("metadata", "sites", "station", "weather")
temp1 <- map(temp_sheets, function(sheet) {
  read_xlsx(file.path(path_may, "IDE_weather/IDE_weather_template.xlsx"),
            sheet = sheet)
})
names(temp1) <- temp_sheets



# parse the wide format ---------------------------------------------------


df2 <- df1 %>% 
  select(-c("Live Biomass  Control", "Total Biomass  Control")) %>% 
  rename(bio_date = `Harvest date`,
         site = Site) %>% 
  mutate(bio_date = parse_if_5digit_date(bio_date))

# fill in site names (first instance name provided, the rest are NAs)

previous_site <- df2$site[1]
for (i in 1:nrow(df2)) {
  if (is.na(df2$site[i])) {
    df2$site[i] <- previous_site
  }
  previous_site <- df2$site[i]
}

# parse biomass dates

# throws ~84 failed to parse error--this is ok here

# from note in excel file could tell that this should be sept
df2$bio_date[df2$bio_date == 2015] <- "September 2015"

df3 <- df2 %>% 
  mutate(month = str_extract(bio_date, "[A-z]+"),
         year = str_extract(bio_date, "\\d{4}"),
         # if just month and year given assume middle of month
         m_d_y = paste(month, "15,", year),
         m_d_y = ifelse(is.na(month), NA, as.character(mdy(m_d_y))),
         bio_date = ifelse(is.na(month), bio_date, m_d_y),
         bio_date = convert_mdy(bio_date),
         bio_date = ymd(bio_date)
  ) %>% 
  select(-month, -year, -m_d_y)
df3$bio_date


# convert to long form and add months -------------------------------------


df4 <- df3 %>% 
  gather(key = "key", value = "precip", matches("pre harvest$"), "Sampling month") %>% 
  mutate(month_num = str_extract(key, "\\d{1,2}"),
         month_num = ifelse(key == "Sampling month", "0", month_num),
         month_num = as.numeric(month_num)) 
# subtracting number of months prior to biomass harvest
df4$ppt_date <- df4$bio_date  %m-% months(df4$month_num)
df4 <- df4 %>% 
  mutate(month = month(ppt_date),
         year = year(ppt_date)) %>% 
  select(-ppt_date, -month_num) %>% 
  arrange(site, year, month)

# duplicates b/ multiple rows for each bio_date 
df5 <- df4 %>% 
  select(site, precip, month, year) %>% 
  .[!duplicated(.), ]

sum(duplicated(df5[, c("site", "month", "year")]))


dup <- df5 %>% 
  filter(duplicated(df5[, c("site", "month", "year")]))
# looking at the original data there is an off by 1 month error 
# in either the 2016 or 2015 row.
# just averaging here
df5 %>% 
  filter(month %in% dup$month, year %in% dup$year, site %in% dup$site)

df6 <- df5 %>% 
  group_by(site, month, year) %>% 
  summarise(precip = mean(precip)) %>% 
  ungroup()


# monthly to daily data ---------------------------------------------------

dly1 <- df6 %>% 
  mutate(station_name = site) %>% 
  group_by(site) %>% 
  nest() %>% 
  rename(df = data) %>% 
  mutate(df = map(df, 
                      monthly2daily_precip, 
                      year = "year",
                      month = "month",
                      precip = "precip",
                      station_name = TRUE),
         # otherwise returned as factor--causes issues with unnest
         df = map(df, function(x) {
           x$station_name = as.character(x$station_name)
           as_tibble(x)
         })) %>% 
  .$df %>% 
  bind_rows()


# prepping excel file -----------------------------------------------------

# sites sheet
sites <- tibble(
  pi = NA,
  site = unique(df6$site),
  site_latitud = NA,
  site_longitud = NA
)

temp2 <- map(temp1, function(df){
  map(df, function(x) {
    rep(NA, length(x))
  }) %>% 
  as_tibble()
})
temp2$metadata <- temp1$metadata

# saving for use in other scripts
saveRDS(temp2, file.path(path_may, "IDE_weather/IDE_weather_template.RDS"))

n <- length(sites$site)

station <- temp1$station %>% 
  map(function(x) {
    rep(NA, n)
  }) %>% 
  as_tibble() %>% 
  mutate(site = sites$site,
         station_name = sites$site)

weather <- dly1 %>% 
  mutate(min_temp = NA,
         max_temp = NA,
         note_weather = "monthly precip values used")

weather %>% 
  select(station_name, date) %>% 
  duplicated() %>% 
  sum()

weather[duplicated(weather[, c("station_name", "date")]), ]
# saving file -------------------------------------------------------------

out <- list("metadata" = temp1$metadata,
            "sites" = sites,
            "station" = station,
            "weather" = weather)
dest <- file.path(path_may, "IDE_weather/submitted_data/argentina_weather_clean.xlsx")

# openxlsx::write.xlsx(out,dest)
