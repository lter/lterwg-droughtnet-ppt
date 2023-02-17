# Purpose:
# compile csv files created by the 05_precipitation reduction calculations.R
# scripted, and summarize the data in simpler manner
# this script is then sourced by downstream scripts

# Author: Martin Holdrege

# Date started: Jan 16, 2023


# dependencies ------------------------------------------------------------

library(tidyverse)
source("R_scripts/dropbox_path.R")
source("R_scripts/functions.R")

# read in data ------------------------------------------------------------

# number of days before biomass harvest for which precipitation totals were
# calculated (i.e. the lag)
lags <- c("365-0days", "730-365days", "1095-730days", "1460-1095days")
names(lags) <- paste0("lag", str_replace(lags, "-","_"))

# getting file paths (newest version of each file)
files <- map(lags, function(x) {
  regex <- paste0('anpp_clean_trt_ppt_no-perc_', x,
                  '_\\d{4}-\\d+-\\d+.csv')

  out <- newest_file_path(
    path = file.path(path, "IDE Meeting_Oct2019/data/precip"),
    file_regex = regex)
  
  out

})

# checking if file names all contain the same date--if not could
# mean that files were created with different versions of the
# 05_...R script, or with different inputs to that script.
dates_files_created <- map_chr(files, str_extract, 
                               pattern = "\\d{4}-\\d+-\\d+(?=\\.csv$)")

if (length(unique(dates_files_created)) != 1) {
  warning("dates on which input ppt files were created are not equal\n",
          paste(dates_files_created, collapse = "\n"))
}

# actually reading in the files
ppt1 <- map_dfr(files, read_csv, show_col_types = FALSE,
                .id = "lag")

# simplify data -----------------------------------------------------

ppt2 <- ppt1 %>% 
  # should never be true anymore, but filtering to be safe
  filter(!annual_ppt_used) %>%
  mutate(lag = lags[lag] # just renaming values
         ) %>% 
  group_by(site_code, year, trt, lag) %>% 
  filter(biomass_date == max(biomass_date)) %>% 
  select(site_code, year, trt, lag, ppt_source,
         matches("^ppt_", perl = TRUE), -ppt_num_wc_interp_sub) %>% 
  rename(ppt_used = ppt_source) %>% 
  distinct() 

# creating rows of ppt for each data source
ppt3 <- ppt2 %>% 
  pivot_longer(matches("^ppt_(?!used)", perl = TRUE)) %>% 
  mutate(data_source = str_extract(name, "[a-z]+$"),
         name = ifelse(str_detect(name, "num_NA"),
                           'num_NA',
                           'ppt')) %>% 
  pivot_wider() %>% 
  mutate(data_source = str_replace(data_source, "ppt_", ""),
         # to match naming convention in ppt_used
         data_source = ifelse(data_source == "sub", "submitted", data_source),
         # was the given data source (row) used for the main
         # analysis (i.e. where submitted or ghcn data was used
         # if available, otherwise chirps or mswep)
         was_source_used = ppt_used == data_source) %>% 
  select(-ppt_used)
  
# check
test <- ppt3 %>% 
  select(site_code, year, trt, lag, data_source) %>% 
  distinct()

if(nrow(test) != nrow(ppt3)) {
  stop('likely problem with filtering, joining, etc')
}

# object to be used in downstream scrips ----------------------------------

ppt4 <- ppt3 %>% 
  # to avoid inadvertantly using bad data downstream
  mutate(ppt = ifelse(num_NA >30, NA, ppt))

# to be used downstream
ppt_comb1 <- ppt4 %>% 
  ungroup()
