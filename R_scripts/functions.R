
# script start 5/20/19

# functions used in GHCN process script (among others as needed)


# take sum ----------------------------------------------------------------

sum_na <- function(x, num_nas = 15) {
  # args:
  #   x--numeric vector
  #   number of nas above which to return NA
  # returns:
  #   sum of x unless x has many NAs, then returns NA
  if(sum(is.na(x)) > num_nas){
    return(NA)
  }
  
  sum(x, na.rm = TRUE)
}

# make date ---------------------------------------------------------------

date_make <- function(year, month, day_of_month) {
  # args:
  #   year (integer)
  #   month (integer)
  #   day_of_month (integer)
  # returns:
  #   date (or NA, if aren't that many days in the month)
  # (doesn't account for leap years)
  days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  max_day <- days[month]
  if(day_of_month > max_day) {
    return(NA)
  }
  date <- paste(year, month, day_of_month, sep = "-") %>% 
    ymd()
  date
}


# convert ghcn data to long form ------------------------------------------

ghcn2long <- function(df, site_code) {
  # args:
  #   df -- ghcn dataframe, as returned by ghcnd function
  #   site_code --character vector
  # returns:
  #   cleaner data frame of daily data
  df_long <- df %>% 
    filter(element == "PRCP") %>% # precip data only
    select(id, year, month, contains("VALUE")) %>% # discarding other fields
    # converting data to long form:
    gather(key = "day_of_month", value = "ppt", matches("VALUE")) %>% 
    # extracting number from eg value21:
    mutate(day_of_month = str_extract(day_of_month, "\\d+$"),
           day_of_month = as.numeric(day_of_month),
           site_code = site_code)
  df_long
}


# download and clean ghcnd data -------------------------------------------

ghcn_download_parse <- function(df) {
  # args:
  #   df--data frame with cols id (id of weather station), name (of station, optional), 
  #     latitude, longitude (of station), distance (optional), site_code (of ide site)
  # returns:
  #   dataframe with daily data from all the stations in df
  nearest_df <- df
  precip <- NULL
  for(i in 1:nrow(nearest_df)){
    staID <- as.character(nearest_df[i,'id'])
    sc <- as.character(nearest_df[i,'site_code'])
    
    tmp <- ghcnd(staID)
    
    df_long <- ghcn2long(df = tmp, site_code = sc)
    
    precip <- rbind(precip, df_long)
    print(i)
  }
  precip
}

