
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

ghcn_download_parse <- function(df, return_list = FALSE) {
  # args:
  #   df--data frame with cols id (id of weather station), name (of station, optional), 
  #     latitude, longitude (of station), distance (optional), site_code (of ide site)
  # returns:
  #   dataframe with daily data from all the stations in df
  nearest_df <- df
  precip <- NULL
  if (return_list) {
    precip <- list()
  }
  for (i in 1:nrow(nearest_df)){
    staID <- as.character(nearest_df[i,'id'])
    sc <- as.character(nearest_df[i,'site_code'])
    
    tmp <- ghcnd(staID)
    
    df_long <- ghcn2long(df = tmp, site_code = sc)
    
    if (return_list){
      precip[[sc]] <- df_long
    } else {
      precip <- rbind(precip, df_long)
    }
    
    print(i)
  }
  precip
}


# parse dates etc of ghcn file --------------------------------------------

ghcn_parse_dates <- function(df) {
  # args:
  #   df--specific data frame as returned by ghcn_download_parse()
  # returns:
  #   data frame (with only real dates)
  df %>% 
    arrange(site_code, year, month, day_of_month) %>% 
    mutate(date = lubridate::make_date(year, month, day_of_month),
           ppt = ppt/10) %>% # convert 10ths of mm to mm
    filter(!is.na(date)) # e.g 30th day of february
}


# compute number of good measurements/year --------------------------------

good_days_per_yr <- function(df, good_days = 334) {
  # args:
  #   df--data frame with id (of wx station, site_code, year, and ppt (precip))
  #       this should be daily data
  #   good_days -- number of valid days of measurement in a year for it to 
  #       be considered good
  # returns:
  #   data frame including number of good (non NA) precip measurements per year
  out <- df %>% 
    group_by(id, site_code, year) %>% 
    # number of rows per year/site (number of observations (missing data or not))
    summarise(n_days = n(),
              n_NA = sum(is.na(ppt)), # number of days with missing data
              ap = sum(ppt, na.rm = TRUE) # annual precipitation
    ) %>% 
    mutate(n_good = n_days - n_NA,
           is_good = ifelse(n_good > good_days, TRUE, FALSE))
  out
}


# check column names ------------------------------------------------------

check_names <- function(x, names) {
  # args:
  #   x--list
  #   names <- string of list element names in alphabetical order 
  #     (seperated by ,)
  # returns:
  #  logical of whether x has (and only has) elements with those specified names
  stopifnot(
    is.list(x),
    is.character(names),
    length(names) == 1
  )
  x <- names(x)
  x <- sort(x)
  collapsed <- paste(x, collapse = ",")
  out <- collapsed == names
  out
}

