
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



# test if all dfs in list have proper cols --------------------------------


check_names_in_list <- function(list, element_name, names, warning) {
  # args:
  #   list--a list element
  #   element_name--name of element in sublist
  #   names--string--column names to check for match (in sublist)
  #   warning--warning to return if doesn't match
  # returns:
  #   warning or all ok message (ie did the df of interest have the proper column names)
  stopifnot(
    is.list(list),
    is.character(element_name),
    is.character(names),
    is.character(warning)
  )
  
  if(!all(
    map2_lgl(list, names(list), function(x, element) {
      out <- check_names(x[[element_name]], names = names)
      if(!out) warning (paste("issue with:",element)) # print if check names False
      out
    }))
  ) {
    warning(warning)
  } else {
    message("check passed")
  }
}


# extract list tables and combine -----------------------------------------


extract_elements_2df <- function(list, element) {
  # list--a list (ie list of lists)
  # element--name of table in each list element that interested in
  # returns:
  #    tibble, of combined list elements, with new col added (of name of list elements)
  stopifnot(
    is.list(list),
    is.character(element)
  )
  bind_rows(
    map2(list, names(list), function(x, name) {
      x[[element]] %>% 
        mutate(file_name = name)
    }) 
  )
}

# see if date will parse --------------------------------------------------

parse_date_warn <- function(x) {
  # args:
  #   x--vector to parse to date
  # returns:
  #   warning or lack there of from parse_date
  #   the point is to see whether this vector will parse without a warning
  parse_date_quietly <- quietly(ymd)
  out <- parse_date_quietly(x)$warning
  out
}


# convert 5 digit date from excel -----------------------------------------

# excel  returns date as a number (days since origin)--so when read into R just that number is given
# Note there is some risk here b/ Excel 2008 for Mac and earlier Excel for Mac 
# versions calculate dates based on the 1904 date system (not 1900 like every thing else)
# first check origin of file by using openxlsx::getDateOrigin

parse_if_5digit_date <- function(x) {
  # args:
  #   x--char vector
  # returns:
  #   character vector--if value is a 5 digit number, returns date
  #   otherwise returns original value
  stopifnot(is.character(x))
  
  is_5digit <- str_detect(x, "^\\d{5}$")

  out <- x
  # see https://stat.ethz.ch/pipermail/r-help/2011-March/270455.html
  out[is_5digit] <- as.character(as_date(as.numeric(x[is_5digit]), 
                                         origin = "1899-12-30"))
  
  out
}



# monthly to daily precip -------------------------------------------------

# here daily precip is calculated as the amount of precip in that month 
# divided by number of days in that month
# this is so that we can get say get total precip in the 12 months prior to say Aug 15 2018
# in analyses in downstream pipelines, and make the data fit into those pipelines.


monthly2daily_precip <- function(df, year, month, precip, station_name = FALSE) {
  # args:
  #   df--data frame of monthly precip
  #   year--string name of year column
  #   month --string name of month column
  #   precip--string name of precip column
  #   station_name --logical is there a station_name column to return ?
  # returns:
  #   data frame that contains mean daily precip (of the month)
  #     for each date in each month. Has Two columns, date and precip
  #     will be in original units that precip given in
  stopifnot(
    is.data.frame(df),
    # proper col names given
    all(c(year, month, precip) %in% names(df)),
    nrow(df) > 0,
    is.numeric(df[[precip]])
  )
  
  if(any(duplicated(df[, c(year, month)]))) {
    warning("duplicated year/month combinations")
  }
  
  if(!station_name) {
    df1 <- df[ , c(year, month, precip)]
  } else {
    df1 <- df[ , c("station_name", year, month, precip)]
  }
  
  df1$first_date_month <- lubridate::ymd(paste(df1[[year]], df1[[month]], "1", sep = "-"))
  df1$days_in_month <- lubridate::days_in_month(df1$first_date_month)
  new_list <- list()
  for (i in 1:nrow(df1)) {
    n <- df1$days_in_month[i]
    dates <- seq(from = df1$first_date_month[i],
        length.out = n,
        by = "day")
    daily_precip <- df1[i, ][[precip]]/n
    
    if(!station_name) {
      new_list[[i]] <- data.frame(date = dates, precip = daily_precip)
    } else {
      new_list[[i]] <- data.frame(station_name = df1[i, ][["station_name"]],
                                  date = dates, 
                                  precip = daily_precip)
    }
    
  }
  out <- dplyr::bind_rows(new_list)
  out
}



# daily temp from monthly values ------------------------------------------


monthly2daily_temp <- function(df, year, month, temp) {
  # args:
  #   df--data frame of monthly values of temp
  #   year--string name of year column
  #   month --string name of month column
  #   precip--string name of temp column
  # returns:
  #   vector of temp data (should be used after monthl2daily_precip--which creates the df)
  #   assigning monthly value of temp to each day in that month
  
  stopifnot(
    is.data.frame(df),
    # proper col names given
    all(c(year, month, temp) %in% names(df)),
    nrow(df) > 0,
    is.numeric(df[[temp]])
  )
  
  if(any(duplicated(df[, c(year, month)]))) {
    warning("duplicated year/month combinations")
  }
  
  df1 <- df[ , c(year, month, temp)]
  
  df1$first_date_month <- lubridate::ymd(paste(df1[[year]], df1[[month]], "1", sep = "-"))
  df1$days_in_month <- lubridate::days_in_month(df1$first_date_month)
  new_list <- list()
  for (i in 1:nrow(df1)) {
    n <- df1$days_in_month[i]
    dates <- seq(from = df1$first_date_month[i],
                 length.out = n,
                 by = "day")
    # assigning monthly value of temp to each day in that month
    temp_values <- df1[i, ][[temp]]
    
    new_list[[i]] <- data.frame(date = dates, new_vals = temp_values)
  }
  out <- dplyr::bind_rows(new_list)
  out[["new_vals"]]
}


# check station names -----------------------------------------------------

check_station_names <- function(list) {
  # args:
  #   list--list from process_submitted_weather.R script 
  #       (each list element has contents of submitted spreadhseet 'spreadsheet')
  # returns:
  #   list 3 elements--all_good: any problems with station names?
  #        not_in_weather--vector of station_names not in weather sheet but in station sheet
  #        not_in_station--vector of station_names not in station sheet but in weather sheet
  new_list <- map2(list, names(list), function(x, name) {
    
    stopifnot(
      is.list(list),
      !is.null(x$station),
      !is.null(x$weather)
    )
    station_in_weather <- x$station$station_name %in% x$weather$station_name
    weather_in_station <- x$weather$station_name %in% x$station$station_name
    
    out <- list()
    out$all_good <- all(c(station_in_weather, weather_in_station))
    if(!out$all_good) {
      warning(paste("Issue with:", name))
    }
    
    out$not_in_weather <- x$station$station_name[!station_in_weather] %>% 
      unique()
    
    out$not_in_station <- x$weather$station_name[!weather_in_station] %>% 
      unique()
    
    out
    })
  new_list
}


# combine primary and secondary station data -----------------------------


# function for process_submitted_weather.R script

comb_primary_secondary_stns <- function(df1, df2) {
  # args:
  #   df1--df of data from primary (preferred) station
  #   df2--df of secondary station (data to use if primary has NA for that date)
  # returns:
  #   df with precip/temp from df1 unless they are NA then from df2
  #   station_name is the station name from which precip data pulled 
  #    (from df1 unless df1 has NA precip)
  wthr_col_names2 <- c("station_name", "date", "precip", "min_temp", "max_temp",
                       "note_weather", "mean_temp")
  
  stopifnot(
    is.data.frame(df1),
    is.data.frame(df2),
    all(wthr_col_names2 %in% names(df1)),
    all(wthr_col_names2 %in% names(df2)),
    length(unique(df1$station_name)) == 1,
    length(unique(df2$station_name)) == 1
  )
  
  out <- full_join(df1, df2, by = "date",
            suffix = c("_1", "_2")) %>% 
    mutate(precip = ifelse(is.na(precip_1),
                           precip_2,
                           precip_1),
           min_temp = ifelse(is.na(min_temp_1),
                             min_temp_2,
                             min_temp_1),
           max_temp = ifelse(is.na(max_temp_1),
                             max_temp_2, # this max_temp_2 data looks weird
                             max_temp_1),
           # station name will refer to what ever station was used for precip,
           # note will be added if temp from different source
           station_name = ifelse(is.na(precip_1),
                                 station_name_2,
                                 station_name_1),
           note_weather = ifelse(is.na(min_temp_1) & !is.na(min_temp_2) &
                                   station_name == station_name_1,
                                 paste("min_temp from", station_name_2),
                                 note_weather_1),
           note_weather = ifelse(is.na(max_temp_1) & !is.na(max_temp_2) &
                                   station_name == station_name_1,
                                 paste0("max_temp from ", station_name_2,". ", 
                                        note_weather),
                                 note_weather),
           mean_temp = ifelse(is.na(mean_temp_1), # usually all NA anyway
                              mean_temp_2,
                              mean_temp_1)
    )
  out[, wthr_col_names2]
}

