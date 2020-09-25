
# script start 5/20/19

# functions used in GHCN process script (among others as needed)


# packages ----------------------------------------------------------------


library(tidyverse)


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

ghcn_download_parse <- function(df, return_list = FALSE,...) {
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
    
    tmp <- ghcnd(staID,...)
    
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

# stop! fix this to take site_code site and site
comb_primary_secondary_stns <- function(df1, df2, other_cols = FALSE) {
  # args:
  #   df1--df of data from primary (preferred) station
  #   df2--df of secondary station (data to use if primary has NA for that date)
  #   other_cols--optional vector of additional column names to keep
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
  
  # doing this when join leads two NA in station_name_x below
  first_station <- df1$station_name[1]
  second_station <- df2$station_name[1]
  
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
           station_name = ifelse(is.na(precip_1) & !is.na(precip_2),
                                 second_station,
                                 first_station),
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
  # add other cols back in if requested
  if (other_cols) {
    stopifnot(
      length(unique(df1$site_name)) == 1,
      length(unique(df1$site_code)) == 1
    )
    out$site_name <- df1$site_name[1]
    out$site_code <- df1$site_code[1]
    out$site <- df1$site[1]
    return(out[, c(wthr_col_names2, "site_name", "site_code", "site")])
  }
  out[, wthr_col_names2]
}



# yearly control and drt precip -------------------------------------------

calc_yearly_precip <- function(site_data, precip_data){
  # site_data--df with biomass dates, treatment dates etc.
  # precip_data--df with daily precip by site code
  # returns site_data with added control and drought precip cols among others
  
  needed_cols1 <- c("bioDat", "site_code", "trtDat", "X365day.trt", 
                    "IfNot365.WhenShelterRemove", "IfNot365.WhenShelterSet",
                    "drought_trt")
  needed_cols2 <- c("site_code", "date", "ppt")
  
  stopifnot(
    is.data.frame(site_data),
    is.data.frame(precip_data),
    all(needed_cols1 %in% names(site_data)),
    all(needed_cols2 %in% names (precip_data))
  )
  
  for (i in 1:nrow(site_data)) {
    print(i)
    row <- site_data[i, ]
    print(row$site_code)
    site_ppt <- precip_data[precip_data$site_code == row$site_code,]
    start_date <- as.Date(row$bioDat-365)
    site_dates <- tibble(
      date = seq(start_date, row$bioDat, by = 1)
    )
    # joining in since someting data doesn't go whole year which causes problems
    site_ppt2 <- site_dates %>% 
      left_join(site_ppt, by = "date") %>%
      filter(date >= start_date & date < row$bioDat) %>% 
      mutate(n_treat_days = difftime(date, row$trtDat, units="days"))
    
    # when did the shelter come off:
    shelter_off_start <- if ((row$X365day.trt == "No" | is.na(row$X365day.trt)) && 
                             # used to protect against when no date given
                             (is.na(row$IfNot365.WhenShelterRemove) | 
                              row$IfNot365.WhenShelterRemove != "")
                             ) {
      min_year <- min(year(site_ppt2$date))
      dmy(paste(row$IfNot365.WhenShelterRemove, min_year))
    } else {
      NA
    }
    # when did the shelter go back on
    shelter_off_end <- if ((row$X365day.trt == "No" | is.na(row$X365day.trt)) && 
                           # used to protect against when no date given
                           (is.na(row$IfNot365.WhenShelterSet) | row$IfNot365.WhenShelterSet != "")) {
      max_year <- max(year(site_ppt2$date))
      dmy(paste(row$IfNot365.WhenShelterSet, max_year))
    } else {
      NA
    }
    is_trt365 <- rep(row$X365day.trt == "Yes" | row$IfNot365.WhenShelterSet == "" | is.na(row$X365day.trt), nrow(site_ppt2))
    # some sites say drought X365day.trt == "No" but don't give shelter on/off dates
    # in those cases I just treated them as having year round shelter on.
    site_ppt2 <- site_ppt2 %>% 
      mutate(
        # is drought occuring
        is_drought = ifelse(n_treat_days < 0,
                            0,
                            ifelse(is_trt365,
                                   1,
                                   ifelse(date < shelter_off_start | date > shelter_off_end | is.na(shelter_off_start),
                                          1,
                                          0)
                            )),
        drought_ppt = ifelse(is_drought,
                             (ppt*(1 - row$drought_trt)),
                             ppt)
      )
    if (!is.null(site_ppt2$wc)) {
      site_data[i, ]$ppt_num_wc_interp <- sum(site_ppt2$wc == "Y")
    }
    site_data[i,]$ppt_num_NA <-  sum(is.na(site_ppt2$ppt))
    site_data[i,]$num_drought_days <- sum(site_ppt2$is_drought)
    
    site_data[i,]$ppt_ambient <-  if (all(is.na(site_ppt2$ppt))) {
      NA } else { 
        sum(site_ppt2$ppt, na.rm = TRUE) 
        }
    site_data[i,]$ppt_drought <-  if (all(is.na(site_ppt2$drought_ppt))) {
      NA } else { 
        sum(site_ppt2$drought_ppt, na.rm = TRUE) 
      }
    
  }
  site_data
}

# path to most recent file ------------------------------------------------

newest_file_path <- function(path, file_regex, mdy = FALSE) {
  # args:
  #  path--path to the older you want to look in 
  #  file_regex--regular expression to match a file name (e.g. may want to match
  #     any files of the form example_yyyy-mm-dd then this function will return
  #     the most recent file (when sorted in descending order))
  #  mdy --logical, set to true if file name contains a date in m-d-y format (which
  #     won't return newest file if just sort file names as is the default)
  # returns:
  #  path of the newest file (based on the file name), throws a warning if 
  #   this also isn't the most recently modified file. 
  stopifnot(
    is.character(path),
    is.character(file_regex),
    is.logical(mdy)
  )
  paths <- list.files(
    path = path,
    pattern = file_regex, 
    full.names = TRUE)
  
  paths_short <- list.files(
    path = path,
    pattern = file_regex)
  
  if (length(paths) == 0) {
    stop("no files match that regex")
  }
  
  # time modified
  time_modified <- file.info(paths)$mtime
  
  if (!mdy) {
    out <- sort(paths, decreasing = TRUE)[1]
  } else {
    file_dates <- stringr::str_extract(paths, "\\d{1,2}-\\d{1,2}-\\d{4}")
    file_dates <- lubridate::mdy(file_dates)
    out <- paths[which(file_dates == max(file_dates))]
  }
  
  # check in case some other file was modified more recently. 
  if(out != paths[which(time_modified == max(time_modified))]) {
    warning("a file other than the one selected based on file name was modified more recently")
  }
  
  message(
    paste("files matching this regex are:", 
          paste(paths_short, collapse = "\n"), 
          collapse = "\n")
  )
  
  out
}


# one unique --------------------------------------------------------------

# used in other function below

one_unique <- function(x) {
  # args:
  #   x--a vector
  # returns:
  #   logical--does the vector just have one unique element?
  stopifnot(is.atomic(x))
  
  if(any(is.na(x))){
    warning("vector contains NAs")
  }
  
  lu <- length(unique(x))
  
  out <- if (lu == 1) {
    TRUE
  } else {
    FALSE
  }
  out
}


# convert mdy date to day-month -------------------------------------------
# needs lubridate loaded

mdy2dm <- function(x) {
  # args
  #   x--vector
  # returns
  #   if any elements of vector have date form of "2/15/2017" then 
  #       day month returned (eg 15-feb)
  is_mdy <- stringr::str_detect(x, "\\d{1,2}/\\d{1,2}/\\d{4}")
  is_mdy <- ifelse(is.na(is_mdy), FALSE, is_mdy)
  if(!any(is_mdy)) {
    return(x)
  } else {
    dates <- lubridate::mdy(x[is_mdy])
    dm <- paste(lubridate::day(dates), lubridate::month(dates, label = TRUE), 
                sep = "-")
    out <- x
    out[is_mdy] <- dm
  }
  out
}

# testing
if (FALSE) {
  x <- c("2017-10-5", "5-feb", "4/25/2017", "", NA)
  mdy2dm(x)
}


# calculate n treat days --------------------------------------------------


calc_n_treat_days_adj <- function(df, return_df = FALSE) {
  # args:
  #   df--dataframe (this needs to be a subsetted df such that it is
  #     only for one one plot site code)--used in calculate_cdf.R script
  #     df needs to have biomass dates, shelter on/off dates and trt dates
  #   return_df--return n
  # returns:
  #   vector of n_treat_days that is the number of days treatment occuring
  #     with days that shelter was off excluded
  # Notes:
  # this function works only when set and remove dates of the shelter occur
  # during the same calendar year (which they do with current date--in theory
  # this could be a problem with a site from the souther hemisphere)
  
  needed_cols <- c("plot", "biomass_date", "first_treatment_date", 
                   "IfNot365.WhenShelterRemove", "IfNot365.WhenShelterSet",
                   "n_treat_days", "X365day.trt")
  stopifnot(
    needed_cols %in% names(df),
    one_unique(df$first_treatment_date),
    one_unique(df$plot),
    one_unique(df$first_treatment_date),
    is.Date(df$biomass_date),
    is.Date(df$first_treatment_date)
  )
  
  if (any(df$X365day.trt != "No" | is.na(df$X365day.trt))) {
    if(return_df) {
      return(df)
    } else {
      return(rep(NA_real_, nrow(df)))
    }
  }
  if (any(is.na(df$IfNot365.WhenShelterSet) | df$IfNot365.WhenShelterSet == ""
          | is.na(df$IfNot365.WhenShelterRemove) | df$IfNot365.WhenShelterRemove == "")) {
    warning("data indicates drought may not be 365 but set/remove dates missing")
    if(return_df) {
      return(df)
    } else {
      return(rep(NA_real_, nrow(df)))
    }
  }
  
  out <- df
  first_trt_date <- df$first_treatment_date[1]
  first_trt_yr <- lubridate::year(first_trt_date)
  df$year <- lubridate::year(df$biomass_date)
  bio_yrs <- df$year 
  trt_yrs <- first_trt_yr:max(bio_yrs)
  # convert mdy dates to day-month (necessary in a couple occasions)
  df$set <- mdy2dm(df$IfNot365.WhenShelterSet)
  df$remove <- mdy2dm(df$IfNot365.WhenShelterRemove)
  
  one_date <- one_unique(df$set) & one_unique(df$remove)
  
  if(!all(trt_yrs %in% bio_yrs) & !one_date) {
    stop("missing biomass dates and multiple shelter on/off dates")
  }
  df_sub <- df %>% 
    filter(year(biomass_date) %in% trt_yrs)
  
  if (one_date) {
    # dates set during treatment years
    trt_set <- lubridate::dmy(paste(df$set[1], trt_yrs, sep = "-"))
    # dates remove during trt years
    trt_remove <- lubridate::dmy(paste(df$remove[1], trt_yrs, sep = "-"))
    
  } else {
    # when multiple set/remove dates are present (based on earlier check
    # this requires that all consecutive years of trmt present)
    trt_set <- lubridate::dmy(paste(df_sub$set, df_sub$year, sep = "-"))
    trt_remove <- lubridate::dmy(paste(df_sub$remove, df_sub$year, sep = "-"))
  }
  # dates shelter was on
  shelter_on <- map2(trt_set, trt_remove, function(set, remove) {
    seq(from = set, to = remove, by = 1)
  })
  shelter_on <- unlist(shelter_on) # dates shelter was on
  
  # biomass harvest dates while trmts ongoing
  trt_bio_dates <- df$biomass_date[df$n_treat_days > 0]
  
  # sequence of dates from first trt date to biomass date
  trt_dates <- map(trt_bio_dates, function(bio_date) {
    # not counting the first day, otherwise june 1 to june 1 would lead
    # to one day of trmt but should be 0
    seq(from = first_trt_date, to = bio_date, by = 1)[-1]
  })
  
  n_treat_days_adj <- map_dbl(trt_dates, function(dates) {
    sum(dates %in% shelter_on)
  })
  df$n_treat_days_adj <- NA
  
  df$n_treat_days_adj[df$n_treat_days > 0] <- n_treat_days_adj
  
  # return whole df
  if( return_df) {
    out$n_treat_days_adj <- df$n_treat_days_adj
    out
  }
  df$n_treat_days_adj
}


# convert_mdy -------------------------------------------------------------

# if date string has some values of format mm/dd/yyyy convert those to date
# leave others the same

convert_mdy <- function(x) {
  # args:
  #   x--character vector of of dates
  # returns:
  #   character vectors with dates of format mm/dd/yyyy converted
  #   to "yyyy-mm-dd"
  stopifnot(
    is.character(x)
  )
  is_mdy <- stringr::str_detect(x, "^\\d{1,2}/\\d{1,2}/\\d{4}$")
  
  if(!any(is_mdy)) {
    message("no dates of format like 2/15/2019")
    return(x)
  }
  out <- x
  out[is_mdy] <- as.character(lubridate::mdy(x[is_mdy]))
  out
}

# test

if (FALSE) {
  x <- c("2018-10-1", "2/13/1991", "12/21/2000")
  convert_mdy(x)
  convert_mdy("may-12-2011")
}


# discard duplicated station dates ----------------------------------------

# specific function only used in process_submitted_weather.R
discard_dup_station_data <- function(df, primary, secondary, other_cols = FALSE) {
  # args:
  #   df--data frame with station name and dates at least
  #   primary--name of station with data of primary importance
  #   secondary--name of station with data to keep only if there are dates
  #     for which primary doesn't have data
  # returns:
  #   subsetted df
  df1 <-  df[df$station_name == primary, ]
  df2 <-  df[df$station_name == secondary, ]
  df_comb <- comb_primary_secondary_stns(df1, df2, other_cols = other_cols)
  df_other <-  df[!df$station_name %in% c(primary, secondary), ]
  out <- bind_rows(df_comb, df_other)
  out
}
