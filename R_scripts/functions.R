
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
