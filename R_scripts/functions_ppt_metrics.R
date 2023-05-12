# Purpose:
# Some potentially useful functions for calculating intra-annual (and maybe other)
# precipitation metrics

# Script Started May 10, 2023

# Author: Martin Holdrege

# helper function used below
check_ppt_input <- function(x) {
  stopifnot(is.numeric(x),
            sum(is.na(x)) < 10)
  if(!length(x) %in% c(365, 366)) {
    stop("Input must have length of 365(6), a years worth of daily ppt")
  }
}

#' Number of days on which half of annual precip falls
#'
#' @param x vector of daily precip
#' @param cutoff value below which ppt is considered 0
#'
#' @return
#' a number between 0 and 365 (or 366)
#' @export
#'
#' @examples
#' # generate fake data
#' n <- 365
#' rained <- runif(n) > 0.8 
#' x <- rep(0, 365)
#' x[rained] <- abs(rnorm(sum(rained), sd = 5))
#' hist(x)
#' days_half_ppt(x)
days_half_ppt <- function(x, cutoff = 1) {
  
  check_ppt_input(x)
  
  # trivial amounts of ppt are considered zero
  x[x < cutoff] <- 0
  
  half <- sum(x, na.rm = TRUE)/2 # half annual precip
  x_ord <- sort(x, decreasing = TRUE) # ordered precip
  
  # cumulative precip
  x_c <- cumsum(x_ord)
  less <- x_c < half #  has half of annual ppt been reached?
  
  # number of days it takes to reach half of annual precip
  days <- sum(less) + 1 # adding 1 because takes partial day to reach half
  days
}

#' Number of wet days in a year
#'
#' @param x vector of daily precip (typically in units of mm)
#' @param cutoff value above which day is considered wet (a cutoff
#' greater than zero is recommended so trivial amounts of precip is 
#' discared)
#'
#' @return
#' a number between 0 and 365 (or 366)
#' 
#' @examples
#' x <- rep(0, 365)
#' x[6:10] <- 10
#' n_wet_days(x)
n_wet_days <- function(x, cutoff = 1) {
  
  check_ppt_input(x)
  
  sum(x > cutoff)
}

# next: create functions to calculate alpha and SI as per Jeff Dukes' research


#' average length of dry spells
#' 
#' @description
#' this is the average length of consecutive days without precipitaiton
#' 
#'
#' @param x daily precipitation (for a year)
#' @param cutoff 
#'
#' @return a number between 0 and 365
#' @export
#'
#' @examples
avg_dryspell_length <- function(x, cutoff = 1) {

  check_ppt_input(x)
  
  is_dry <- x < cutoff
  rle <- rle(is_dry)
  dry_lengths <- rle$lengths[rle$values] # selecting only dry lengths
  mean(dry_lengths) # mean length of dry periods
}


#' calculate size of a given percentile (for days with precip)
#'
#' @param x vector of ppt
#' @param prob percentile want to calculate the size of
#' @param cutoff below which ppt is considered 0
#'
#' @return numeric value (event size of the percentile)
#' 
#' @examples
#' x <- rep(0, 365)
#' x[1:20] <- 1:20
#' ppt_percentile_size(x)
ppt_percentile_size <- function(x, prob = 0.95, cutoff = 1) {
  check_ppt_input(x)
  x2 <- x[x > cutoff]
  as.numeric(quantile(x2, probs = prob))
}



#' Calculate D of daily precipitation
#' 
#' @description
#' Similar to cv, calculates variability between values
#'
#' @param var daily ppt
#' @param cutoff 
#' @param k 
#'
#' @examples
#' #' # generate fake data
#' n <- 365
#' rained <- runif(n) > 0.8 
#' x <- rep(0, 365)
#' x[rained] <- abs(rnorm(sum(rained), sd = 10))
#' hist(x)
#' d_variability(x)
d_variability <- function (var, cutoff = 1, k=0){
  var <- var[var > cutoff]
  var<-var[!is.na(var)]
  if(length(var)<3){return(NA)}
  if(sd(var)==0){return(0)}
  if (min(var)<0){var <- var + abs(min(var)) + 0.01*(max(var)-min(var))}
  if (length(var)<2){return(NA)}else{
    var <- var + k
    aux <- numeric(length(var)-1)
    for (i in 1:(length(var)-1)){
      aux [i] <- abs(log(var[i+1]/var[i]))
    }
  }
  return(mean(aux, na.rm=T))
}


#' calculate MAP and other precipitation endices
#'
#' @param df dataframe with site_code, date and ppt columns
#' @param min_date minimum date (string) to use for filtering
#' @param max_date maximum date (string) to use for filtering
#'
#' @return dataframe with:
#' MAP--mean annual ppt
#' cv_ppt_intra (intra annual ppt cv--based on monthly ppt)
#' cv_ppt_inter interannual cv of precipitation
#' seasonality_index--an measure of intra-annual ppt variation (also
#' based on monthly ppt)
ppt_mean_annual <- function(df, min_date, max_date) {
  
  if("precip" %in% names(df)) {
    df <- rename(df, ppt = precip)
  }
  
  stopifnot(
    c('site_code', "ppt", "date") %in% names(df)
  )
  
  out <- df %>% 
    filter(.data$date >= min_date,
           .data$date <= max_date) %>% 
    mutate(month = lubridate::month(date),
           year = lubridate::year(date)) %>% 
    # order of grouping matters (b/ drop_last below)
    group_by(site_code, year, month) %>% 
    # monthly ppt
    summarise(ppt = sum(ppt), .groups = "drop_last",
              n = n()) %>% 
    # calculatings for the given year (across months), supposedly better
    # then averaged across yrs (next step)
    summarise(seasonality_index = seasonality_index(ppt),
              # intra-annual ppt calculated for the given year
              # (then averaged across years in the next step)
              cv_ppt_intra = sd(ppt)/mean(ppt)*100,
              ppt = sum(ppt),# total ppt for the given year
              n = sum(n),
              .groups = "drop_last")
  
  if(any(out$n > 366 | out$n < 365)) {
    stop("not every year as 365[6] dates")
  }
  
  out <- out %>% 
    summarize(
      MAP = mean(ppt),
      cv_ppt_intra = mean(cv_ppt_intra),
      # inter annual cv of precipitation
      cv_ppt_inter = sd(ppt)/mean(ppt)*100,
      yearly_ppt_d = d_variability(ppt, cutoff = 0),
      seasonality_index = mean(seasonality_index)
    ) %>% 
    mutate(data_period = paste(lubridate::year(min_date),
                               lubridate::year(max_date),
                               sep = "-"))
  out
}

