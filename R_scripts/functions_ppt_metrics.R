# Purpose:
# Some potentially useful functions for calculating intra-annual (and maybe other)
# precipitation metrics

# Script Started May 10, 2023

# Author: Martin Holdrege


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
days_half_ppt <- function(x, cutoff = 0) {
  
  stopifnot(is.numeric(x),
            sum(is.na(x)) < 10)
  if(!length(x) %in% c(365, 366)) {
    stop("Input must have length of 365(6), a years worth of daily ppt")
  }
  
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
