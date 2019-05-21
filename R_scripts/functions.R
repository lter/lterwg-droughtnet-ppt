
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

