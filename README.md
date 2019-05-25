# pcent_cv_ppt

Repository started during the May 2019 Drought Net/IDE meeting.

The repository was started for scripts that calculate the interannual cv of precipitation for each site, based on GHCN weather station data (when available) for each IDE site. It has expanded to contain various scripts that use GHCN data. 

The initial conclusion from the May 2019 meeting was that interannual CV from the station data matched pretty well with CV from the interpolated global products (e.g. from the TPA tool) so this data may not need to be pursued too much further in the future. 

# Brief description of Scripts:

## `functions.r`

This script is sourced by a couple of the other scripts. It should contain functions that were made for use in these other scripts. 

## `GHCN process.R` 

Used to get historical GHCN precip data (from best/closest) stations to each IDE site. It cleans the historical data to only contain good years (i.e. missing up 31 days precip) and must contain a certain number (30 years) of data. The CV of annual precip is then calculated from this data.

In preliminary analysis we were only considering stations good if they are within 100 km and 500 m elevation of the IDE site. 

## `DN site - GHCN station options.R`

Output from this script used in the `GHCN process.R` script. It was used to select better stations for some sites, when the first bit of code in `GHCN process` selected/used data from bad stations. It is used in the 2nd half of `GHCN process`.

## `biomass_get_dates.R`

Short script that extracts the years that biomass was harvested. This script was than sourced in `GHCN_process_expt_years.R` script. May need to be updated/checked as new data from sites comes in. 

## `GHCN_process_expt_years.R`

