# LTER working group (droughtnet) precipitation data wrangling and analysis

Repository started during the May 2019 Drought Net/IDE meeting.

The repository was started for scripts that calculate the interannual cv of precipitation for each site, based on GHCN weather station data (when available) for each IDE site. It has expanded to contain various scripts that use GHCN data, compile user submitted weather data, as well as pulling in gridded precipitation data products (including CHIRPS).

The initial conclusion from the May 2019 meeting was that interannual CV from the station data matched pretty well with CV from the interpolated global products (e.g. from the TPA tool) so this data may not need to be pursued too much further in the future. 

As of the April 2022 meeting the main purpose of the repository is to compile user submitted data, GHCN station data and CHIRPS data to calculate the amount of ppt reaching drought and control plots for the 365 days before biomass sampling each year.

# Brief description of Scripts:

All scripts are found in the R_scripts folder.

Note about names--number prefixes in the script names provides the general order in which scripts need to be run. 

Note--not all file paths have been updated in all scripts but the workflow going forward
is to set the path to dropbox in the dropbox_path.R script, and have this script sourced by other scripts. That way (in theory at least) assuming the user has access to the droughtnet dropbox folders, only one path will need to be changed for these scripts to run. 

## `01_pre-process_site-name_weather.R`

These are scripts that pre-process site submitted data. The outputs of these scripts are then read
in by the `02_process_submitted_weather.R` script. Note these pre-processing scripts only exist for some sites, where extra pre-processing is needed. Note--these scripts don't need to be re-run, so you can move onto running the `02_process_submitted_weather.R`

## `01_CHIRPS_climate-data-_download.R`

This script connects to google earth engine to extract daily precipitation data for each site (this only works for sites between 50S and 50N latitude). This script requires more setup to run than others (e.g. having a earth engine account). This is getting data from the CHIRPS gridded data product. 

## `01_worldclim_extract-monthly`

Extract monthly precip from world clim (this requires the monthly world clim rasters to have been downloaded, they aren't stored on the dropbox because they are in the neighborhood of 100GB). 

## `01_GHCN_process_expt_years.R`

Uses the rnoaa package to automatically grab weather data from the nearest global historical climatology network weather station (if available).

Used to pull daily GHCN data for the years of interest. Gets daily data for closest station (within 100 km w/ less than 500 m elevation difference) that has good data for the pre-treatment calendar year (and year before that) and first treatment yr and 2nd treatment year. If biomass was measured for additional years then the precip data was pulled for those years but it wasn't required to be "good".
This script outputs a csv of daily precip data for all the sites where it existed. 

## `02_process_submitted_weather.R`

The main script that pulls together the weather data that the sites submitted

## `03_check_submitted_weather.R`

Takes the output of `02_process_submitted_weather.R` and checks for some potential data errors.

## `04_worldclim_monthly_precip_substitution.R`

Takes the output of `03_check_submitted_weather.R` and replaces missing values with long term means (i.e. average expected amount of precip) from worldclim.

## `05_precipitation reduction calculations.R`

Uses submitted weather data (output of `04_worldclim_monthly_precip_substitution.R`), ghcn data (output of chirps precip data `01_GHCN_process_expt_years.R`) and CHIRPs data (output of `01_CHIRPS_climate-data-_download.R`), to calculate the amount of ppt received in both drought and control plots, for the 365 days prior to biomass harvest. 
The output is in long form (1 row for each site, plot, and year).  

For the drought treatments the percentage reduction that shelters impose was used to calculate precip received by drought plots, so output contains annual precip for both control and drought. 

User submitted weather data is used if available, if not then GHCN data is used, if not then CHIRPS data is used.

## `06_calculate_cdf.R`

Takes the output of `05_precipitation reduction calculations.R`, and calculates the percentile of historic annual ppt (from worldclim). The percentiles are of the actual precipitation that occured at the drought/control plots for the given treatment years.  Most importatnly this script combines the data into a cleaner data file with 1 row per year and site. 
This script creates the main important output used by other analyses (precip_by_trmt_year_with_percentiles_yyyy-mm-dd.csv).

## `07_station_distances.R`

Created to calculate the average distances between weather stations from which data was used (both user submitted and GHCN) and IDE sites.
This script doesn't create output that is used in other scripts. 

## `functions.R`
This script is sourced by other scripts in this repository. It should contain functions that were made for use in these other scripts. 

## `biomass_get_dates.R`

Short script that extracts the years that biomass was harvested. This script was than sourced in `01_GHCN_process_expt_years.R` script. May need to be updated/checked as new data from sites comes in. 

## `worldclim_seasonality.R`

Script--for a one of analysis (not sure this output is used anywhere anymore)

Purpose of this code is to pull mean monthly precipitation mean monthly temp
(this is means of daily average temp) from each site
and then calculate various precipitation seasonality metrics

## `combine_weather_climate_C-study.R`

Pull together climate and weather data for Baoku Shi for soil C
sequestration study. This is only for a subset of IDE sites.

## `soil_extract_from_isric.R`
Purpose--extract soil info (percent sand) for the IDE sites
from ISRIC world soil information (output not used by other scripts in this repo)

# Old scripts that really aren't used anymore:



## `GHCN process.R` 

Used to get historical GHCN precip data (from best/closest) stations to each IDE site. It cleans the historical data to only contain good years (i.e. missing up 31 days precip) and must contain a certain number (30 years) of data. The CV of annual precip is then calculated from this data.

In preliminary analysis we were only considering stations good if they are within 100 km and 500 m elevation of the IDE site. 

## `DN site - GHCN station options.R`

Output from this script used in the `GHCN process.R` script. It was used to select better stations for some sites, when the first bit of code in `GHCN process` selected/used data from bad stations. It is used in the 2nd half of `GHCN process`.

## `biomass_sensitivity_vs_ppt_reductions.R`

Pulls in biomass effect size (likely to change as full biomass.csv changes) as calculated in a seperate script on dropbox. Makes figures of effect size vs various metrics of precipitation reductions. 


