#!/bin/bash

# Purpose: download MSWEP climate data (global daily precip data)
# (http://www.gloh2o.org/mswep/#faq)
# they make this data available by sharing a google drive folder
# the the data is large (~15 Gb/decade of data) and needs to be downloaded using rclone
# I'm dowloading this data to an external hard drive 

# Author: Martin Holdrege

# Script started: 12/20/2022

# testing on a couple files
rclone ls --drive-shared-with-me gdrive2:MSWEP_V280/Past/Daily --include "{{200[8-9]00\d.nc}}"

# where putting the files
dest=D:/IDE_climate_rasters/MSWEP_V280/Daily

# google drive folder (source)
source=gdrive2:MSWEP_V280/Past/Daily

rclone copy --drive-shared-with-me "$source" "$dest"  # rclone doesn't re-copy files if they're exactly the same as in destination, so can run this 

# download the most recent files (they are in a different folder)
sourceNRT=gdrive2:MSWEP_V280/NRT/Daily
rclone copy --drive-shared-with-me "$sourceNRT" "$dest" 


