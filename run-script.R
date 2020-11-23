# update-dashboard-data
# by Tim Riley
# retrieves Census API data using tidycensus and updates Google Sheets

# define packages
pkgs <- c("tidycensus", "tidyverse", "googlesheets4", "googledrive")

# uncomment and run if necessary
# install.packages(pkgs)

# load packages
invisible(lapply(pkgs, require, character.only = TRUE))

# load census API key
censusAPIKey <- read_file("api_key.txt")
