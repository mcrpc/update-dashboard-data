# update-dashboard-data
# by Tim Riley
# retrieves Census API data using tidycensus and updates Google Sheets

######### CHANGE TO MOST RECENT ACS 5-YEAR RELEASE #########
acsYear <- 2018

# initialization ----------------------------------------------------------

# define packages
pkgs <- c("tidycensus", "tidyverse", "googlesheets4", "googledrive")

# uncomment and run if necessary
# install.packages(pkgs)

# load packages
invisible(lapply(pkgs, require, character.only = TRUE))

# load census API key
censusAPIKey <- read_file("api_key.txt")

# get lists of census API vars
# acs5VarList <- load_variables(acsYear, "acs5", cache = TRUE)
acs5SubjectVarList <- load_variables(acsYear, "acs5/subject", cache = TRUE)

# list of google sheets go here
listTables <- c()

tbl_S2503 <- get_acs(
  geography = "county",
  table = "S2503",
  year = acsYear,
  state = "17",
  county = 113,
)

hh_inc_tenure <- tbl_S2503 %>% 
  left_join(acs5SubjectVarList, by = c("variable" = "name")) %>%
  select(-c(moe, concept, GEOID)) %>%
  filter(str_detect(label, "-occupied") & str_detect(label, "!!HOUSEHOLD INCOME")) %>%
  filter(!str_detect(label, "!!Median")) %>%
  mutate(
    "Tenure" = ifelse(str_detect(label, "wner-occ"), "Owner Occupied", "Renter Occupied"),
    "Median HH Income" = word(label, 5, sep = "!!"),
    "Housing Units" = ifelse(!str_detect(label, "Percent"), estimate, NA),
    "Percentage" = ifelse(str_detect(label, "Percent"), estimate, NA),
    NAME = "McLean County, IL",
    "Year" = paste(acsYear - 4, acsYear, sep = "-")
  ) %>%
  rename(
    "Geography" = NAME
  ) %>%
  select(-c(variable, estimate, label)) %>%
  relocate("Geography", "Year")
  

# tbl_S1903 <- map_dfr(
#   .x = listYears,
#   ~ get_acs(
#     geography = "county",
#     table = "S1903",
#     year = .x,
#     state = "17",
#     county = 113,
#   ),
#   .id = "year"
# )
# 
# tbl_S1811 <- map_dfr(
#   .x = listYears,
#   ~ get_acs(
#     geography = "county",
#     table = "S1811",
#     year = .x,
#     state = "17",
#     county = 113,
#   ),
#   .id = "year"
# )
# 
# tbl_DP04 <- map_dfr(
#   .x = listYears,
#   ~ get_acs(
#     geography = "county",
#     table = "DP04",
#     year = .x,
#     state = "17",
#     county = 113,
#   ),
#   .id = "year"
# )
# 
# tbl_B25001 <- map_dfr(
#   .x = listYears,
#   ~ get_acs(
#     geography = "county",
#     table = "B25001",
#     year = .x,
#     state = "17",
#     county = 113,
#   ),
#   .id = "year"
# )
# 
# tbl_B25064 <- map_dfr(
#   .x = listYears,
#   ~ get_acs(
#     geography = "county",
#     table = "B25064",
#     year = .x,
#     state = "17",
#     county = 113,
#   ),
#   .id = "year"
# )
# 
# tbl_B25095 <- map_dfr(
#   .x = listYears,
#   ~ get_acs(
#     geography = "county",
#     table = "B25095",
#     year = .x,
#     state = "17",
#     county = 113,
#   ),
#   .id = "year"
# )
# 
# tbl_B25106 <- map_dfr(
#   .x = listYears,
#   ~ get_acs(
#     geography = "county",
#     table = "B25106",
#     year = .x,
#     state = "17",
#     county = 113,
#   ),
#   .id = "year"
# )