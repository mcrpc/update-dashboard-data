# update-dashboard-data
# by Tim Riley
# retrieves Census API data using tidycensus and updates Google Sheets

######### CHANGE TO MOST RECENT ACS 5-YEAR RELEASE #########
acsYear <- 2018

# run this if the tidyverse API becomes de-authorized on the google drive
# gs4_auth()

# initialization ----------------------------------------------------------

# define packages
pkgs <- c("tidycensus", "tidyverse", "googlesheets4")

# uncomment and run as needed to install/update packages
# install.packages(pkgs)

# load packages
invisible(lapply(pkgs, require, character.only = TRUE))

# load census API key
censusAPIKey <- read_file("api_key.txt")

# get lists of census API vars and their labels
acs5VarList <- load_variables(acsYear, "acs5", cache = TRUE)
acs5SubjectVarList <- load_variables(acsYear, "acs5/subject", cache = TRUE)
acs5ProfileVarList <- load_variables(acsYear, "acs5/profile", cache = TRUE)



# get ACS data ------------------------------------------------------------
tblList <- c(
  # retrieve table S2503
  tbl_S2503 <- get_acs(
    geography = "county",
    table = "S2503",
    year = acsYear,
    state = 17,
    county = 113,
  ),
  
  # retrieve table S1903
  tbl_S1903 <- get_acs(
    geography = "county",
    table = "S1903",
    year = acsYear,
    state = 17,
    county = 113,
  ),
  
  # retrieve table S1811
  tbl_S1811 <- get_acs(
    geography = "county",
    table = "S1811",
    year = acsYear,
    state = 17,
    county = 113,
  ),
  
  # retrieve table DP04
  tbl_DP04 <- get_acs(
    geography = "county",
    table = "DP04",
    year = acsYear,
    state = 17,
    county = 113,
  ),
  
  # retrieve table B25001
  tbl_B25001 <- get_acs(
    geography = "county",
    table = "B25001",
    year = acsYear,
    state = 17,
    county = 113,
  ),
  
  # retrieve table B25064
  tbl_B25064 <- get_acs(
    geography = "county",
    table = "B25064",
    year = acsYear,
    state = 17,
    county = 113,
  ),
  
  # retrieve table B25095
  tbl_B25095 <- get_acs(
    geography = "county",
    table = "B25095",
    year = acsYear,
    state = 17,
    county = 113,
  ),
  
  # retrieve table B25106
  tbl_B25106 <- get_acs(
    geography = "county",
    table = "B25106",
    year = acsYear,
    state = 17,
    county = 113,
  )
)

# Housing by Median HH Income and Tenure ----------------------------------
# establish preferred sorting order of rows
hh_inc_order <- c(
  "Less than $5,000",
  "$5,000 to $9,999",
  "$10,000 to $14,999", 
  "$15,000 to $19,999",
  "$20,000 to $24,999",
  "$25,000 to $34,999",
  "$35,000 to $49,999",
  "$50,000 to $74,999",
  "$75,000 to $99,999",
  "$100,000 to $149,999",
  "$150,000 or more"
)

# wrangle the data
hh_inc_tenure_new <- tbl_S2503 %>% 
  left_join(acs5SubjectVarList, by = c("variable" = "name")) %>%
  select(-c(moe, concept, GEOID)) %>%
  filter(str_detect(label, "-occupied") & str_detect(label, "!!HOUSEHOLD INCOME")) %>%
  filter(!str_detect(label, "!!Median")) %>%
  mutate(
    Tenure = ifelse(str_detect(label, "wner-occ"), "Owner Occupied", "Renter Occupied"),
    medhhinc = word(label, 5, sep = "!!"),
    units = ifelse(!str_detect(label, "Percent"), estimate, NA),
    Percentage = ifelse(str_detect(label, "Percent"), estimate, NA),
    NAME = "McLean County, IL",
    Year = paste(acsYear - 4, acsYear, sep = "-")
  ) %>%
  select(-c(variable, estimate, label)) %>%
  relocate(NAME, Year) %>%
  group_by(.[1:4]) %>%
  summarize(
    units = sum(units, na.rm = TRUE),
    Percentage = sum(Percentage, na.rm = TRUE)
  ) %>%
  mutate(medhhinc = factor(medhhinc, levels = hh_inc_order)) %>%
  arrange(.[3], .[4]) %>%
  rename(
    Geography = NAME,
    "Median HH Income" = medhhinc,
    "Housing Units" = units
  )

# read google sheet
hh_inc_tenure_sheet <- "1_HZA6n7PjXfM3Fz8AsxBMa3blfomcghGQYTvf6oHmzY"

hh_inc_tenure_old <- read_sheet(hh_inc_tenure_sheet)

# join wrangled data to google sheet data
hh_inc_tenure_combined <- hh_inc_tenure_new %>%
  bind_rows(hh_inc_tenure_old) %>%
  distinct() # removes duplicate rows

# write to google sheet
write_sheet(hh_inc_tenure_combined, hh_inc_tenure_sheet, 1)

# Median HH Income by Age of Householder ----------------------------------
# wrangle the data
hh_inc_age_new <- tbl_S1903 %>% 
  left_join(acs5SubjectVarList, by = c("variable" = "name")) %>%
  select(-c(moe, concept, GEOID)) %>%
  filter(str_detect(label, "Estimate!!Median") & str_detect(label, "!!HOUSEHOLD INCOME BY AGE")) %>%
  transmute(
    Geography = "McLean County, IL",
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    Age = word(label, 4, sep = "!!") %>% str_replace(" to ", "-") %>% str_remove(" years"),
    "Median HH Income" = estimate
  )

# read google sheet
hh_inc_age_sheet <- "1al2igX2hbQyBEIOtL_nxZO5JY-Ad-y91Zuw3Xzi_PEc"

hh_inc_age_old <- read_sheet(hh_inc_age_sheet)

# join wrangled data to google sheet data
hh_inc_age_combined <- hh_inc_age_new %>%
  bind_rows(hh_inc_age_old) %>%
  distinct()

# write to google sheet
write_sheet(hh_inc_age_combined, hh_inc_age_sheet, 1)

# Disability by Poverty Status --------------------------------------------
# wrangle the data
dis_by_pov_new <- tbl_S1811 %>% 
  left_join(acs5SubjectVarList, by = c("variable" = "name")) %>%
  select(-c(moe, concept, GEOID)) %>%
  filter(str_detect(label, "poverty level") & str_detect(label, "Disability")) %>%
  transmute(
    Geography = "McLean County, IL",
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    "Poverty Status" = word(label, 5, sep = "!!"),
    "Disability Status" = word(label, 2, sep = "!!") %>% str_remove("With a "),
    Percent = estimate
  ) %>%
  arrange(desc(.[4]))

# read google sheet
dis_by_pov_sheet <- "1JkVRIpn0woNhGfvb6UVj-LDPQ3hDRW5lsIifjyMNpuo"

dis_by_pov_old <- read_sheet(dis_by_pov_sheet)

# join wrangled data to google sheet data
dis_by_pov_combined <- dis_by_pov_new %>%
  bind_rows(dis_by_pov_old)

# write to google sheet
write_sheet(dis_by_pov_combined, dis_by_pov_sheet, 1)

# Housing by Type ---------------------------------------------------------
# wrangle the data
total_units_new
by_type_new

# read google sheet
housing_by_type_sheet

total_units_old
by_type_old

# join wrangled data to google sheet data
total_units_combined
by_type_combined

# write to google sheet
write_sheet(total_units_combined, housing_by_type_sheet, 1)
write_sheet(by_type_combined, housing_by_type_sheet, 2)

# Total Housing Units -----------------------------------------------------
# wrangle the data

# read google sheet

# join wrangled data to google sheet data

# write to google sheet


# Median Home Value -------------------------------------------------------
# wrangle the data

# read google sheet

# join wrangled data to google sheet data

# write to google sheet


# Median Gross Rent -------------------------------------------------------
# wrangle the data

# read google sheet

# join wrangled data to google sheet data

# write to google sheet


# Owner Occupied Home Values (Distribution) -------------------------------
# wrangle the data

# read google sheet

# join wrangled data to google sheet data

# write to google sheet


# Housing Cost Burden by Income Bracket -----------------------------------
# wrangle the data

# read google sheet

# join wrangled data to google sheet data

# write to google sheet


# Housing Cost Burden by Tenure (owner and renter) ------------------------
# wrangle the data

# read google sheet

# join wrangled data to google sheet data

# write to google sheet


# Housing Units by Decade Structure Built ---------------------------------
# wrangle the data

# read google sheet

# join wrangled data to google sheet data

# write to google sheet


# Housing Units by Year Householder Moved into Unit -----------------------
# wrangle the data

# read google sheet

# join wrangled data to google sheet data

# write to google sheet


# Federal Downpayment Home Loan Bank Funding ------------------------------
# wrangle the data

# read google sheet

# join wrangled data to google sheet data

# write to google sheet


# one-time update of Disability Status by Poverty Status ------------------
# 
# acsYearList <- as.list(c(acsYear - 4:0)) %>%
#   set_names(c(acsYear - 4:0))
# 
# tbl_S1811_multiYear <- map_dfr(
#   acsYearList,
#   ~ get_acs(
#     geography = "county",
#     table = "S1811",
#     year = .x,
#     state = 17,
#     county = 113
#   ),
#   .id = "year"
# )
# 
# dis_by_pov_overwrite <- tbl_S1811_multiYear %>% 
#   left_join(acs5SubjectVarList, by = c("variable" = "name")) %>%
#   select(-c(moe, concept, GEOID)) %>%
#   filter(str_detect(label, "poverty level") & str_detect(label, "Disability")) %>%
#   transmute(
#     Geography = "McLean County, IL",
#     Year = paste(as.numeric(year) - 4, year, sep = "-"),
#     "Poverty Status" = word(label, 5, sep = "!!"),
#     "Disability Status" = word(label, 2, sep = "!!") %>% str_remove("With a "),
#     Percent = estimate
#   ) %>%
#   arrange(desc(.[2]), desc(.[4]))
# 
# write_sheet(dis_by_pov_overwrite, dis_by_pov_sheet, 1)
