# update-dashboard-data
# by Tim Riley
# retrieves Census API data using tidycensus and updates Google Sheets using
# googlesheets4. uses tidyverse framework for wrangling data

######### CHANGE TO MOST RECENT ACS 5-YEAR RELEASE #########
acsYear <- 2019

######### CONTROLS WHETHER TABLES WRITE TO GSHEETS #########
# set to TRUE if testing data retrieval & processing
# set to FALSE when verified working & it is time to update the Google Sheets
testMode <- TRUE

# run this if the tidyverse API becomes de-authorized on the Google Drive
# (this may happen from time to time)
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
  
  # retrieve table DP04 at multiple geographic levels
  tbl_DP04 <- bind_rows(
    tbl_DP04_county <- get_acs(
      geography = "county",
      table = "DP04",
      year = acsYear,
      state = 17,
      county = 113
    ),
    tbl_DP04_place <- get_acs(
      geography = "place",
      table = "DP04",
      year = acsYear,
      state = 17
    ) %>%
      filter(str_starts(NAME, "Bloomington city|Normal town"))
  ),
  
  # retrieve table DP04 at tract level for mapping
  tbl_DP04_tract <- get_acs(
    geography = "tract",
    table = "DP04",
    year = acsYear,
    state = 17,
    county = 113
  ),
  
  # retrieve table B25001
  tbl_B25001 <- bind_rows(
    tbl_B25001_county <- get_acs(
      geography = "county",
      table = "B25001",
      year = acsYear,
      state = 17,
      county = 113,
    ),
    tbl_B25001_place <- get_acs(
      geography = "place",
      table = "B25001",
      year = acsYear,
      state = 17,
    ) %>%
      filter(str_starts(NAME, "Bloomington city|Normal town"))
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

# run garbage collection to prevent allocation of too much memory
gc()

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
# there are some rather inelegant solutions involving the detection of 
# specific strings in the labels of variables. changes to table S2503
# should be monitored closely to make sure these methods will still work
hh_inc_tenure_new <- tbl_S2503 %>% 
  left_join(acs5SubjectVarList, by = c("variable" = "name")) %>%
  select(-c(moe, concept, GEOID)) %>%
  filter(str_detect(label, "-occupied") & str_detect(label, "!!HOUSEHOLD INCOME")) %>%
  filter(!str_detect(label, "!!Median")) %>%
  transmute(
    Geography = str_replace(NAME, "Illinois", "IL"),
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    Tenure = ifelse(str_detect(label, "wner-occ"), "Owner Occupied", "Renter Occupied"),
    medhhinc = word(label, 5, sep = "!!"),
    units = ifelse(!str_detect(label, "Percent"), estimate, NA),
    Percentage = ifelse(str_detect(label, "Percent"), estimate, NA)
  ) %>%
  group_by(.[1:4]) %>%
  summarize(
    units = sum(units, na.rm = TRUE),
    Percentage = sum(Percentage, na.rm = TRUE)
  ) %>%
  mutate(medhhinc = factor(medhhinc, levels = hh_inc_order)) %>%
  arrange(.[3], .[4]) %>%
  rename(
    "Median HH Income" = medhhinc,
    "Housing Units" = units
  )

# read google sheet
hh_inc_tenure_sheet <- "1_HZA6n7PjXfM3Fz8AsxBMa3blfomcghGQYTvf6oHmzY"

hh_inc_tenure_old <- read_sheet(hh_inc_tenure_sheet)

# join wrangled data to google sheet data
hh_inc_tenure_combined <- hh_inc_tenure_new %>%
  bind_rows(hh_inc_tenure_old) %>%
  distinct() # prevents duplicate rows being added

# write joined data to google sheet
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(hh_inc_tenure_combined),
  # if FALSE (testMode not active)
  write_sheet(hh_inc_tenure_combined, hh_inc_tenure_sheet, 1)
)

# Median HH Income by Age of Householder ----------------------------------
# wrangle the data
hh_inc_age_new <- tbl_S1903 %>% 
  left_join(acs5SubjectVarList, by = c("variable" = "name")) %>%
  select(-c(moe, concept, GEOID)) %>%
  filter(str_detect(label, "Estimate!!Median") & str_detect(label, "!!HOUSEHOLD INCOME BY AGE")) %>%
  transmute(
    Geography = str_replace(NAME, "Illinois", "IL"),
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
  distinct() # prevents duplicate rows being added

# write joined data to google sheet # OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(hh_inc_age_combined),
  # if FALSE (testMode not active)
  write_sheet(hh_inc_age_combined, hh_inc_age_sheet, 1)
)

# Disability by Poverty Status --------------------------------------------
# wrangle the data
dis_by_pov_new <- tbl_S1811 %>% 
  left_join(acs5SubjectVarList, by = c("variable" = "name")) %>%
  select(-c(moe, concept, GEOID)) %>%
  filter(str_detect(label, "poverty level") & str_detect(label, "Disability")) %>%
  transmute(
    Geography = str_replace(NAME, "Illinois", "IL"),
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
  bind_rows(dis_by_pov_old) %>%
  distinct() # prevents duplicate rows being added

# write joined data to google sheet 
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(dis_by_pov_combined),
  # if FALSE (testMode not active)
  write_sheet(dis_by_pov_combined, dis_by_pov_sheet, 1)
)


# Housing by Type ---------------------------------------------------------
# wrangle the data
by_type_new <- tbl_DP04 %>%
  left_join(acs5ProfileVarList, by = c("variable" = "name")) %>%
  select(-c(moe, concept, GEOID)) %>%
  filter(variable == "DP04_0046P" | variable == "DP04_0047P") %>%
  transmute(
    Geography = NAME %>%
      str_remove(" city") %>%
      str_remove(" town") %>%
      str_replace("Illinois", "IL"),
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    Type = word(label, 4, sep = "!!") %>%
      str_replace("-o", " O"), 
    Percent = estimate
  )

# read google sheet
by_type_sheet <- "1YlJNPyZmuxRFaqsbp4BSYBUtzgzC8Xgq6DR4WjiPem0"

by_type_old <- read_sheet(by_type_sheet, sheet = 2)

# join wrangled data to google sheet data
by_type_combined <- by_type_new %>%
  bind_rows(by_type_old) %>%
  arrange(desc(Year)) %>%
  distinct()

# write joined data to google sheet 
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(by_type_combined),
  # if FALSE (testMode not active)
  write_sheet(by_type_combined, by_type_sheet, 2)
)

# Total Housing Units -----------------------------------------------------
# wrangle the data
total_units_new <- tbl_B25001 %>%
  select(-c("GEOID", "moe", "variable")) %>%
  transmute(
    Geography = NAME %>%
      str_remove(" city") %>%
      str_remove(" town") %>%
      str_replace("Illinois", "IL"),
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    "Total Units" = estimate
  )

# read google sheet
total_units_sheet <- "1YlJNPyZmuxRFaqsbp4BSYBUtzgzC8Xgq6DR4WjiPem0"

total_units_old <- read_sheet(total_units_sheet, sheet = 1)

# join wrangled data to google sheet data
total_units_combined <- total_units_new %>%
  bind_rows(total_units_old) %>%
  arrange(desc(Year)) %>%
  distinct()

# write joined data to google sheet 
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(total_units_combined),
  # if FALSE (testMode not active)
  write_sheet(total_units_combined, total_units_sheet, 1)
)

# Median Home Value -------------------------------------------------------
# wrangle the data
median_val_new <- tbl_DP04 %>%
  select(-c(moe, GEOID)) %>%
  filter(variable == "DP04_0089") %>%
  transmute(
    Geography = NAME %>%
      str_remove(" city") %>%
      str_remove(" town") %>%
      str_replace("Illinois", "IL"),
    Year = paste(acsYear - 4, acsYear, sep = "-"), 
    "Home Value" = estimate
  )

# read google sheet
median_val_sheet <- "1JGG4VMg50cAdQa0eIP_YYer7dzZXH1vQLefPpRQPyFg"

median_val_old <- read_sheet(median_val_sheet, sheet = 1)

# join wrangled data to google sheet data
median_val_combined <- median_val_new %>%
  bind_rows(median_val_old) %>%
  arrange(desc(Year)) %>%
  distinct()

# write joined data to google sheet 
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(median_val_combined),
  # if FALSE (testMode not active)
  write_sheet(median_val_combined, median_val_sheet, 1)
)


# Median Home Value (map) -------------------------------------------------
# wrangle the data
median_val_map_new <- tbl_DP04_tract %>%
  filter(variable == "DP04_0089") %>%
  transmute(
    GEOID = GEOID,
    Tract = NAME %>%
      word(1, sep = ", "),
    Year = paste(as.numeric(acsYear) - 4, acsYear, sep = "-"), 
    "Home Value" = estimate
  )

# read google sheet
median_val_map_sheet <- "1JGG4VMg50cAdQa0eIP_YYer7dzZXH1vQLefPpRQPyFg"

median_val_map_old <- read_sheet(median_val_map_sheet, sheet = 3)

# join wrangled data to google sheet data
median_val_map_combined <- median_val_map_new %>%
  bind_rows(median_val_map_old) %>%
  arrange(desc(Year)) %>%
  distinct()

# write joined data to google sheet
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(median_val_map_combined),
  # if FALSE (testMode not active)
  write_sheet(median_val_map_combined, median_val_map_sheet, 3)
)

# Median Gross Rent -------------------------------------------------------
# wrangle the data
median_rent_new <- tbl_DP04 %>%
  select(-c(moe, GEOID)) %>%
  filter(variable == "DP04_0134") %>%
  transmute(
    Geography = NAME %>%
      str_remove(" city") %>%
      str_remove(" town") %>%
      str_replace("Illinois", "IL"),
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    "Gross Rent" = estimate
  )

# read google sheet
median_rent_sheet <- "1JGG4VMg50cAdQa0eIP_YYer7dzZXH1vQLefPpRQPyFg"

median_rent_old <- read_sheet(median_rent_sheet, sheet = 2)

# join wrangled data to google sheet data
median_rent_combined <- median_rent_new %>%
  bind_rows(median_rent_old) %>%
  arrange(desc(Year)) %>%
  distinct()

# write joined data to google sheet 
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(median_rent_combined),
  # if FALSE (testMode not active)
  write_sheet(median_rent_combined, median_rent_sheet, 2)
)


# Median Gross Rent (map) -------------------------------------------------
# wrangle the data
median_rent_map_new <- tbl_DP04_tract %>%
  filter(variable == "DP04_0134") %>%
  transmute(
    GEOID = GEOID,
    Tract = NAME %>%
      word(1, sep = ", "),
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    "Gross Rent" = estimate
  )

# read google sheet
median_rent_map_sheet <- "1JGG4VMg50cAdQa0eIP_YYer7dzZXH1vQLefPpRQPyFg"

median_rent_map_old <- read_sheet(median_rent_map_sheet, sheet = 4)

# join wrangled data to google sheet data
median_rent_map_combined <- median_rent_map_new %>%
  bind_rows(median_rent_map_old) %>%
  arrange(desc(Year)) %>%
  distinct()

# write joined data to google sheet
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(median_rent_map_combined),
  # if FALSE (testMode not active)
  write_sheet(median_rent_map_combined, median_rent_map_sheet, 4)
)


# Owner Occupied Home Values (Distribution) -------------------------------
# establish preferred sorting order of rows
owner_vals_order <- c(
  "Less than $50,000",
  "$50,000 to $99,999",
  "$100,000 to $149,999", 
  "$150,000 to $199,999",
  "$200,000 to $299,999",
  "$300,000 to $499,999",
  "$500,000 to $999,999",
  "$1,000,000 or more"
)

# wrangle the data
owner_vals_new <- tbl_DP04 %>%
  left_join(acs5ProfileVarList, by = c("variable" = "name")) %>%
  select(-c(moe, GEOID, concept, variable)) %>%
  filter(str_detect(label, "VALUE!!Owner-occupied units!!")) %>%
  filter(!str_detect(label, "Median")) %>%
  transmute(
    Geography = NAME %>%
      str_remove(" city") %>%
      str_remove(" town") %>%
      str_replace("Illinois", "IL"),
    Years = paste(acsYear - 4, acsYear, sep = "-"),
    Value = word(label, 4, sep = "!!"),
    Distribution = ifelse(str_detect(label, "Percent"), estimate, NA),
    units = ifelse(!str_detect(label, "Percent"), estimate, NA)
  ) %>%
  group_by(.[1:3]) %>%
  summarize(
    Distribution = sum(Distribution, na.rm = TRUE),
    units = sum(units, na.rm = TRUE)
  ) %>%
  mutate(Value = factor(Value, levels = owner_vals_order)) %>%
  arrange(Value) %>%
  rename(
    "Estimated # units" = units
  )
  

# read google sheet
owner_vals_sheet <- "1T3i3GhWLwdzS4oQ53BMfQ3qB1-meHAH4OpBki0UABiI"

owner_vals_old <- read_sheet(owner_vals_sheet)

# join wrangled data to google sheet data
owner_vals_combined <- owner_vals_new %>%
  bind_rows(owner_vals_old) %>%
  arrange(desc(Years), Geography, Value) %>%
  distinct()

# write joined data to google sheet 
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(owner_vals_combined),
  # if FALSE (testMode not active)
  write_sheet(owner_vals_combined, owner_vals_sheet, 1)
)

# Housing Cost Burden by Income Bracket -----------------------------------
inc_burden_order <- c(
  "Less than $20,000",
  "$20,000 to $34,999",
  "$35,000 to $49,999",
  "$50,000 to $74,999",
  "$75,000 or more"
)

# wrangle the data
inc_burden_new <- tbl_B25106 %>%
  left_join(acs5VarList, by = c("variable" = "name")) %>%
  select(-c(moe, GEOID, concept, variable)) %>%
  filter(str_detect(label, "!!30 percent") & str_detect(label, "Renter")) %>%
  transmute(
    Geography = str_replace(NAME, "Illinois", "IL"),
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    "Income Bracket" = label %>%
      word(4, sep = "!!") %>%
      str_remove(":") %>%
      factor(levels = inc_burden_order),
    Estimate = estimate
  )

# read google sheet
inc_burden_sheet <- "1AtcWtsuUY9h0Gb4Ec-KWPse7W2EPKyI4yDRUMRCmpu0"

inc_burden_old <- read_sheet(inc_burden_sheet, sheet = 2)

# join wrangled data to google sheet data
inc_burden_combined <- inc_burden_new %>%
  bind_rows(inc_burden_old) %>%
  arrange(desc(Year)) %>%
  distinct()

# write joined data to google sheet 
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(inc_burden_combined),
  # if FALSE (testMode not active)
  write_sheet(inc_burden_combined, inc_burden_sheet, 2)
)

# Housing Cost Burden by Tenure (owner and renter) ------------------------
tenure_burden_vars <- tribble(
  ~label, ~variables,
  "Renters", c("DP04_0141P", "DP04_0142P"),
  "Owners with a Mortgage", c("DP04_0114P", "DP04_0115P"),
  "Owners without a Mortgage", c("DP04_0123P", "DP04_0124P")
)

# wrangle the data using an extraordinarily janky series of nested ifelse() 
# statements. There is undoubtedly a more elegant solution within tidyverse
# but I have not yet found it
tenure_burden_new <- tbl_DP04_county %>%
  left_join(acs5ProfileVarList, by = c("variable" = "name")) %>%
  select(-c(moe, GEOID, concept)) %>%
  group_by(
    Geography = str_replace(NAME, "Illinois", "IL"),
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    Population = ifelse(
      variable %in% unlist(tenure_burden_vars[1,]), tenure_burden_vars[[1,1]], ifelse(
        variable %in% unlist(tenure_burden_vars[2,]), tenure_burden_vars[[2,1]], ifelse(
          variable %in% unlist(tenure_burden_vars[3,]), tenure_burden_vars[[3,1]], NA
        )
      )
    )
  ) %>%
  filter(Population != "NA") %>%
  summarize(Percentage = sum(estimate))

# read google sheet
tenure_burden_sheet <- "1AtcWtsuUY9h0Gb4Ec-KWPse7W2EPKyI4yDRUMRCmpu0"

tenure_burden_old <- read_sheet(tenure_burden_sheet, sheet = 1)

# join wrangled data to google sheet data
tenure_burden_combined <- tenure_burden_new %>%
  bind_rows(tenure_burden_old) %>%
  arrange(desc(Year), desc(Percentage)) %>%
  # due to a floating-point bug, R does not think 43.7 == 43.7
  # so the following distinct() statement only respects Year and Population cols
  distinct(Year, Population, .keep_all = TRUE)

# write joined data to google sheet # OR, if testMode is TRUE, View() joined data
write_sheet(tenure_burden_combined, tenure_burden_sheet, 1)

# Housing Units by Decade Structure Built ---------------------------------
# wrangle the data
units_dec_new <- tbl_DP04_county %>%
  left_join(acs5ProfileVarList, by = c("variable" = "name")) %>%
  select(-c(moe, GEOID, concept)) %>%
  filter(!str_detect(label, "Percent") & str_detect(label, "units!!Built")) %>%
  transmute(
    Geography = str_replace(NAME, "Illinois", "IL"),
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    Decade = str_remove(word(label, 4, sep = "!!"), "Built "),
    Units = estimate
  )

# read google sheet
units_dec_sheet <- "1-u0uq4IKgaj_zjfFeQGH4vhLyJrZfJzDzMk-0y-Otnc"

units_dec_old <- read_sheet(units_dec_sheet, sheet = 1)

# join wrangled data to google sheet data
units_dec_combined <- units_dec_new %>%
  bind_rows(units_dec_old) %>%
  arrange(desc(Year)) %>%
  distinct()

# write joined data to google sheet 
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(units_dec_combined),
  # if FALSE (testMode not active)
  write_sheet(units_dec_combined, units_dec_sheet, 1)
)

# Housing Units by Year Householder Moved into Unit -----------------------
# wrangle the data
units_moved_new <- tbl_DP04_county %>%
  left_join(acs5ProfileVarList, by = c("variable" = "name")) %>%
  select(-c(moe, GEOID, concept)) %>%
  filter(!str_detect(label, "Percent") & str_detect(label, "!!Moved in")) %>%
  transmute(
    Geography = str_replace(NAME, "Illinois", "IL"),
    Year = paste(acsYear - 4, acsYear, sep = "-"),
    "Decade Moved In" = str_remove(word(label, 4, sep = "!!"), "Moved in "),
    Units = estimate
  )

# read google sheet
units_moved_sheet <- "1-u0uq4IKgaj_zjfFeQGH4vhLyJrZfJzDzMk-0y-Otnc"

units_moved_old <- read_sheet(units_moved_sheet, sheet = 2)

# join wrangled data to google sheet data
units_moved_combined <- units_moved_new %>%
  bind_rows(units_moved_old) %>%
  arrange(desc(Year)) %>%
  distinct()

# write joined data to google sheet
# OR, if testMode is TRUE, View() joined data
ifelse(
  testMode,
  # if TRUE (testMode active)
  View(hh_inc_tenure_combined),
  # if FALSE (testMode not active)
  write_sheet(units_moved_combined, units_moved_sheet, 2)
)

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
#
# tbl_DP04_multiYear <- map_dfr(
#   acsYearList,
#   ~ get_acs(
#       geography = "county",
#       table = "DP04",
#       year = .x,
#       state = 17,
#       county = 113
#   ),
#   .id = "Year"
# )
