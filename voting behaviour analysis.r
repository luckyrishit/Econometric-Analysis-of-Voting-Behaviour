rm(list = ls())

library(dplyr)
library(tidyverse)
library(fredr)
library(tidyr)
library(ggplot2)
library(readxl)
library(car)
library(ggcorrplot)
library(caret)
library(lmtest) 
library(sandwich)

election_data <- read.csv("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/countypres_2000-2020.csv")
fred_key <- readLines("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/w5/05_data_acquisition/fred_key.txt")
fredr_set_key(fred_key)

#Election Day 2012: November 6, 2012
#Election Day 2016: November 8, 2016

#Merge all datasets into one

# Keep only the data for the years 2012 and 2016
election_data <- election_data[election_data$year %in% c(2012, 2016), ]
View(election_data)

#unique(election_data$candidate)


#percentage changes between each county

# county level vote share results
election_data <- election_data %>%
    mutate(vote_shares = candidatevotes / totalvotes) %>%
    filter(candidatevotes != 0 & totalvotes != 0)  #remove distric 99 from 2012 -> has 0 values
view(election_data)

#in county fips last three is county_fips, and the first ones are state_fips
# Extract state_fips and county_fips from the county_fips column
election_data <- election_data %>%
  mutate(state_fips = substr(county_fips, 1, nchar(county_fips) - 3),
         county_fips = substr(county_fips, nchar(county_fips) - 2, nchar(county_fips)))
        # Remove leading zeros from county_fips
        election_data <- election_data %>%
          mutate(county_fips = sub("^0+", "", county_fips))

#county_fips and state_fips as numeric
election_data <- election_data %>%
   mutate(county_fips = as.numeric(county_fips),
          state_fips = as.numeric(state_fips))
 view(election_data)

###########################################################
# SWING IN COUNTIES (REPUBLICAN VOTE SHARE IN 2016 VS 2012)
###########################################################
#caclulate the swing in counties (republican vote share in 2016 vs 2012) ### COUNTIES
#2016
election_data_2016 <- election_data %>%
  filter(year == 2016)
view(election_data_2016)

# total votes and the Republican vote share by state
rep_county2016 <- election_data_2016 %>%
  group_by(state, county_name, party) %>%
  summarise(total_votes = sum(candidatevotes), .groups = 'drop') %>%
  pivot_wider(names_from = party, values_from = total_votes, values_fill = 0) %>%
  mutate(rep_county2016 = REPUBLICAN / (DEMOCRAT + REPUBLICAN + OTHER)) %>%  # Calculate Republican vote share
  select(state, county_name, rep_county2016)

# have state, state_po, state_fips, county_fips, county_name, totalvotes in rep_county2016
#rep_county2016 <- rep_county2016 %>%
#  left_join(election_data_2016 %>% select(state, state_po, state_fips, county_fips, county_name, totalvotes) %>% distinct(), by = c("state", "county_name"))
#view(rep_county2016)

#above but in 2012
election_data_2012 <- election_data %>%
  filter(year == 2012)
view(election_data_2012)

# total votes and the Republican vote share by state
rep_county2012 <- election_data_2012 %>%
  group_by(state, county_name, party) %>%
  summarise(total_votes = sum(candidatevotes), .groups = 'drop') %>%
  pivot_wider(names_from = party, values_from = total_votes, values_fill = 0) %>%
  mutate(rep_county2012 = REPUBLICAN / (DEMOCRAT + REPUBLICAN + OTHER)) %>%  # Calculate Republican vote share
  select(state, county_name, rep_county2012)

# have state, state_po, state_fips, county_fips, county_name, totalvotes, rep_county2012
#rep_county2012 <- rep_county2012 %>%
#  left_join(election_data_2012 %>% select(state, state_po, state_fips, county_fips, county_name, totalvotes) %>% distinct(), by = c("state", "county_name"))
#view(rep_county2012)

#IGNORE MERGE FOR NOW
# Merge 2012 and 2016 Republican vote share data
#merged_data_counties <- rep_county2012 %>%
#  inner_join(rep_county2016, by = c("state", "state_fips", "county_fips", "county_name"), suffix = c("_2012", "_2016"))

merged_data_counties <- rep_county2012 %>%
  inner_join(rep_county2016, by = c("state",  "county_name"), suffix = c("_2012", "_2016"))

# Calculate the difference in Republican vote share between 2016 and 2012
merged_data_counties <- merged_data_counties %>%
    mutate(swingrep = rep_county2016 - rep_county2012)

# Add state_po from election data into merged_data_counties
merged_data_counties <- merged_data_counties %>%
  left_join(election_data %>% select(state, state_po, county_name) %>% distinct(), by = c("state", "county_name"))

view(merged_data_counties)

#lower state and county names ######
merged_data_counties_lower <- merged_data_counties %>%
    mutate(county_name = tolower(county_name))
view(merged_data_counties_lower)


##################################
# UNEMPLOYMENT DATA FROM FRED API
##################################
state_po <- unique(merged_data_counties_lower$state_po)
length(state_po)

# Define a function to fetch unemployment data for a given state postal code
fetch_unemployment_data <- function(state_po) {
  # Search for series related to the state
  unemp_search <- fredr_series_search_text(
    search_text = paste(state_po, "county unemployment"),
    filter_variable = "frequency",
    filter_value = "Monthly"
  ) %>%
    filter(str_starts(id, state_po) & str_starts(title, "Unemployment Rate in"))
  
  # Function to fetch data for a single FRED ID
  fetch_data_for_id <- function(series_id) {
    tryCatch({
      fredr(
        series_id = series_id,
        observation_start = as.Date("2012-11-06"),
        observation_end = as.Date("2016-11-08")
      ) %>%
        mutate(series_id = series_id)
    }, error = function(e) {
      NULL  # Handle errors gracefully
    })
  }
  
  # Apply the function to all FRED IDs
  all_data <- lapply(unemp_search$id, fetch_data_for_id)
  
  # Combine all the results into a single dataframe
  final_data <- bind_rows(all_data)
  
  # Add the title from unemp_search to the final_data to correspond its id
  final_data <- final_data %>%
    left_join(unemp_search %>% select(id, title), by = c("series_id" = "id"))
  
  # Calculate the average 'value' for each series_id
  average_data <- final_data %>%
    group_by(series_id, title) %>%
    summarise(unemp = (mean(value, na.rm = TRUE))/100, .groups = 'drop')
  
  # Extract state_po from the title in average_data
  average_data <- average_data %>%
    mutate(state_po = str_extract(title, "[A-Z]{2}$"))
  
  # Extract county names from the title in average_data
  average_data <- average_data %>%
    mutate(county_name = str_extract(title, "(?<=Unemployment Rate in )[^,]+")) %>%
    mutate(county_name = tolower(county_name)) %>%
    mutate(county_name = str_replace(county_name, " county.*$", ""))
  
  return(average_data)
}

# Get unique state postal codes from election_data
state_abbreviations <- unique(as.character(election_data$state_po))

# Fetch unemployment data for all states
all_unemployment_data <- lapply(state_abbreviations, fetch_unemployment_data)

# Combine all state data into a single dataframe
final_unemployment_data <- bind_rows(all_unemployment_data)
View(final_unemployment_data)
# Merge average_data into merged_data_counties based on county_name and state_po
merged_data_counties_lower <- merged_data_counties_lower %>%
  left_join(final_unemployment_data %>% select(state_po, county_name, unemp), by = c("state_po", "county_name"))

# Preview the updated merged_data_counties
view(merged_data_counties_lower)

write.csv(merged_data_counties_lower, "/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/merged_data_counties_lower.csv")
#saved it because the function

#import that saved file
merged_data_counties <- read.csv("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/merged_data_counties_lower.csv")
view(merged_data_counties) #includes FRED api called county level (unemployment) data

# List all rows with invalid numbers (e.g., NA or Inf) in merged_data_counti
invalid_data <- merged_data_counties %>%
  filter(is.na(rep_county2016) | is.na(rep_county2012) | is.na(swingrep) | is.na(unemp) |
         is.infinite(rep_county2016) | is.infinite(rep_county2012) | is.infinite(swingrep) | is.infinite(unemp))

# View the invalid data
view(invalid_data)
#by now, alaska (AZ), a lot of indiana (IN) louisiana (LA), maine(ME), oregon (OR)(some Virginia (VA)) data is missing, lets look at this.

# Remove Alaska (AK) from merged_data_counties
unemp_data <- merged_data_counties %>%
  filter(state_po != "AK")

##############################################################################
# COUNTY DEMOGRAPHICS (AGE (split into 4 groups), SEX (%MALE), RACE (%WHITE))
##############################################################################
# Read the text file containing county race data
#county_race <- read.table(gzfile("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/us.1969_2022.19ages.adjusted.txt.gz"))

# Adjust these based on the dictionary
column_widths <- c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8) 

# Define column names based on the dictionary
column_names <- c("year", "state_po", "state_fips", 
                  "county_fips", "registry", "race", "origin", 
                  "sex", "age", "population")

# Read the gzipped text file containing county race data efficiently
county_race <- read.fwf(gzfile("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/us.1969_2022.19ages.adjusted.txt.gz"), 
              widths = column_widths, col.names = column_names)
          #     ,header = FALSE, sep = "\t", stringsAsFactors = FALSE, nrows = 500)

#saving it because computer took over 2 hours to run the above -> full data
write.csv(county_race, "/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/county_race.csv")

county_race <- read.csv("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/county_race.csv")

# Filter county_race data for the years 2012 and 2016
county_race_filtered <- county_race %>%
  filter(year %in% c(2012, 2016))

write.csv(county_race_filtered, "/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/county_race_filtered.csv")

county_race_filtered <- read.csv("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/county_race_filtered.csv")

# Preview the filtered county_race data
view(county_race_filtered)

# Remove the 'origin' column from county_race
county_race_filtered <- county_race_filtered %>%
  select(-origin)

county_race_filtered <- county_race_filtered %>%
  select(-X) 

county_race_filtered <- county_race_filtered %>%
  select(-X.1) 

# Remove the 'registry' column from county_race
county_race_filtered <- county_race_filtered %>%
  select(-registry)

#remove rows with '00-04' age group (0-19 years old) -> unable to vote (i know it's 18+ to vote but '04' age group includes ages 15-19)
county_race_filtered <- county_race_filtered %>%
  filter(!age %in% c(0, 1, 2, 3, 4))

# Preview the updated county_race_filtered data
head(county_race_filtered)
view(county_race_filtered)

# from website:
# ages 20-29 = group 5-6 = Young Adults
# ages 30-44 = group 7-9 = Early Middle-Age Adults
# ages 45-64 = group 10-13 = Late Middle-Age Adults
# ages 65+ = group 14-18 = Seniors

# race 1 = White -> percentage_white
# race 2 = Black
# race 3 = Other 

# sex 1 = Male -> percentage_male
# sex 2 = Femal 

county_race2012 <- county_race_filtered %>%
  filter(year == 2012)

# Combine age groups 5 and 6 as a proportion of the total population for each county in 2012
county_race2012 <- county_race2012 %>%
  group_by(state_po, state_fips, county_fips) %>%
  mutate(total_population = sum(population),
         ages_20_29 = sum(population[age %in% c(5, 6)]) / total_population,
         ages_30_44 = sum(population[age %in% c(7, 8, 9)]) / total_population,
         ages_45_64 = sum(population[age %in% c(10, 11, 12, 13)]) / total_population,
         ages_65_plus = sum(population[age %in% c(14, 15, 16, 17, 18)]) / total_population,
         percentage_male = sum(population[sex == 1]) / total_population,
         percentage_white = sum(population[race == 1]) / total_population) %>%
  ungroup()

#grab all columns except race, sex, age and population, and grab unique state_fips and county_fips
county_race2012 <- county_race2012 %>%
  select(state_po, state_fips, county_fips, total_population, ages_20_29, ages_30_44, ages_45_64, ages_65_plus, percentage_male, percentage_white) %>%
  distinct()

view(county_race2012)

# have state, state_po, state_fips, county_fips, county_name, totalvotes, rep_county2012
rep_county2012 <- rep_county2012 %>%
  left_join(election_data_2012 %>% select(state, state_po, state_fips, county_fips, county_name, totalvotes) %>% distinct(), by = c("state", "county_name"))
view(rep_county2012)

# merge rep_county2012 and county_race2012
merge_county2012 <- rep_county2012 %>%
  left_join(county_race2012, by = c("state_po", "state_fips", "county_fips"))

#calculate vote turnout
merge_county2012  <- merge_county2012 %>%
  mutate(turnout = totalvotes / total_population) %>%
  select(-total_population) %>% #remove total_population
  select(-totalvotes) #remove state_po

view(merge_county2012)

####above but in 2016
county_race2016 <- county_race_filtered %>%
  filter(year == 2016)

# Combine age groups 5 and 6 as a proportion of the total population for each county in 2016

county_race2016 <- county_race2016 %>%
  group_by(state_po, state_fips, county_fips) %>%
  mutate(total_population = sum(population),
         ages_20_29 = sum(population[age %in% c(5, 6)]) / total_population,
         ages_30_44 = sum(population[age %in% c(7, 8, 9)]) / total_population,
         ages_45_64 = sum(population[age %in% c(10, 11, 12, 13)]) / total_population,
         ages_65_plus = sum(population[age %in% c(14, 15, 16, 17, 18)]) / total_population,
         percentage_male = sum(population[sex == 1]) / total_population,
         percentage_white = sum(population[race == 1]) / total_population) %>%
  ungroup()

county_race2016 <- county_race2016 %>%
  select(state_po, state_fips, county_fips, total_population, ages_20_29, ages_30_44, ages_45_64, ages_65_plus, percentage_male, percentage_white) %>%
  distinct()

view(county_race2016)

# have state, state_po, state_fips, county_fips, county_name, totalvotes in rep_county2016
rep_county2016 <- rep_county2016 %>%
  left_join(election_data_2016 %>% select(state, state_po, state_fips, county_fips, county_name, totalvotes) %>% distinct(), by = c("state", "county_name"))
view(rep_county2016)

# merge rep_county2012 and county_race2012
merge_county2016 <- rep_county2016 %>%
  left_join(county_race2016, by = c("state_po", "state_fips", "county_fips"))

#calculate vote turnout
merge_county2016  <- merge_county2016 %>%
  mutate(turnout = totalvotes / total_population) %>%
  select(-total_population) %>% #remove total_population
  select(-totalvotes) #remove state_po

view(merge_county2016)


## now merge both 2012 and 2016 demographics data - with the differences between the two years
# swingrep = rep_county2016 - rep_county2012

merge_county_swings <- merge_county2012 %>%
  inner_join(merge_county2016, by = c("state_po", "state_fips", "county_fips"), suffix = c("_2012", "_2016")) %>%
  mutate(swingrep = rep_county2016 - rep_county2012) %>%
  mutate(
    swingages_20_29 = ages_20_29_2016 - ages_20_29_2012,
    swingages_30_44 = ages_30_44_2016 - ages_30_44_2012,
    swingages_45_64 = ages_45_64_2016 - ages_45_64_2012,
    swingages_65_plus = ages_65_plus_2016 - ages_65_plus_2012,
    swingmale = percentage_male_2016 - percentage_male_2012,
    swingwhite = percentage_white_2016 - percentage_white_2012,
    swingturnout = turnout_2016 - turnout_2012
  )

view(merge_county_swings)

#rename columns
merge_county_swings <- merge_county_swings %>%
  rename(state = state_2012, county_name = county_name_2012)

# View rows with NA values in merge_county_full
na_rows_merge_county_swings <- merge_county_swings %>%
  filter(is.na(rep_county2016) | is.na(rep_county2012) | is.na(swingrep) | is.na(ages_20_29_2012) | is.na(ages_20_29_2016) |
         is.na(ages_30_44_2012) | is.na(ages_30_44_2016) | is.na(ages_45_64_2012) | is.na(ages_45_64_2016) | is.na(ages_65_plus_2012) |
         is.na(ages_65_plus_2016) | is.na(percentage_male_2012) | is.na(percentage_male_2016) | is.na(percentage_white_2012) |
         is.na(percentage_white_2016) | is.na(turnout_2012) | is.na(turnout_2016))

# View the rows with NA values
view(na_rows_merge_county_swings)

#All of Alaska is missing, and one each of Indiana, Louisiana, Maine, Oregon, and Virginia are missing
#remove Alaska (AK) from merge_county_swings
merge_county_swings <- merge_county_swings %>%
  filter(!state_po %in% c("AK")) %>%
  filter(!is.na(rep_county2016) & !is.na(rep_county2012) & !is.na(swingrep) & !is.na(ages_20_29_2012) & !is.na(ages_20_29_2016) &
    !is.na(ages_30_44_2012) & !is.na(ages_30_44_2016) & !is.na(ages_45_64_2012) & !is.na(ages_45_64_2016) & !is.na(ages_65_plus_2012) &
    !is.na(ages_65_plus_2016) & !is.na(percentage_male_2012) & !is.na(percentage_male_2016) & !is.na(percentage_white_2012) &
    !is.na(percentage_white_2016) & !is.na(turnout_2012) & !is.na(turnout_2016))

    # Convert county_name to lowercase
    merge_county_swings <- merge_county_swings %>%
      mutate(county_name = tolower(county_name))

#merge unemployment data
unemp_merged_county_swings <- merge_county_swings %>%
  left_join(unemp_data %>% select(state_po, county_name, unemp), by = c("state_po", "county_name"))

#should run unemp fred api again to do this
#unemp_merged_county_swings <- merge_county_swings %>%
#  left_join(final_unemployment_data %>% select(state_po, county_name, unemp), by = c("state_po", "county_name"))

view(unemp_merged_county_swings)

#make dataset neater by only keeping swings and unemployment
unemp_merged_county_swings <- unemp_merged_county_swings %>%
  select(state, state_po, state_fips, county_fips, county_name, swingrep, swingages_20_29, swingages_30_44, swingages_45_64, swingages_65_plus, swingmale, swingwhite, swingturnout, unemp)

####
#check NA's in unemployment data
# Check for NA values in the unemployment data
na_unemployment_data <- unemp_merged_county_swings %>%
  filter(is.na(unemp))

# View the rows with NA values in the unemployment data
view(na_unemployment_data)
####


###################
# URBAN/RURAL DATA
###################

#theres data from 2023 and 2013, so using 2013
# Read the Excel file containing urban/rural data
urbanrural <- read_xls("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/ruralurbancodes2013.xls")

# Preview the urban/rural data
view(urbanrural)

# Add a new column 'urbanrural' based on the 'Description' column
urbanrural1 <- urbanrural %>%
  mutate(urbanrural = ifelse(grepl("Metro", Description), 1, ifelse(grepl("Nonmetro", Description), 0, NA)))

view(urbanrural1)

# Extract state_fips and county_fips from the FIPS column
urbanrural1 <- urbanrural1 %>%
  mutate(state_fips = substr(FIPS, 1, 2),
         county_fips = substr(FIPS, 3, 5))

# Convert state_fips and county_fips to numeric
urbanrural2 <- urbanrural1 %>% #urbanrural2 for plots
  mutate(state_fips = as.numeric(state_fips),
         county_fips = as.numeric(county_fips))

# Select relevant columns and rename them for clarity
urbanrural1 <- urbanrural2 %>%
  select(state_fips, county_fips, urbanrural)

# Merge urban/rural data with the main dataset
unemp_urban_merged_county_swings <- unemp_merged_county_swings %>%
   left_join(urbanrural1, by = c("state_fips", "county_fips"))

# Preview the updated dataset
   view(unemp_urban_merged_county_swings)

############
#INCOME DATA
############
income <- read.csv("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/CAINC1__ALL_AREAS_1969_2023.csv")

view(income)

# Filter income data to only include rows with "Per capita personal income (dollars)" in the Description column
income_filtered <- income %>%
  filter(Description == "Per capita personal income (dollars) 2/")

  # Select relevant columns and rename them for clarity
  income_filtered <- income_filtered %>%
    select(GeoFIPS, GeoName, X2012, X2013, X2014, X2016) %>%
    rename(income_2012 = X2012, income_2013 = X2013, income_2014 = X2014, income_2016 = X2016)

    # Split GeoFIPS into state_fips and county_fips
    income_filtered <- income_filtered %>%
      mutate(state_fips = substr(GeoFIPS, 1, nchar(GeoFIPS) - 3),
             county_fips = substr(GeoFIPS, nchar(GeoFIPS) - 2, nchar(GeoFIPS)))

    # Convert state_fips and county_fips to numeric
    income_filtered <- income_filtered %>%
      mutate(state_fips = as.numeric(state_fips),
             county_fips = as.numeric(county_fips))

    view(income_filtered)

    # Convert income_2012 and income_2016 to numeric
    income_filtered <- income_filtered %>%
      mutate(income_2012 = as.numeric(gsub(",", "", income_2012)),
             income_2016 = as.numeric(gsub(",", "", income_2016)))

            # Calculate the percentage difference between income_2016 and income_2012
            income_filtered <- income_filtered %>%
              mutate(swingincome = ((income_2016 - income_2012) / income_2012))

            # Preview the updated income_filtered data
            view(income_filtered)

            # Select state_fips, county_fips, and swingincome
            income_filtered <- income_filtered %>%
              select(state_fips, county_fips, swingincome)

            # Merge income data with the main dataset
            unemp_urban_inc_merged_county_swings <- unemp_urban_merged_county_swings %>%
              left_join(income_filtered, by = c("state_fips", "county_fips"))

            # Preview the updated dataset
            view(unemp_urban_inc_merged_county_swings)

################
# EDUCATION DATA
################ 
educ <- read.csv("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/Education.csv")

view(educ)

# THERES ONLY 2008-12 AND 2018-2022 DATA -> MISSING 2016 DATA
# choose 2008-2012 for 2012 data and 2018-2022 data for close to 2016

# Filter education data to only include rows with the specified attributes
educ_filtered <- educ %>%
  filter(Attribute %in% c("Percent of adults with a bachelor's degree or higher, 2008-12", 
                          "Percent of adults with a bachelor's degree or higher, 2018-22"))

# Pivot the data to have separate columns for 2008-12 and 2018-22
educ_pivot <- educ_filtered %>%
  pivot_wider(names_from = Attribute, values_from = Value) %>%
  rename(educ_2012 = `Percent of adults with a bachelor's degree or higher, 2008-12`,
         educ_2018 = `Percent of adults with a bachelor's degree or higher, 2018-22`)

view(educ_pivot)

#Split FIPS.Code into state_fips and county_fips
educ_pivot <- educ_pivot %>%
  mutate(state_fips = substr(FIPS.Code, 1, nchar(FIPS.Code) - 3),
         county_fips = substr(FIPS.Code, nchar(FIPS.Code) - 2, nchar(FIPS.Code))) %>%
  mutate(county_fips = sub("^0+", "", county_fips))


#Interpolate between 2008–2012 and 2018–2022 to estimate education levels for 2016.
#conveniently 2016 is in the middle of 2008-2012 and 2018-2022 so average of both added is interpolated as 2016 data
educ_pivot <- educ_pivot %>%
  mutate(educ_2016 = (educ_2012 + educ_2018) / 2) %>%
  mutate(swingeduc = (educ_2016 - educ_2012) / educ_2012)

# Select relevant columns
educ_pivot <- educ_pivot %>%
  select(state_fips, county_fips, swingeduc)

  # Convert state_fips and county_fips to numeric
  educ_pivot <- educ_pivot %>%
    mutate(state_fips = as.numeric(state_fips),
           county_fips = as.numeric(county_fips))

# Merge education data with the main dataset
unemp_urban_inc_educ_merged_county_swings <- unemp_urban_inc_merged_county_swings %>%
  left_join(educ_pivot, by = c("state_fips", "county_fips"))

# Preview the updated dataset
view(unemp_urban_inc_educ_merged_county_swings)

final_data <- unemp_urban_inc_educ_merged_county_swings

# Remove columns ending with 2012 and 2016
final_data <- final_data %>%
  select(-ends_with("2012"), -ends_with("2016"))

write.csv(final_data, "/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/final_data.csv")

########################################################################################################################################################################################################

# END OF MERGING ##########################
#Now we have the dataset ready for analysis:

########################################################################################################################################################################################################

final_data <- read.csv("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/final_data.csv")

view(final_data)
colnames(final_data)


# final_data: (all county-level data)
# DEPENDENT VARIABLE:
# swingrep = change in republican vote share between 2016 and 2012 (decimals)
# INDEPENDANT VARIABLES: 
# swingages_20_29 = change in proportion of 20-29 year olds of population between 2016 and 2012 (decimals)
# swingages_30_44 = change in proportion of 30-44 year olds of population between 2016 and 2012 (decimals)
# swingages_45_65 = change in proportion of 45-65 year olds of population between 2016 and 2012 (decimals)
# swingages_65_plus = change in proportion of 65+ year olds of population between 2016 and 2012 (decimals)
# swingmale = change in proportion of male population between 2016 and 2012 (decimals)
# swingwhite = change in proportion of white population between 2016 and 2012 (decimals)
# swingturnout = change in proportion of population who voted between 2016 and 2012 (decimals)
# unemp = county level unemployment rate (average between 2012 and 2016) (decimals)
# urbanrural = if county is urban (1) or rural (0) (binary)
# swingincome = change in per-capita personal income between 2016 and 2012 (decimals)
# swingeduc = change in proportion of population with a bachelor's degree or higher between 2016 and 2012 (decimals)

#colSums(is.na(final_data))

# Check which rows have missing income data
#income_missing <- final_data[is.na(final_data$swingincome), ]
#view(missing_income_data)

#still 184 counties with no unemployment data
#unemp_missing <- final_data[is.na(final_data$unemp),]
#view(unemp_missing)

# Remove rows with missing income data
final_data <- final_data %>%
  filter(!is.na(swingincome)) 

# Remove rows with missing education data
final_data <- final_data %>%
  filter(!is.na(swingeduc))

#remove rows with missing unemployment data
final_data <- final_data %>%
  filter(!is.na(unemp))

#remove state_po, state_fips, county_fips
final_data <- final_data %>%
  select(-X, -state_po, -state_fips, -county_fips)

summary(final_data)

# Summary of final_data
summary_table <- final_data %>%
  summarise(
    swingrep_mean = mean(swingrep, na.rm = TRUE),
    swingrep_sd = sd(swingrep, na.rm = TRUE),
    swingrep_median = median(swingrep, na.rm = TRUE),
    swingrep_min = min(swingrep, na.rm = TRUE),
    swingrep_max = max(swingrep, na.rm = TRUE),
    swingages_20_29_mean = mean(swingages_20_29, na.rm = TRUE),
    swingages_20_29_sd = sd(swingages_20_29, na.rm = TRUE),
    swingages_20_29_median = median(swingages_20_29, na.rm = TRUE),
    swingages_20_29_min = min(swingages_20_29, na.rm = TRUE),
    swingages_20_29_max = max(swingages_20_29, na.rm = TRUE),
    swingmale_mean = mean(swingmale, na.rm = TRUE),
    swingmale_sd = sd(swingmale, na.rm = TRUE),
    swingmale_median = median(swingmale, na.rm = TRUE),
    swingmale_min = min(swingmale, na.rm = TRUE),
    swingmale_max = max(swingmale, na.rm = TRUE),
    swingwhite_mean = mean(swingwhite, na.rm = TRUE),
    swingwhite_sd = sd(swingwhite, na.rm = TRUE),
    swingwhite_median = median(swingwhite, na.rm = TRUE),
    swingwhite_min = min(swingwhite, na.rm = TRUE),
    swingwhite_max = max(swingwhite, na.rm = TRUE),
    swingturnout_mean = mean(swingturnout, na.rm = TRUE),
    swingturnout_sd = sd(swingturnout, na.rm = TRUE),
    swingturnout_median = median(swingturnout, na.rm = TRUE),
    swingturnout_min = min(swingturnout, na.rm = TRUE),
    swingturnout_max = max(swingturnout, na.rm = TRUE),
    unemp_mean = mean(unemp, na.rm = TRUE),
    unemp_sd = sd(unemp, na.rm = TRUE),
    unemp_median = median(unemp, na.rm = TRUE),
    unemp_min = min(unemp, na.rm = TRUE),
    unemp_max = max(unemp, na.rm = TRUE),
    urbanrural_mean = mean(urbanrural, na.rm = TRUE),
    urbanrural_sd = sd(urbanrural, na.rm = TRUE),
    urbanrural_median = median(urbanrural, na.rm = TRUE),
    urbanrural_min = min(urbanrural, na.rm = TRUE),
    urbanrural_max = max(urbanrural, na.rm = TRUE),
    swingincome_mean = mean(swingincome, na.rm = TRUE),
    swingincome_sd = sd(swingincome, na.rm = TRUE),
    swingincome_median = median(swingincome, na.rm = TRUE),
    swingincome_min = min(swingincome, na.rm = TRUE),
    swingincome_max = max(swingincome, na.rm = TRUE),
    swingeduc_mean = mean(swingeduc, na.rm = TRUE),
    swingeduc_sd = sd(swingeduc, na.rm = TRUE),
    swingeduc_median = median(swingeduc, na.rm = TRUE),
    swingeduc_min = min(swingeduc, na.rm = TRUE),
    swingeduc_max = max(swingeduc, na.rm = TRUE)
  )

# Convert to percentages where applicable
summary_table <- summary_table %>%
  mutate(
    swingrep_mean = swingrep_mean * 100,
    swingrep_sd = swingrep_sd * 100,
    swingrep_median = swingrep_median * 100,
    swingrep_min = swingrep_min * 100,
    swingrep_max = swingrep_max * 100,
    swingages_20_29_mean = swingages_20_29_mean * 100,
    swingages_20_29_sd = swingages_20_29_sd * 100,
    swingages_20_29_median = swingages_20_29_median * 100,
    swingages_20_29_min = swingages_20_29_min * 100,
    swingages_20_29_max = swingages_20_29_max * 100,
    swingmale_mean = swingmale_mean * 100,
    swingmale_sd = swingmale_sd * 100,
    swingmale_median = swingmale_median * 100,
    swingmale_min = swingmale_min * 100,
    swingmale_max = swingmale_max * 100,
    swingwhite_mean = swingwhite_mean * 100,
    swingwhite_sd = swingwhite_sd * 100,
    swingwhite_median = swingwhite_median * 100,
    swingwhite_min = swingwhite_min * 100,
    swingwhite_max = swingwhite_max * 100,
    swingturnout_mean = swingturnout_mean * 100,
    swingturnout_sd = swingturnout_sd * 100,
    swingturnout_median = swingturnout_median * 100,
    swingturnout_min = swingturnout_min * 100,
    swingturnout_max = swingturnout_max * 100,
    unemp_mean = unemp_mean * 100,
    unemp_sd = unemp_sd * 100,
    unemp_median = unemp_median * 100,
    unemp_min = unemp_min * 100,
    unemp_max = unemp_max * 100,
    swingincome_mean = swingincome_mean * 100,
    swingincome_sd = swingincome_sd * 100,
    swingincome_median = swingincome_median * 100,
    swingincome_min = swingincome_min * 100,
    swingincome_max = swingincome_max * 100,
    swingeduc_mean = swingeduc_mean * 100,
    swingeduc_sd = swingeduc_sd * 100,
    swingeduc_median = swingeduc_median * 100,
    swingeduc_min = swingeduc_min * 100,
    swingeduc_max = swingeduc_max * 100
  )

# Print the summary table
view(summary_table)

##############################################
# MODELLING VOTING BEHAVIOR (REGRESSION MODEL)
##############################################
##############################################
# OLS regression model
# Fit a linear regression model to explain the drivers of voting behavior
##############################################
voting_model <- lm(swingrep ~ swingages_20_29 + swingmale + swingwhite + swingturnout + unemp + urbanrural + swingincome + swingeduc, data = final_data)

# Summarize the model
summary(voting_model)

# Report the extent to which the model accounts for voting behavior
r_squared <- summary(voting_model)$r.squared
cat("R-squared: ", r_squared, "\n")

# Comment on which variables are most important in the model
coefficients <- summary(voting_model)$coefficients
cat("Coefficients:\n")
print(coefficients)

################
# MULTICOLLINEARITY
################
# Check for multicollinearity using Variance Inflation Factor (VIF)
# Calculate VIF for each predictor variable
# only kept ages 20-29 or otherwise there is perfect multicollienarity
vif(voting_model)

##################################
# RESIDUAL DIAGNOSTICS (WITH PLOT)
##################################
# Check for homoscedasticity
# Plot residuals vs. fitted values

par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid
plot(voting_model, which = 1)  # Residuals vs Fitted
plot(voting_model, which = 2)  # Q-Q Plot
plot(voting_model, which = 3)  # Scale-Location
plot(voting_model, which = 4)  # Residuals vs Leverage
par(mfrow = c(1, 1))  # Reset to default layout

# check for normality of residuals
shapiro.test(residuals(voting_model))
#Non-Normality of Residuals: 
#The residuals deviate significantly from normality. Address this by applying diagnostic plots, transformations, or bootstrapping for inference.

# check for homoscedasticity
# Breusch-Pagan test
bptest(voting_model)
#Heteroscedasticity: 
#There is evidence of non-constant variance in residuals. 
#Address this by using robust standard errors or alternative modeling techniques like WLS.

# Apply robust standard errors to the voting model
robust_voting_model <- coeftest(voting_model, vcov = vcovHC(voting_model, type = "HC1"))

# Print the robust standard errors
print(robust_voting_model)


# Residual diagnostics for robust_voting_model
# Extract residuals and fitted values
residuals_robust <- residuals(voting_model)
fitted_values_robust <- fitted(voting_model)

# Plot residuals vs. fitted values
png("/Users/Desktop/robust_voting_model_residual_diagnostics_combined.png", width = 1600, height = 1600)
par(mfrow = c(2, 2))  # Arrange plots in a 2x2 grid

# Residuals vs Fitted Values
plot(fitted_values_robust, residuals_robust, 
  main = "Residuals vs Fitted Values (Robust Model)",
  xlab = "Fitted Values", ylab = "Residuals", cex.main = 3)
abline(h = 0, col = "red")

# Q-Q plot of residuals
qqnorm(residuals_robust, main = "Q-Q Plot of Residuals (Robust Model)", cex.main = 3)
qqline(residuals_robust, col = "red")

# Scale-Location plot
plot(fitted_values_robust, sqrt(abs(residuals_robust)), 
  main = "Scale-Location Plot (Robust Model)",
  xlab = "Fitted Values", ylab = "Square Root of |Residuals|", cex.main = 3)
abline(h = 0, col = "red")

# Cook's Distance for robust_voting_model
cooksd <- cooks.distance(voting_model)

# Plot Cook's Distance
plot(cooksd, type = "h", main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Observation Index", cex.main = 3)
abline(h = 4/(nrow(final_data) - length(coef(voting_model))), col = "red")  # Add a threshold line

par(mfrow = c(1, 1))  # Reset to default layout
dev.off()

################
# BOOTSTRAPPING
################
library(boot)

# Define the bootstrap function
bootstrap_fn <- function(final_data, indices) {
  # Resample the data
  resampled_data <- final_data[indices, ]
  # Fit the model to the resampled data
  model <- lm(voting_model, data = resampled_data)
  # Return the coefficients
  return(coef(model))
}

# Perform bootstrapping with 1000 resamples
set.seed(46)  # For reproducibility
bootstrap_results <- boot(data = final_data, statistic = bootstrap_fn, R = 1000)

# Display bootstrap results
print(bootstrap_results)

# Extract and summarize bootstrap results
bootstrap_summary <- data.frame(
  Original = coef(lm(voting_model, data = final_data)),
  BootstrapMean = apply(bootstrap_results$t, 2, mean),
  Bias = apply(bootstrap_results$t, 2, mean) - coef(lm(voting_model, data = final_data)),
  StdError = apply(bootstrap_results$t, 2, sd)
)
view(bootstrap_summary)
print(bootstrap_summary)

# Visualize bootstrap distributions for each coefficient
dev.copy(png, "/Users/Desktop/bootstrap_sampling_distributions.png")
par(mfrow = c(3, 3))  # Arrange plots
coef_names <- names(coef(lm(voting_model, data = final_data)))
for (i in 1:ncol(bootstrap_results$t)) {
  hist(bootstrap_results$t[, i], breaks = 30, main = paste("Bootstrap Dist. of", coef_names[i]),
       xlab = "Coefficient Value", col = "lightblue", border = "black", freq = FALSE)
  abline(v = bootstrap_summary$Original[i], col = "red", lwd = 2)  # Mark original coefficient
}
dev.off()
par(mfrow = c(1, 1))  # Reset plotting layout

# Create a data frame from the bootstrap_results
bootstrap_results_df <- data.frame(
  Intercept = bootstrap_results$t[, 1],
  swingages_20_29 = bootstrap_results$t[, 2],
  swingmale = bootstrap_results$t[, 3],
  swingwhite = bootstrap_results$t[, 4],
  swingturnout = bootstrap_results$t[, 5],
  unemp = bootstrap_results$t[, 6],
  urbanrural = bootstrap_results$t[, 7],
  swingincome = bootstrap_results$t[, 8],
  swingeduc = bootstrap_results$t[, 9]
)

# View the bootstrap_results_df
view(bootstrap_results_df)

##################
# SIMULATION STUDY
################### Load required libraries
library(MASS)  # For mvrnorm function
library(grid)  # For textGrob function
set.seed(46)

# Define parameters for simulation
n <- 100  # Number of observations
p <- 8    # Number of predictors (excluding intercept)
num_simulations <- 10000  # Number of simulation iterations

# True coefficients for the predictors
# Use the coefficients from the voting_model 
true_beta <- coefficients(voting_model)

#The coefficients from your linear regression model represent the observed relationships between predictors and the response variable in your real dataset.
#Using these as the "true" coefficients ensures that the simulation reflects the patterns in your data.
#Interpretability:

#The simulation study will help assess the variability of your observed coefficients around the assumed "true" coefficients.
#This provides insight into the stability and reliability of your model’s estimates.
#Consistency:

#Aligning the simulation’s true 
#𝛽 with the estimated coefficients ensures that the sampling distributions you generate are meaningful and relevant to your actual analysis.


# Extract residual standard error
rse <- summary(voting_model)$sigma
epsilon <- rnorm(n, mean = 0, sd = rse)

# Function to simulate data and fit a regression model
simulate_regression <- function(n, true_beta, rse) {
  # Generate predictors
  X <- cbind(
    1,  # Intercept
    rnorm(n, mean = 0, sd = 1),  # swingages_20_29
    rnorm(n, mean = 0, sd = 1),  # swingmale
    rnorm(n, mean = 0, sd = 1),  # swingwhite
    rnorm(n, mean = 0, sd = 1),  # swingturnout
    rnorm(n, mean = 0, sd = 1),  # unemp
    rbinom(n, 1, prob = 0.5),    # urbanrural (binary)
    rnorm(n, mean = 0, sd = 1),  # swingincome
    rnorm(n, mean = 0, sd = 1)   # swingeduc
  )
  
  # Generate random noise based on the RSE
  epsilon <- rnorm(n, mean = 0, sd = rse)
  
  # Generate the response variable (swingrep)
  y <- X %*% true_beta + epsilon
  
  # Fit OLS regression to the simulated data
  model <- lm(y ~ X[, -1])  # Exclude intercept column from predictors in lm
  
  return(coef(model))  # Return estimated coefficients
}

# Run simulations
simulation_results <- replicate(num_simulations, simulate_regression(n, true_beta, rse))

# Convert results to a data frame for analysis
coefficients_df <- as.data.frame(t(simulation_results))
colnames(coefficients_df) <- c("Intercept", "swingages_20_29", "swingmale", "swingwhite", 
                               "swingturnout", "unemp", "urbanrural", "swingincome", "swingeduc")

# Plot the sampling distributions
dev.copy(png, "/Users/Desktop/DGSsampling_distributions.png")
par(mfrow = c(3, 3))  # Set up a 3x3 grid for the plots
for (coeff in names(coefficients_df)) {
  hist(coefficients_df[[coeff]], probability = TRUE, 
       main = paste(coeff), 
       xlab = "Coefficient Value", col = "lightblue", border = "white")
  lines(density(coefficients_df[[coeff]]), col = "red", lwd = 2)
  abline(v = mean(coefficients_df[[coeff]]), col = "blue", lwd = 2, lty = 2)
}
dev.off()
par(mfrow = c(1, 1))  # Reset the plotting layout

# Summarize results for each coefficient
simulated_stats <- apply(coefficients_df, 2, function(x) {
  c(mean = mean(x), sd = sd(x), bias = mean(x) - true_beta[which(colnames(coefficients_df) == colnames(coefficients_df)[colnames(coefficients_df) == x])])
})
simulated_stats <- t(simulated_stats)
rownames(simulated_stats) <- colnames(coefficients_df)

# Display summary statistics
simulated_stats
#                       mean          sd 
#Intercept        0.02783492 0.007639762
#swingages_20_29  0.93547200 0.005315455
#swingmale        0.25419679 0.005360549
#swingwhite       1.30065154 0.005467263
#swingturnout     0.15166722 0.005453402
#unemp            0.47707431 0.005424443
#urbanrural      -0.02935873 0.010663801
#swingincome     -0.07944487 0.005462150
#swingeduc        0.04406485 0.005371428

#bias for simulation
bias <- colMeans(coefficients_df) - true_beta
print(bias)
#      Intercept swingages_20_29       swingmale      swingwhite    swingturnout 
#  -7.244058e-05    2.769785e-05    4.641792e-05   -2.374048e-06    7.005312e-05 
#          unemp      urbanrural     swingincome       swingeduc 
#   1.488073e-05   -2.612753e-08    7.602233e-05    1.623392e-05 

print(bootstrap_summary)
# Remove the "Original" column from bootstrap_summary
bootstrap_summary <- bootstrap_summary %>%
  select(-Original)

# Print the updated bootstrap_summary
view(bootstrap_summary)

simulated_stats2 <- data.frame(simulated_stats)
simulated_stats2$bias <- bias


# Combine bootstrap and simulation results for comparison
comparison_df <- data.frame(
  Coefficient = rownames(bootstrap_summary),
  Bootstrap_Mean = bootstrap_summary$BootstrapMean,
  Bootstrap_SD = bootstrap_summary$StdError,
  Simulation_Mean = simulated_stats2$mean,
  Simulation_SD = simulated_stats2$sd,
  Bootstrap_Bias = bootstrap_summary$Bias,
  Simulation_Bias = simulated_stats2$bias
)

# Print the comparison data frame
print(comparison_df)
view(comparison_df)


bootstrap_results_df <- as.data.frame(boot_results$t)
colnames(bootstrap_results_df) <- rownames(bootstrap_summary)
view(bootstrap_results_df)

#############################################################################################
#Density Plot of Sampling Distributions vs Bootstrap Distributions
#############################################################################################
# Combine all density plots into one picture
library(ggplot2)
library(gridExtra)

# Combine all density plots into one picture
density_plots <- list()

for (coef in rownames(bootstrap_summary)) {
  # Combine bootstrap and simulation distributions
  combined_data <- data.frame(
    Value = c(bootstrap_results_df[[coef]], rep(coefficients_df[[coef]], length.out = nrow(bootstrap_results_df))),
    Method = rep(c("Bootstrap", "Simulation"), each = nrow(bootstrap_results_df))
  )
  
  # Rename (Intercept) as "Intercept"
  if (coef == "(Intercept)") {
    coef <- "Intercept"
  }
  
  # Plot the density comparison
  p <- ggplot(combined_data, aes(x = Value, fill = Method)) +
    geom_density(alpha = 0.5) +
    labs(
      title = paste("Comparison of Sampling Distributions for", coef),
      x = "Coefficient Value",
      y = "Density"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Bootstrap" = "blue", "Simulation" = "red"))
  
  # Add the plot to the list
  density_plots[[coef]] <- p
}

# Arrange all plots in one picture
combined_density_plots <- grid.arrange(grobs = density_plots, ncol = 2, top = textGrob("Comparison of Sampling Distributions", gp = gpar(fontsize = 20, fontface = "bold")))

# Save the combined plot as an image
ggsave("/Users/Desktop/comparison_density_plotscombines.png", plot = combined_density_plots, width = 15, height = 15)



#############################################################################################

###########################
# MORE PLOTS
###########################

#### INTRODUCTION ####

##############################################################################################
#WHICH COUNTIES SWITCHED IN 2016 (DEMOCRAT TO REPUBLICAN = RED, REPUBLICAN TO DEMOCRAT = BLUE)
##############################################################################################

# winners in counties
county_results <- election_data %>%
    group_by(year, state, county_name) %>%
    filter(candidatevotes == max(candidatevotes)) %>%
    select(year, state, county_name, party) %>%
    mutate(county_name = tolower(county_name)) %>%
    mutate(state = tolower(state))  # Convert state names to lowercase

view(county_results)
view(election_data)
#counties that switched parties between 2012 and 2016 -> swing parties
swing_counties <- county_results %>%
    spread(year, party) %>%
    filter(`2012` != `2016`) %>%
    select(state, county_name, `2012`, `2016`) %>%
    mutate(state = tolower(state))  # Convert state names to lowercase

view(swing_counties)

loyal_counties <- county_results %>%
    spread(year, party) %>%
    filter(`2012` == `2016`) %>%
    select(state, county_name, `2012`, `2016`) %>%
    mutate(state = tolower(state))  # Convert state names to lowercase

view(loyal_counties)

# Load necessary libraries
library(ggplot2)
library(maps)

# Get map data for counties
county_map <- map_data("county")

# Prepare swing counties data for plotting
swing_counties <- swing_counties %>%
  mutate(swing_party = ifelse(`2016` == "REPUBLICAN", "REPUBLICAN", "DEMOCRAT"))

# Merge swing counties data with county map data
county_map <- county_map %>%
  mutate(county_name = tolower(subregion)) %>%
  left_join(swing_counties, by = c("region" = "state", "county_name" = "county_name"))
   
# Plot the map
swing_plot <- ggplot(county_map, aes(x = long, y = lat, group = group, fill = swing_party)) +
  geom_polygon(color = "black", size = 0.1) +
  scale_fill_manual(values = c("REPUBLICAN" = "red", "DEMOCRAT" = "blue", "white")) +
  theme_void() +
  labs(title = "Swing Counties in 2016 Election",
       fill = "Party") +
  theme(legend.position = "bottom") +
  coord_fixed(ratio = 1.3)

swing_plot

# Save the plot as an image
ggsave("swing_counties_2016.png", plot = swing_plot, width = 10, height = 8)

#### swing and loyal counties combines

# Get map data for counties
county_map <- map_data("county")

# Prepare swing counties data for plotting
swing_counties <- swing_counties %>%
  mutate(swing_party = ifelse(`2016` == "REPUBLICAN", "Swing Republican", "Swing Democrat"))

# Prepare loyal counties data for plotting
loyal_counties <- loyal_counties %>%
  mutate(loyal_party = ifelse(`2016` == "REPUBLICAN", "Loyal Republican", "Loyal Democrat"))

view(loyal_counties)
# Merge swing counties data with county map data
county_map <- county_map %>%
  mutate(county_name = tolower(subregion)) %>%
  left_join(swing_counties, by = c("region" = "state", "county_name" = "county_name")) %>%
  left_join(loyal_counties, by = c("region" = "state", "county_name" = "county_name"))

# Plot the map
plot2 <- ggplot(county_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = swing_party), color = "black", size = 0.1) +
  geom_polygon(aes(fill = loyal_party), color = "black", size = 0.1, alpha = 0.5) +
  scale_fill_manual(values = c("Swing Republican" = "red", "Swing Democrat" = "#0000ff", "Loyal Republican" = "darkred", "Loyal Democrat" = "#000048", "white")) +
  theme_void() +
  labs(title = "Swing and Loyal Counties in 2016 Election",
       fill = "Party") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.title = element_text(face = "bold")) +
  coord_fixed(ratio = 1.3) 

plot2
ggsave("/Users/Desktop/swingcounties_2016.png", plot = swing_plot, width = 10, height = 8)
ggsave("/Users/Desktop/swingandloyal_counties_2016.png", plot = plot2, width = 10, height = 8)


###########################
#Republican Vote Share Map
###########################
election_data_2016 <- election_data %>%
  filter(year == 2016)

# Calculate the Republican vote share by county for 2016
rep_vote_share_county_2016 <- election_data_2016 %>%
  group_by(state, county_name, party) %>%
  summarise(total_votes = sum(candidatevotes), .groups = 'drop') %>%
  pivot_wider(names_from = party, values_from = total_votes, values_fill = 0) %>%
  mutate(rep_vote_share = REPUBLICAN / (DEMOCRAT + REPUBLICAN + OTHER)) %>%  # Calculate Republican vote share
  select(state, county_name, rep_vote_share)

# Convert state and county names to lowercase for consistency
rep_vote_share_county_2016 <- rep_vote_share_county_2016 %>%
  mutate(state = tolower(state),
         county_name = tolower(county_name))

# Get map data for counties
county_map <- map_data("county")

# Merge the Republican vote share data with the county map data
county_map <- county_map %>%
  mutate(county_name = tolower(subregion)) %>%
  left_join(rep_vote_share_county_2016, by = c("region" = "state", "county_name" = "county_name"))

# Plot the map
county_vote_share_plot_2016 <- ggplot(county_map, aes(x = long, y = lat, group = group, fill = rep_vote_share)) +
  geom_polygon(color = "black", size = 0.1) +
  scale_fill_gradient(low = "white", high = "red", na.value = "gray") +  # Color gradient from white to red
  theme_void() +
  labs(title = "2016 U.S. Election: Republican Vote Share by County",
       fill = "Vote Share") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  coord_fixed(ratio = 1.3)

county_vote_share_plot_2016

# Save the plot as an image
ggsave("/Users/Desktop/republican_vote_share_by_county_2016.png", plot = county_vote_share_plot_2016, width = 10, height = 8)


election_data_2012 <- election_data %>%
  filter(year == 2012)

  # Calculate the Republican vote share by county for 2012
  rep_vote_share_county_2012 <- election_data_2012 %>%
    group_by(state, county_name, party) %>%
    summarise(total_votes = sum(candidatevotes), .groups = 'drop') %>%
    pivot_wider(names_from = party, values_from = total_votes, values_fill = 0) %>%
    mutate(rep_vote_share = REPUBLICAN / (DEMOCRAT + REPUBLICAN + OTHER)) %>%  # Calculate Republican vote share
    select(state, county_name, rep_vote_share)

  # Convert state and county names to lowercase for consistency
  rep_vote_share_county_2012 <- rep_vote_share_county_2012 %>%
    mutate(state = tolower(state),
           county_name = tolower(county_name))

  # Get map data for counties
  county_map_2012 <- map_data("county")

  # Merge the Republican vote share data with the county map data
  county_map_2012 <- county_map_2012 %>%
    mutate(county_name = tolower(subregion)) %>%
    left_join(rep_vote_share_county_2012, by = c("region" = "state", "county_name" = "county_name"))

  # Plot the map
  county_vote_share_plot_2012 <- ggplot(county_map_2012, aes(x = long, y = lat, group = group, fill = rep_vote_share)) +
    geom_polygon(color = "black", size = 0.1) +
    scale_fill_gradient(low = "white", high = "red", na.value = "gray") +  # Color gradient from white to red
    theme_void() +
    labs(title = "2012 U.S. Election: Republican Vote Share by County",
         fill = "Vote Share") +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    coord_fixed(ratio = 1.3)

  county_vote_share_plot_2012
# Save the plot as an image
ggsave("/Users/Desktop/republican_vote_share_by_county_2012.png", plot = county_vote_share_plot_2012, width = 10, height = 8)



######################################################################
#Change in Republican vote share (2012-2016):
#A choropleth map showing the swing in Republican vote share by county.
######################################################################
# swing in Republican vote share by county
swing_rep_vote_share <- merged_data_counties %>%
  select(state, county_name, swingrep)

# Convert state and county names to lowercase for consistency
swing_rep_vote_share <- swing_rep_vote_share %>%
  mutate(state = tolower(state),
         county_name = tolower(county_name))

# Get map data for counties
county_map <- map_data("county")

# Merge the swing in Republican vote share data with the county map data
county_map <- county_map %>%
  mutate(county_name = tolower(subregion)) %>%
  left_join(swing_rep_vote_share, relationship = "many-to-many", by = c("region" = "state", "county_name" = "county_name"))

# Plot the map
swing_rep_vote_share_plot <- ggplot(county_map, aes(x = long, y = lat, group = group, fill = swingrep)) +
  geom_polygon(color = "black", size = 0.1) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "gray") +  # Color gradient from blue to red
  theme_void() +
  labs(title = "Swing in Republican Vote Share (2012-2016) by County",
       fill = "Swing Vote Share") +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  coord_fixed(ratio = 1.3)

swing_rep_vote_share_plot

# Save the plot as an image
ggsave("/Users/Desktop/swing_rep_vote_share_by_county_2012_2016.png", plot = swing_rep_vote_share_plot, width = 10, height = 8)

#########################################################################
#Boxplots of swingrep by urbanrural:
#Highlight differences in voting behavior between urban and rural counties
#########################################################################
# Boxplots of swingrep by urbanrural
boxplot <- ggplot(final_data, aes(x = factor(urbanrural), y = swingrep, fill = factor(urbanrural))) +
  geom_boxplot() +
  scale_fill_manual(values = c("0" = "lightblue", "1" = "lightgreen"), labels = c("Rural", "Urban")) +
  labs(title = "Swing in Republican Vote Share by Urban/Rural Counties",
       x = "County Type",
       y = "Swing in Republican Vote Share",
       fill = "County Type") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 22, face = "bold"),
    axis.title.x = element_text(size = 18, face = "bold"),
    axis.title.y = element_text(size = 18, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )
boxplot
# Save the boxplot as an image
ggsave("/Users/Desktop/swingrep_by_urbanrural_boxplot.png", plot = boxplot, width = 10, height = 8)

###################################################
#US Counties mapped if theyre rural or not
###################################################
# Plot US Counties mapped if they are rural or not
# Get map data for counties

# Get map data for counties
county_map <- map_data("county")


final_data <- read.csv("/Users/Desktop/year 4/semester 1/ECON0128 Statistical Programming for Social Data Science/second coursework due 17 jan/dataverse_files/final_data.csv")

data_urb <- final_data %>%
  select(state, county_name, urbanrural)

view(data_urb)

data_urb <- data_urb %>%
  mutate(state = tolower(state)) %>%
  mutate(county_name = tolower(county_name))

  # Merge urban/rural data with county map data
  county_map <- county_map %>%
    mutate(county_name = tolower(subregion)) %>%
    left_join(data_urb, relationship = "many-to-many", by = c("region" = "state", "county_name" = "county_name"))

  # Plot the map
  urban_rural_plot <- ggplot(county_map, aes(x = long, y = lat, group = group, fill = factor(urbanrural))) +
    geom_polygon(color = "black", size = 0.1) +
    scale_fill_manual(values = c("0" = "lightblue", "1" = "lightgreen"), labels = c("Rural", "Urban")) +
    theme_void() +
    labs(title = "Urban and Rural Counties in the US",
         fill = "County Type") +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, face = "bold")) +
    coord_fixed(ratio = 1.3)

  urban_rural_plot

ggsave("/Users/Desktop/urban_rural_counties.png", plot = urban_rural_plot, width = 10, height = 8)




##################Part II: Modeling Voting Behavior##################

########################################################################################
#Bar charts or histograms of key variables:
#Distributions of variables (e.g., swingages_20_29, swingmale, swingwhite, swingturnout, swingincome, unemp, swingeduc, urbanrural ).
########################################################################################
# Bar charts or histograms of key variables
# Create histograms for key variables
plot1 <- ggplot(final_data, aes(x = swingages_20_29)) +
  geom_histogram(binwidth = 0.01, fill = "blue", color = "black", alpha = 0.7) +
  labs(title = "Proportion Change of Ages 20-29", x = "SwingAges 20-29", y = "Frequency")

plot2 <- ggplot(final_data, aes(x = swingmale)) +
  geom_histogram(binwidth = 0.01, fill = "green", color = "black", alpha = 0.7) +
  labs(title = "Proportion Change of Males", x = "SwingMale", y = "Frequency")

plot3 <- ggplot(final_data, aes(x = swingwhite)) +
  geom_histogram(binwidth = 0.01, fill = "red", color = "black", alpha = 0.7) +
  labs(title = "Proportion Change of White Population", x = "SwingWhite", y = "Frequency")

plot4 <- ggplot(final_data, aes(x = swingturnout)) +
  geom_histogram(binwidth = 0.01, fill = "purple", color = "black", alpha = 0.7) +
  labs(title = "Voter Turnout Chang", x = "SwingTurnout", y = "Frequency")

plot5 <- ggplot(final_data, aes(x = swingincome)) +
  geom_histogram(binwidth = 0.01, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Proportion Change of Per-Capita Personal Income ", x = "SwingIncome", y = "Frequency")

plot6 <- ggplot(final_data, aes(x = unemp)) +
  geom_histogram(binwidth = 0.01, fill = "cyan", color = "black", alpha = 0.7) +
  labs(title = "Unemployment", x = "Unemployment", y = "Frequency")

plot7 <- ggplot(final_data, aes(x = swingeduc)) +
  geom_histogram(binwidth = 0.01, fill = "pink", color = "black", alpha = 0.7) +
  labs(title = "Proportion Change of Population with Bachelor’s Degree", x = "SwingEduc", y = "Frequency")

plot8 <- ggplot(final_data, aes(x = factor(urbanrural))) +
  geom_bar(fill = "yellow", color = "black", alpha = 0.7) +
  labs(title = "Urban/Rural", x = "UrbanRural", y = "Frequency")

# Arrange all plots in one picture
histograms <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol = 2, top = textGrob("Histograms of Key Variables", gp = gpar(fontsize = 20, fontface = "bold")))

# Save the histograms as an image
ggsave("/Users/Desktop/histograms_key_variables.png", plot = histograms, width = 15, height = 20)


########################################################################################################################
#Scatterplots with regression lines:
#Example: Scatterplots of swingrep vs. key predictors (e.g., swingages_20_29, swingmale, swingwhite, swingturnout, swingincome, unemp, swingeduc, urbanrural ) to show relationships.
########################################################################################################################
# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Create scatterplots with regression lines for each key predictor
plot1 <- ggplot(final_data, aes(x = swingages_20_29, y = swingrep)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Proportion Change of Ages 20-29", x = "SwingAges 20-29", y = "SwingRep")

plot2 <- ggplot(final_data, aes(x = swingmale, y = swingrep)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Proportion Change of Males", x = "SwingMale", y = "SwingRep")

plot3 <- ggplot(final_data, aes(x = swingwhite, y = swingrep)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Proportion Change of White Population", x = "SwingWhite", y = "SwingRep")

plot4 <- ggplot(final_data, aes(x = swingturnout, y = swingrep)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Voter Turnout Change", x = "SwingTurnout", y = "SwingRep")

plot5 <- ggplot(final_data, aes(x = swingincome, y = swingrep)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Proportion Change of Per-Capita Personal Income ", x = "SwingIncome", y = "SwingRep")

plot6 <- ggplot(final_data, aes(x = unemp, y = swingrep)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Unemployment", x = "Unemployment", y = "SwingRep")

plot7 <- ggplot(final_data, aes(x = swingeduc, y = swingrep)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Proportion Change of Population with Bachelor’s Degree", x = "SwingEduc", y = "SwingRep")

plot8 <- ggplot(final_data, aes(x = factor(urbanrural), y = swingrep)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Urban/Rural", x = "UrbanRural", y = "SwingRep")

# Arrange all plots in one picture
scatter_regression <- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol = 2, top = textGrob("Swing in Republican Vote Share vs Control Variables", gp = gpar(fontsize = 20, fontface = "bold")))
# Save the scatterplots with regression lines as an image
ggsave("/Users/Desktop/scatter_regression.png", plot = scatter_regression, width = 15, height = 20)


###################################################################################
#Partial dependence plots:
#Show how changes in individual predictors(e.g., swingages_20_29, swingmale, swingwhite, swingturnout, swingincome, unemp, swingeduc, urbanrural )and how they impact swingrep.
###################################################################################
# Load necessary libraries
library(pdp)
library(gridExtra)

# Create partial dependence plots for each predictor
pdp_swingages_20_29 <- partial(voting_model, pred.var = "swingages_20_29", plot = TRUE, rug = TRUE, main = "Partial Dependence of SwingAges 20-29 on SwingRep", ylab = "SwingRep")
pdp_swingmale <- partial(voting_model, pred.var = "swingmale", plot = TRUE, rug = TRUE, main = "Partial Dependence of SwingMale on SwingRep", ylab = "SwingRep")
pdp_swingwhite <- partial(voting_model, pred.var = "swingwhite", plot = TRUE, rug = TRUE, main = "Partial Dependence of SwingWhite on SwingRep", ylab = "SwingRep")
pdp_swingturnout <- partial(voting_model, pred.var = "swingturnout", plot = TRUE, rug = TRUE, main = "Partial Dependence of SwingTurnout on SwingRep", ylab = "SwingRep")
pdp_swingincome <- partial(voting_model, pred.var = "swingincome", plot = TRUE, rug = TRUE, main = "Partial Dependence of SwingIncome on SwingRep", ylab = "SwingRep")
pdp_unemp <- partial(voting_model, pred.var = "unemp", plot = TRUE, rug = TRUE, main = "Partial Dependence of Unemployment on SwingRep", ylab = "SwingRep")
pdp_swingeduc <- partial(voting_model, pred.var = "swingeduc", plot = TRUE, rug = TRUE, main = "Partial Dependence of SwingEduc on SwingRep", ylab = "SwingRep")
pdp_urbanrural <- partial(voting_model, pred.var = "urbanrural", plot = TRUE, rug = TRUE, main = "Partial Dependence of UrbanRural on SwingRep", ylab = "SwingRep")

# Combine all partial dependence plots into one plot
combined_pdp <- grid.arrange(pdp_swingages_20_29, pdp_swingmale, pdp_swingwhite, pdp_swingturnout, pdp_swingincome, pdp_unemp, pdp_swingeduc, pdp_urbanrural, ncol = 2, top = textGrob("Partial Dependence Plots", gp = gpar(fontsize = 20, fontface = "bold")))

# Save the combined plot as an image
ggsave("/Users/Desktop/partial_dependence_plots.png", plot = combined_pdp, width = 15, height = 15)


######################################################################################################
#Interaction Plots:
#Interaction effects, e.g., how the relationship between swingages_20_29 and swingrep, swingmale and swingrep, swingwhite and swingrep, swingturnout and swingrep, swingincome and swingrep, unemp and swingrep, swingeduc and swingrep, urbanrural and swingrep changes by urbanrural.
#Interaction effects, e.g., how the relationship between swingturnout and urbanrural changes swingrep
######################################################################################################
# Interaction effect between swingturnout and urbanrural on swingrep
interaction_model <- lm(swingrep ~ swingturnout * urbanrural, data = final_data)

# Summarize the interaction model
summary(interaction_model)

# Visualize the interaction effect
interaction_plot <- ggplot(final_data, aes(x = swingturnout, y = swingrep, color = factor(urbanrural))) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Interaction Effect of Voter Turnout Change and Urban/Rural on Republican Swing",
       x = "SwingTurnout",
       y = "SwingRep",
       color = "UrbanRural") +
  theme_minimal() +
  theme(
                        plot.title = element_text(size = 15, face = "bold"),
                        axis.title = element_text(size = 3),
                        axis.text = element_text(size = 18),
                        legend.title = element_text(size = 20),
                        legend.text = element_text(size = 18),
                        strip.text = element_text(size = 20)  )

interaction_plot

# Save the interaction plot as an image
ggsave("/Users/Desktop/interaction_swingturnout_urbanrural.png", plot = interaction_plot, width = 10, height = 8)


############################################
# AGE GROUPS CHECKING
############################################

voting_model <- lm(swingrep ~ swingages_20_29 + swingages_30_44 + swingages_45_64 + swingages_65_plus + swingmale + swingwhite + swingturnout + unemp + urbanrural + swingincome + swingeduc, data = final_data)

# Summarize the model
summary(voting_model)

# Check for multicollinearity using Variance Inflation Factor (VIF)
vif(voting_model) #shows error
#Error in vif.default(voting_model) : 
#  there are aliased coefficients in the model

#but if we only keep one age variable:.groups
vifcheck20_29 <- lm(swingrep ~  swingages_30_44 + swingages_45_64 + swingages_65_plus + swingmale + swingwhite + swingturnout + unemp + urbanrural + swingincome + swingeduc, data = final_data)
vif(vifcheck20_29) #works
#summary(vifcheck20_29)

# ages 20-29 taken out
#  swingages_30_44   swingages_45_64 swingages_65_plus         swingmale 
#         2.572298          2.492427          2.617368          1.406951 
#       swingwhite      swingturnout             unemp        urbanrural 
#         1.113333          1.066353          1.213648          1.116831 
#      swingincome         swingeduc 
#         1.235219          1.019751 

vifcheck30_44 <- lm(swingrep ~  swingages_20_29 + swingages_45_64 + swingages_65_plus + swingmale + swingwhite + swingturnout + unemp + urbanrural + swingincome + swingeduc, data = final_data)
vif(vifcheck30_44)

# ages 30-44 taken out
#  swingages_20_29   swingages_45_64 swingages_65_plus         swingmale 
#         1.501453          1.843548          1.999339          1.406951 
#       swingwhite      swingturnout             unemp        urbanrural 
#         1.113333          1.066353          1.213648          1.116831 
#      swingincome         swingeduc 
#         1.235219          1.019751 

vifcheck45_64 <- lm(swingrep ~  swingages_20_29 + swingages_30_44 + swingages_65_plus + swingmale + swingwhite + swingturnout + unemp + urbanrural + swingincome + swingeduc, data = final_data)
vif(vifcheck45_64)

# ages 45-64 taken out
#  swingages_20_29   swingages_30_44 swingages_65_plus         swingmale 
#         1.403419          1.778398          1.617594          1.406951 
#       swingwhite      swingturnout             unemp        urbanrural 
#         1.113333          1.066353          1.213648          1.116831 
#      swingincome         swingeduc 
#         1.235219          1.019751 

vifcheck65_plus <- lm(swingrep ~  swingages_20_29 + swingages_30_44 + swingages_45_64 + swingmale + swingwhite + swingturnout + unemp + urbanrural + swingincome + swingeduc, data = final_data)
vif(vifcheck65_plus)

# ages 65+ taken out
#swingages_20_29 swingages_30_44 swingages_45_64       swingmale      swingwhite 
#       1.635809        2.140740        1.795446        1.406951        1.113333 
#   swingturnout           unemp      urbanrural     swingincome       swingeduc 
#       1.066353        1.213648        1.116831        1.235219        1.019751 

# age 20-29 affects the others most when taken out -> important indicator:
# explore this further through lasso

# Load the glmnet package for Lasso regression
library(glmnet)

# Prepare the data for Lasso regression
x <- model.matrix(swingrep ~ swingages_20_29 + swingages_30_44 + swingages_45_64 + swingages_65_plus + swingmale + swingwhite + swingturnout + unemp + urbanrural + swingincome + swingeduc, data = final_data)[, -1]
y <- final_data$swingrep

# Fit the Lasso regression model
lasso_model <- cv.glmnet(x, y, alpha = 1)

# Print the best lambda value
cat("Best lambda value: ", lasso_model$lambda.min, "\n")

# Coefficients of the Lasso model
lasso_coefficients <- coef(lasso_model, s = "lambda.min")
print(lasso_coefficients) 
#(Intercept)        0.03416196
#swingages_20_29    1.02708219 -> most important and tells us most about swingreps variance
#swingages_30_44   -0.12342338
#swingages_45_64    0.35346531
#swingages_65_plus  .         

#keep 20-29 age group and remove the others

# Plot the cross-validation results
plot(lasso_model)

############################################################################################################
#END OF CODE
############################################################################################################