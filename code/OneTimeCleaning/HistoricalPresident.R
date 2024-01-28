library(tidyverse)
library(readxl)

load("data/HistoricalElections/dataverse_shareable_presidential_county_returns_1868_2020.Rdata")

pres_uncleaned <- pres_elections_release
rm(pres_elections_release)
alaska_results <- read.csv('data/HistoricalElections/AKPres.csv')

#This dataset gets the total number of votes for each party for each race
#I stop cleaning halfway through because it is used for both PVI and Pres stats
pres_votes <- pres_uncleaned %>%
  group_by(election_year, state) %>%
  #organized by county, must sum county votes to get total votes
  summarize(
    dem_votes = sum(democratic_raw_votes, na.rm = TRUE), 
    rep_votes = sum(republican_raw_votes, na.rm = TRUE), 
    total_votes = sum(raw_county_vote_totals)
  ) %>%
  filter(election_year >= 1996) %>%
  #doesn't include alaska (doesn't report by county), so manually added it
  bind_rows(alaska_results)


#finishing presidential analysis
pres_finished <- pres_votes %>%
  mutate(margin = 100 * (dem_votes - rep_votes) / total_votes) %>%
  select(c('election_year', 'state', 'margin')) %>%
  rename(year = election_year) %>%
  mutate(open_seat = year %in% c(2004, 2012, 2020)) %>%
  group_by(state) %>%
  mutate(incumbent_margin = case_when(
    open_seat ~ NA_real_, 
    TRUE ~ lag(margin, 1, order_by = year)
  )) %>%
  filter(year >= 2004)
  

write.csv(pres_finished, "cleaned_data/President Historical.csv")

#---- Working on PVI Values -----#
#summary of each race -- two party-democratic results since 2000, and lagged values
pres_summary <- read.csv("data/HistoricalElections/President Summary.csv") %>%
  mutate(natl_dem_tp = 100 * DEMOCRAT / (DEMOCRAT + REPUBLICAN)) %>%
  #national lagged democratic two-party pct -- needed for PVI
  mutate(lagged_natl_dem_tp = lag(natl_dem_tp, order_by = year)) %>%
  select(-c("DEMOCRAT", "REPUBLICAN")) %>%
  filter(year > 1996)

#Calculating state PVI values
state_pvi <- pres_votes %>%
  mutate(dem_tp_state = 100 * dem_votes/(dem_votes + rep_votes)) %>%
  group_by(state) %>%
  mutate(election_year = as.numeric(election_year), 
         lagged_dem_tp_state = lag(dem_tp_state, order_by = election_year)) %>%
  filter(election_year > 1996) %>%
  full_join(pres_summary, by = c("election_year" = "year")) %>%
  #formula for PVI
  mutate(pvi = 0.75 * (dem_tp_state - natl_dem_tp) + 
           0.25 * (lagged_dem_tp_state - lagged_natl_dem_tp)) %>%
  rowwise() %>%
  mutate(year = list(c(election_year + 2, election_year + 4))) %>%
  unnest(year) %>%
  select(c('year', 'state', 'pvi')) %>%
  #all state values should have district values of 0 (at large)
  mutate(district = 0, .before = pvi)

#Working with district PVI values
#Note: This data has some problems -- specifically, I input NA for 2020 NC and 
#2016 FL, NC, and VA because the necessary data (post-redistricting, pre-election)
#is unavailable

PVI_path <- "data/PVI.xlsx"

#Getting PVI data, which is kept in different excel sheets
PVI_list <- PVI_path %>%
  excel_sheets() %>%
  map(~ read_excel(PVI_path, sheet = .)) %>%
  magrittr::extract(2:length(.)) #First excel sheet is an explanation

#For some reason, Cook changed District to Number in the final dataset
PVI_list[[15]]$District <- PVI_list[[15]]$Number

#Districts we want from the PVI website:
#2023 (elections for 2016 and 2020, post-redistricting) -> 2022, 2024
#2019 (elections for 2012 and 2016) -> 2018, 2020
#2015 (elections for 2008, 2012) -> 2014, 2016
#2012 (elections for 2004, 2008, post-redistricting) -> 2012
#2009 (elections for 2004 and 2008, pre-redistricting) -> 2010
#2007 (elections for 2000 and 2004) -> 2006, 2008
#2003 (elections for 1996, 2000, post-redistricting) -> 2002, 2004

PVI_district <- Reduce(function(x, y) full_join(x, y, by=c("State","District")), PVI_list) %>%
  #Replacing D+12 with 12, R+12 with -12, and EVEN with 0
  mutate(across(contains("Cook"), ~case_when(
    grepl("D", .) ~ gsub("D\\+", "", .),
    grepl("R", .) ~ gsub("R\\+","-", .),
    grepl("EVEN", .) ~ "0",
    TRUE ~ NA_character_
  ))) %>%
  mutate(across(contains("Cook"), as.numeric)) %>%
  #Pivoting dataset to make it look like it should
  select(c("State", "District") | contains("Raw Cook PVI")) %>%
  pivot_longer(contains("Cook"), names_to = "Year", values_to = "Raw_PVI") %>%
  mutate(District = str_remove(District, "\\.0"),
         Year = as.numeric(str_remove(Year, " Raw Cook PVI"))) %>%
  #Using previously-explained years to get the exact data/year combos I want
  filter(Year %in% c(2003, 2007, 2009, 2012, 2015, 2019, 2023)) %>%
  rowwise() %>%
  mutate(True_Year = list(case_when(
    Year == 2023 ~ c(2022, 2024),
    Year == 2019 ~ c(2018, 2020), 
    Year == 2015 ~ c(2014, 2016), 
    Year == 2012 ~ 2012, 
    Year == 2009 ~ 2010, 
    Year == 2007 ~ c(2006, 2008), 
    Year == 2003 ~ c(2002, 2004), 
    TRUE ~ Year
  )), .after = Year) %>%
  unnest(True_Year) %>%
  unique() %>%
  filter(!(
    (Year == 2020 & State == "North Carolina") |
      (Year == 2016 & State %in% c("North Carolina", "Florida", "Virginia"))
  )) %>%
  select(-c("Year"))

states_list <- read.csv("cleaned_data/StatesList.csv")


PVI_full <- PVI_district %>%
  full_join(states_list, by = c("State" = "State")) %>%
  select("Abbreviation", "District", "True_Year", "Raw_PVI") %>%
  rename(
    year = True_Year, 
    pvi = Raw_PVI, 
    district = District, 
    state = Abbreviation
  ) %>%
  #At-Large Districts count as district #1
  mutate(district = ifelse(district == "AL", 1, district), 
         district = as.numeric(district)) %>%
  bind_rows(state_pvi) %>%
  filter(!is.na(pvi))

write.csv(PVI_full, "cleaned_data/Completed PVI.csv")




