library(tidyverse)
library(readxl)

load("data/HistoricalElections/dataverse_shareable_presidential_county_returns_1868_2020.Rdata")

pres_uncleaned <- pres_elections_release
rm(pres_elections_release) #Clearing memory -- dataset is large
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
  #doesn't include alaska (doesn't report by county), so manually added it
  bind_rows(alaska_results) %>%
  rename(year = election_year)

#---- Calculating state PVI values -- necessary for incumbent margins
#summary of each race -- two party-democratic results since 2000, and lagged values
pres_summary <- read.csv("data/HistoricalElections/President Summary.csv") %>%
  mutate(natl_dem_tp = 100 * DEMOCRAT/ (DEMOCRAT + REPUBLICAN)) %>%
  #national lagged democratic two-party pct -- needed for PVI
  mutate(lagged_natl_dem_tp = lag(natl_dem_tp, order_by = year)) %>%
  select(-c("DEMOCRAT", "REPUBLICAN")) 

state_pvi <- pres_votes %>%
  mutate(dem_tp_state = 100 * dem_votes/(dem_votes + rep_votes)) %>%
  group_by(state) %>%
  mutate(election_year = as.numeric(year), 
         lagged_dem_tp_state = lag(dem_tp_state, order_by = year)) %>%
  full_join(pres_summary, by = "year") %>%
  #formula for PVI
  mutate(pvi = 0.75 * (dem_tp_state - natl_dem_tp) + 
           0.25 * (lagged_dem_tp_state - lagged_natl_dem_tp)) %>%
  rowwise() %>%
  mutate(year = list(c(year + 2, year + 4))) %>%
  unnest(year) %>%
  select(c('year', 'state', 'pvi')) %>%
  #all state values should have district values of 0 (at large)
  mutate(district = 0, .before = pvi)

#Getting incumbency values for this current election
current_pres <- read.csv("data/2024President.csv") %>%
  select(year, state)

#finishing presidential analysis
pres_finished <- pres_votes %>%
  bind_rows(current_pres) %>%
  mutate(margin = 100 * (dem_votes - rep_votes) / total_votes, 
         dem_tp = 100 * dem_votes/ (dem_votes + rep_votes),
         district = 0) %>%
  select(c('year', 'state', 'district', 'margin', 'dem_tp')) %>%
  mutate(open_seat = year %in% c(2008, 2016)) %>%
  left_join(state_pvi, by = c('year', 'state', 'district')) %>%
  left_join(pres_summary, by = 'year') %>%
  group_by(state) %>%
  #Getting past results for incumbency data
  mutate(past_dem_tp = lag(dem_tp, 1, order_by = year), 
         past_pvi = lag(pvi, 1, order_by = year), 
         past_gen_dem_tp = lag(natl_dem_tp, 1, order_by = year)) %>%
  mutate(incumbent_differential = ifelse(open_seat, NA_real_, 
          (past_dem_tp - past_gen_dem_tp) - past_pvi)) %>%
  select(c('year', 'state', 'district', 'open_seat', 'incumbent_differential', 'margin')) %>%
  filter(year >= 2004)
  
write.csv(pres_finished, "cleaned_data/AllPresident.csv")

#---- Working on PVI District Values -----#

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
#1999 (elections for 1992, 1996) -> 1998, 2000

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
  filter(Year %in% c(1999, 2003, 2007, 2009, 2012, 2015, 2019, 2023)) %>%
  rowwise() %>%
  mutate(True_Year = list(case_when(
    Year == 2023 ~ c(2022, 2024),
    Year == 2019 ~ c(2018, 2020), 
    Year == 2015 ~ c(2014, 2016), 
    Year == 2012 ~ 2012, 
    Year == 2009 ~ 2010, 
    Year == 2007 ~ c(2006, 2008), 
    Year == 2003 ~ c(2002, 2004),
    Year == 1999 ~ c(1998, 2000),
    TRUE ~ Year
  )), .after = Year) %>%
  unnest(True_Year) %>%
  unique() %>%
  filter(!(
    (True_Year == 2020 & State == "North Carolina") |
      (True_Year == 2016 & State %in% c("North Carolina", "Florida", "Virginia"))
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




