library(tidyverse)
library(readxl)

load("data/HistoricalElections/electoral_studies_replication_presidential_analysis_data.Rdata")

pres_uncleaned <- electoral_studies_presidential
rm(electoral_studies_presidential)

#This dataset gets the total number of votes for each party for each race
#I stop cleaning halfway through because it is used for both PVI and Pres stats
pres_votes <- pres_uncleaned %>%
  group_by(election_year, state) %>%
  #organized by county, must sum county votes to get total votes
  summarize(
    dem_votes = sum(democratic_raw_votes), 
    rep_votes = sum(republican_raw_votes), 
    total_votes = sum(raw_county_vote_totals)
  ) %>%
  filter(election_year >= 1996) 

#finishing presidential analysis
pres_finished <- pres_votes %>%
  mutate(margin = 100 * (dem_votes - rep_votes) / total_votes) %>%
  filter(election_year >= 2004) %>%
  select(c('election_year', 'state', 'margin')) %>%
  rename(year = election_year)

write.csv(pres_finished, "cleaned_data/PresidentHistorical.csv")

#---- Working on PVI Values -----#
#summary of each race -- two party-democratic results since 2000, and lagged values
pres_summary <- read.csv("data/HistoricalElections/President Summary.csv") %>%
  mutate(natl_dem_tp = 100 * DEMOCRAT / (DEMOCRAT + REPUBLICAN), ) %>%
  #national lagged democratic two-party pct -- needed for PVI
  mutate(lagged_natl_dem_tp = lag(natl_dem_tp, order_by = year)) %>%
  select(-c("DEMOCRAT", "REPUBLICAN")) %>%
  filter(year > 1996)

#Calculating state PVI values
state_pvi <- pres_votes %>%
  mutate(dem_tp_state = 100 * dem_votes/(dem_votes + rep_votes)) %>%
  group_by(state) %>%
  mutate(lagged_dem_tp_state = lag(dem_tp_state, order_by = election_year),
         election_year = as.numeric(election_year)) %>%
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

PVI_path <- "../data/PVI.xlsx"

#Getting PVI data, which is kept in different excel sheets
PVI_list <- PVI_path %>%
  excel_sheets() %>%
  map(~ read_excel(PVI_path, sheet = .)) %>%
  magrittr::extract(2:length(.)) #First excel sheet is an explanation

#For some reason, Cook changed District to Number in the final dataset
PVI_list[[15]]$District <- PVI_list[[15]]$Number

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


total_president_margin <- read.csv("../data/HistoricalElections/President Summary - Sheet1.csv")

PVI_state <- cleaned_pres %>%
  group_by(state_po) %>%
  mutate(lagged_state_margin = lag(margin, order_by = state_po)) %>%
  filter(year >= 2000) %>%
  full_join(total_president_margin, by = c("year" = "year")) %>%
  rename(state_margin = margin) %>%
  select(-c("DEMOCRAT", "REPUBLICAN")) %>%
  mutate(Raw_PVI = 0.5*(0.75*(state_margin - national_margin) + 
                          0.25 * (lagged_state_margin - lagged_national_margin))) %>%
  select(c("year", "state_po", "Raw_PVI")) %>%
  rowwise() %>%
  mutate(True_Year = list(c(year + 2, year + 4)), .after = year, 
         district = "0") %>%
  unnest(True_Year) %>%
  select(-c("year")) %>%
  rename(Abbreviation = state_po, 
         District = district)

states_list <- read.csv("../cleaned_data/StatesList.csv")


PVI_full <- PVI_district %>%
  full_join(states_list, by = c("State" = "State")) %>%
  select("Abbreviation", "District", "True_Year", "Raw_PVI") %>%
  bind_rows(PVI_state)

write.csv(PVI_full, "../cleaned_data/Completed_PVI.csv")




