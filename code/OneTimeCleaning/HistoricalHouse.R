library(tidyverse)
library(readxl)
library(janitor)

senate_results <- read.csv("data/1976-2020-senate.csv")
house_uncleaned <- read.csv("data/HistoricalElections/HouseHistory.csv")
  

#Getting a dataframe that takes every major candidate to their party
reps_to_party <- house_uncleaned %>%
  filter(year >= 2002 & stage == "GEN" & (!runoff | is.na(runoff))
         & !special & state_po != "DC") %>%
  select(c("candidate", "party", "district", "year", "state_po")) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  distinct()

#Calculating the total votes for every race
total_votes <- house_uncleaned %>%
  filter(year >= 2002 & stage == "GEN" & (!runoff | is.na(runoff))
         & !special & state_po != "DC") %>% #only choosing important races
  select(c("year", "state_po", "district", "totalvotes")) %>%
  unique() %>%
  mutate(totalvotes = case_when(
    totalvotes < 30000 ~ NA_integer_, 
    TRUE ~ totalvotes
  )) %>% #for uncontested races, totalvotes < 30k
  group_by(year, state_po) %>% #replacing uncontested races with the mean of the other races
  mutate(mean_totalvotes = as.integer(round(mean(totalvotes, na.rm = TRUE)))) %>%
  mutate(totalvotes = if_else(is.na(totalvotes), mean_totalvotes, totalvotes)) %>%
  select(-"mean_totalvotes") %>% 
  group_by(year, state_po, district) %>%
  summarize(totalvotes = max(totalvotes)) #fixes problems with Alabama's data, sometimes same race has different totalvotes
  

#Getting results from every past house election!
house_cleaned <- house_uncleaned %>%
  filter(year >= 2002 & stage == "GEN" & (!runoff | is.na(runoff))
         & !special & state_po != "DC") %>% # taking only important races
  select(c("year", "state_po", "district", "candidate", "party", "writein", "candidatevotes")) %>%
  group_by(year, state_po, district, candidate) %>%
  #getting sum of votes for each candidate
  summarize(sum_candidate_votes = sum(candidatevotes)) %>%
  #only care abt in candidates that ALSO ran under the dem/rep ticket that year
  right_join(reps_to_party, by = c("state_po" = "state_po", 
                                   "year" = "year", 
                                   "district" = "district",
                                   "candidate" = "candidate")) %>% 
  #unopposed candidates should be removed
  mutate(sum_candidate_votes = case_when(
    sum_candidate_votes <= 1 ~ NA_integer_, 
    TRUE ~ sum_candidate_votes
  )) %>%
  group_by(year, state_po, district, party) %>%
  summarize(party_votes = sum(sum_candidate_votes)) %>%
  pivot_wider(names_from = party, values_from = party_votes) %>%
  #bringing in previous total_votes calculations
  left_join(total_votes, by = c("year" = "year", "state_po" = "state_po", 
                                 "district" = "district")) %>%
  #calculating margins and whether candidates were unopposed
  mutate(dem_pct = 100 * DEMOCRAT/totalvotes,
         rep_pct = 100 * REPUBLICAN/totalvotes,
         margin = dem_pct - rep_pct,
         rep_unopposed = is.na(DEMOCRAT),
         dem_unopposed = is.na(REPUBLICAN)) %>%
  select(c("year", "state_po", "district", "margin", "rep_unopposed", "dem_unopposed")) %>%
  unique() %>%
  #we only care about "relatively" competitive, opposed races
  filter(!(dem_unopposed | rep_unopposed) & margin < 95 & margin > -95) %>% #when margins are this high, races are universally ACTUALLY unopposed
  select(-c("dem_unopposed", "rep_unopposed")) 


write.csv(house_cleaned, "cleaned_data/HouseHistorical.csv")


