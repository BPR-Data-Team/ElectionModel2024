library(tidyverse)
library(readxl)
library(janitor)

#Most of this is manually done, but I had to get the incumbency differential
#Figured out. Key problem: Many senators last ran in a special election 
#At the same time as other senators, so Logan/I had to manually go in and write 
#Those down. Didn't take that long, but now need to do extra analysis

senate_uncleaned <- read.csv("data/HistoricalElections/SenateHistory.csv") %>%
  clean_names() %>%
  mutate(uncontested = ifelse(state == "Vermont", FALSE, uncontested))

pvi <- read.csv("cleaned_data/Completed PVI.csv")
generic_ballot <- read.csv("cleaned_data/Generic Ballot.csv")

current_senate <- read.csv("data/AllRaces.csv") %>%
  filter(Office_type == "Senate" & Weird == "") %>%
  mutate(special_election = str_detect(State, " Special"), 
         State = str_remove(State, " Special"),
         open_seat = !Incumbent, 
         year = 2024) %>%
  rename(state = State) %>%
  select(year, state, open_seat, special_election)

cleaned_senate <- senate_uncleaned %>%
  filter(!uncontested & (democratic_total > 1 & republican_total > 1 & independent_total < 20)) %>%
  bind_rows(current_senate) %>%
  mutate(district = 0,
         incumbent_special = ifelse(is.na(incumbent_year), FALSE, TRUE),
         incumbent_year = ifelse(is.na(incumbent_year), year - 6, incumbent_year), 
         state = state.abb[match(state, state.name)], 
         pvi_year = ifelse(year %% 2 == 0, year, year - 1), 
         margin = as.numeric(margin)) %>%
  left_join(pvi, by = c('state', 'pvi_year' = 'year', 'district')) %>%
  select(c(state, year, district, special_election, republican_total, 
           democratic_total, margin, open_seat, incumbent_year, incumbent_special, 
           pvi))

#Tough code here: joining cleaned senate with itself, but at the incumbency point
#So if an incumbent last ran in 2008, I join the current year (2014) with data
#From 2008
senate_with_incumbency <- cleaned_senate %>%
  left_join(cleaned_senate, by = c('state', 'incumbent_year' = 'year',
                                   'incumbent_special' = 'special_election', 'district')) %>%
  rename_with(.fn = ~ str_replace(., "\\.y", "_past"), .cols = matches("\\.y")) %>%
  rename_with(.fn = ~ str_remove(., "\\.x"), .cols = matches("\\.x")) %>%
  left_join(generic_ballot, by = c('incumbent_year' = 'year')) %>%
  #Because we matched genballot to incumbent year, we actually 
  #Choose the current genballot rather than past (since past would be too far back)
  mutate(incumbent_differential = ifelse(open_seat, NA_real_, 
                      (margin_past - gen_margin) - 2*pvi_past)) %>%
  select(c(state, year, district, special_election, open_seat,
           incumbent_differential, margin)) %>%
  filter(year == 2024 | (!is.na(margin) & !(state == "GA" & special_election))) %>% 
  filter(year %% 2 == 0 & !(state == "CA" & special_election & year == 2022)) %>%
  filter(!(state == "IL" & special_election & year == 2010) & state != "LA")


write.csv(senate_with_incumbency, "cleaned_data/AllSenate.csv")              
