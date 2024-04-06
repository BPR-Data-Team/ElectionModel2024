library(tidyverse)

load("data/HistoricalElections/dataverse_shareable_gubernatorial_county_returns_1865_2020.Rdata")

governor_uncleaned <- gov_elections_release
rm(gov_elections_release) #Clearing memory

current_governor <- read.csv("data/2024Governor.csv") %>%
  mutate(State = str_remove(State, " Special"),
         open_seat = !Incumbent, 
         year = 2024, 
         State = state.abb[match(State, state.name)]) %>%
  rename(state = State) %>%
  select(year, state, open_seat)

generic_ballot <- read.csv("cleaned_data/Generic Ballot.csv")

#getting PVI data for incumbent differential
state_pvi <- read.csv("cleaned_data/Completed PVI.csv") %>%
  filter(district == 0) %>%
  select(-c('X', 'district'))
  
governor_cleaned <- governor_uncleaned %>%
  group_by(election_year, state, seat_status) %>%
  filter(!grepl("Uncontested", seat_status)) %>% #Don't want uncontested
  summarize(dem_votes = sum(democratic_raw_votes, na.rm = TRUE), 
            rep_votes = sum(republican_raw_votes, na.rm = TRUE), 
            total_votes = sum(raw_county_vote_totals, na.rm = TRUE)) %>%
  filter(dem_votes + rep_votes >= total_votes*0.75) %>%
  rename(year = election_year) %>%
  #If gov ran in 2003, we're saying they ran in 2004
  mutate(open_seat = (grepl("Open Seat", seat_status)),
         margin = 100 * (dem_votes - rep_votes) / total_votes,
         dem_tp = dem_votes / (dem_votes + rep_votes),
         year = ifelse(year %% 2 == 0, year, year + 1)) %>% 
  left_join(state_pvi, by = c('year', 'state')) %>%
  left_join(generic_ballot, by = c('year')) %>%
  bind_rows(current_governor) %>%
  group_by(state) %>%
  #Incumbent differential!
  mutate(prev_dem_tp = lag(dem_tp, 1, order_by = year),
         prev_dem_gen_tp = lag(gen_dem_tp, 1, order_by = year),
         prev_pvi = lag(pvi, 1, order_by = year)) %>%
  mutate(incumbent_differential = ifelse(open_seat, NA_real_,
              100*(prev_dem_tp - prev_dem_gen_tp) - prev_pvi)) %>%
  #Weird stuff in 2001 and 2020 WV elections
  filter(year >= 2002 &
           !(state == "WV" & year == 2012) &
           !(state == "WV" & year == 2020)) %>%
  select(c('year', 'state', 'open_seat', 'margin', 'incumbent_differential'))

write.csv(governor_cleaned, "cleaned_data/AllGovernor.csv")
