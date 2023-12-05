library(tidyverse)
library(readxl)
library(janitor)

#loads the r dataframe into the global environment, called electoral_studies_senate
load("data/HistoricalElections/dataverse_shareable_us_senate_county_returns_1908_2020.Rdata", .GlobalEnv)

senate_uncleaned <- senate_elections_release
#don't want two copies of the same dataframe, renamed
rm(senate_elections_release)

#getting non-vote related info on each race: type of incumbent, seat class, special/regular
race_facts <- senate_uncleaned %>%
  #we treat appointed senators as non-incumbents
  mutate(open_seat = ifelse(grepl("[r|R]e-election", seat_status), 0, 1), 
         election_year = as.numeric(election_year)) %>%
  select(c("election_year", "state", "open_seat", "seat_class", "election_type")) %>%
  unique()

#cleaning senate dataset
senate_finished <- senate_uncleaned %>%
  group_by(election_year, state, election_type) %>%
  #data based on counties, so have to sum by counties
  summarize(dem_votes = sum(democratic_raw_votes, na.rm = TRUE), 
            rep_votes = sum(republican_raw_votes, na.rm = TRUE), 
            total_votes = sum(raw_county_vote_totals, na.rm = TRUE)) %>%
  #getting margin for each race
  mutate(margin = 100 * (dem_votes - rep_votes) / total_votes, 
         election_year = as.numeric(election_year), 
         sig_third = (dem_votes + rep_votes) / total_votes <= 0.6)
  # full_join(race_facts, by = c("election_year", "state", "special_election")) %>%
  # #getting previous margins
  # group_by(state, seat_class) %>%
  # mutate(prev_margin = lag(margin, 1, order_by = election_year)) %>%
  # ungroup() %>%
  # filter(election_year >= 2002) %>%
  # #we only care about previous margins if the senator is an incumbent
  # mutate(open_seat = case_when(
  #   dem_incumbency == 0 ~ 1,
  #   TRUE ~ 0), 
  #   prev_margin = case_when(
  #     open_seat == 1 ~ NA_real_, 
  #     TRUE ~ prev_margin
  #   )) %>%
  # select(c("election_year", "state", "open_seat", "margin", "prev_margin"))

write.csv(senate_finished, "cleaned_data/SenateHistorical.csv")
