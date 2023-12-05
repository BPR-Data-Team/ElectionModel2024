library(tidyverse)

load("~/Documents/Political Projections/ElectionModel2024/data/HistoricalElections/dataverse_shareable_gubernatorial_county_returns_1865_2020.Rdata")

governor_uncleaned <- gov_elections_release
rm(gov_elections_release)

governor_cleaned <- governor_uncleaned %>%
  group_by(election_year, state, seat_status) %>%
  filter(!grepl("Uncontested", seat_status)) %>%
  summarize(dem_votes = sum(democratic_raw_votes, na.rm = TRUE), 
            rep_votes = sum(republican_raw_votes, na.rm = TRUE), 
            total_votes = sum(raw_county_vote_totals, na.rm = TRUE)) %>%
  filter(dem_votes + rep_votes >= total_votes*0.75) %>%
  mutate(open_seat = (grepl("Open Seat", seat_status)),
         margin = 100 * (dem_votes - rep_votes) / total_votes, .keep = "unused") %>%
  group_by(state) %>%
  mutate(incumbent_margin = case_when(
    open_seat ~ NA_real_,
    TRUE ~ lag(margin, order_by = election_year)
  )) %>%
  filter(election_year >= 2002) %>%
  rename(year = election_year)

write.csv(governor_cleaned, "cleaned_data/GovernorHistorical.csv")