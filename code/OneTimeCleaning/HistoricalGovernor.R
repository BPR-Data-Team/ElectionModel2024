library(tidyverse)

load("~/Documents/Political Projections/ElectionModel2024/data/HistoricalElections/electoral_studies_replication_gov_analysis_data.Rdata")

governor_uncleaned <- electoral_studies_gov
rm(electoral_studies_gov)

governor_cleaned <- governor_uncleaned %>%
  group_by(election_year, state, seat_status) %>%
  summarize(dem_votes = sum(democratic_raw_votes), 
            rep_votes = sum(republican_raw_votes), 
            total_votes = sum(raw_county_vote_totals)) %>%
  mutate(open_seat = (seat_status == "Open Seat"), 
         margin = 100 * (dem_votes - rep_votes) / total_votes, .keep = "unused") %>%
  group_by(state) %>%
  mutate(lagged_margin = case_when(
    open_seat ~ NA_real_, 
    TRUE ~ lag(margin, order_by = election_year)
  )) %>%
  filter(election_year >= 2002) %>%
  rename(year = election_year)

write.csv(governor_cleaned, "cleaned_data/GovernorHistorical.csv")