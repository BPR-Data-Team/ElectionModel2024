library(tidyverse)
library(readxl)
library(janitor)
#DO INCUMBENT MARGIN OVER PVI, NOT JUST INCUMBENT MARGIN

house_uncleaned <- read.csv("data/HistoricalElections/HouseHistory.csv")
pvi_full <- read.csv("cleaned_data/Completed PVI.csv") %>%
  select(-X)
  
#Getting a dataframe that takes every major candidate to their party
reps_to_party <- house_uncleaned %>%
  filter(year >= 1998 & stage == "GEN" & (!runoff | is.na(runoff))
         & !special & state_po != "DC") %>%
  select(c("candidate", "party", "district", "year", "state_po")) %>%
  #minnesota has DFL instead of Democratic party, ND has DNL
  mutate(party = case_when(
    party == "DEMOCRATIC-FARMER-LABOR" ~ "DEMOCRAT",
    party == "DEMOCRATIC-NONPARTISAN LEAGUE" ~ "DEMOCRAT",
    TRUE ~ party
  )) %>%
  filter(party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  distinct()

#Calculating the total votes for every race
total_votes <- house_uncleaned %>%
  filter(year >= 1998 & stage == "GEN" & (!runoff | is.na(runoff))
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
  filter(year >= 1998 & stage == "GEN" & (!runoff | is.na(runoff))
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
  ))

#this dataset is just to determine which races have incumbents
incumbency <- house_cleaned %>%
  select(-"sum_candidate_votes") %>%
  group_by(year, state_po, district, party) %>%
  summarize(candidate = ifelse(n() > 1, "MULTIPLE", candidate)) %>%
  pivot_wider(names_from = party, values_from = candidate) %>%
  group_by(state_po, district) %>%
  mutate(prev_DEMOCRAT = lag(DEMOCRAT, 1, order_by = year), 
         prev_REPUBLICAN = lag(REPUBLICAN, 1, order_by = year)) %>%
  #a race is only open if neither candidate was in the prev election, AND
  #if the state does not have a jungle primary
  mutate(open_seat = ifelse(
    ((!is.na(DEMOCRAT) & !is.na(prev_DEMOCRAT) & DEMOCRAT == prev_DEMOCRAT) |
      (!is.na(REPUBLICAN) & !is.na(prev_REPUBLICAN) & REPUBLICAN == prev_REPUBLICAN)) &
      state_po != "LA", 
    FALSE, TRUE
  ))

house_finished <- house_cleaned %>%
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
         dem_tp = 100 * (dem_pct - rep_pct) / (dem_pct + rep_pct),
         rep_unopposed = is.na(DEMOCRAT),
         dem_unopposed = is.na(REPUBLICAN))

house_including_unopposed <- house_finished

house_finished <- house_finished %>%
  select(c("year", "state_po", "district", "margin", "dem_tp", "rep_unopposed", "dem_unopposed")) %>%
  unique() %>%
  #we only care about "relatively" competitive, opposed races
  filter(!(dem_unopposed | rep_unopposed) & margin < 95 & margin > -95) %>% #when margins are this high, races are universally ACTUALLY unopposed
  select(-c("dem_unopposed", "rep_unopposed")) %>%
  left_join(pvi_full, by = c('year' = 'year', 'state_po' = 'state', 
                             'district' = 'district')) %>%
  #now working with incumbency 
  group_by(state_po, district) %>%
  mutate(past_dem_tp = lag(dem_tp, 1, order_by = year), 
         past_pvi = lag(pvi, 1, order_by = year)) %>%
  filter(year >= 2002) %>%
  #rep_to_race contains information on which candidates are incumbents
  left_join(incumbency, by = c("year" = "year", "state_po" = "state_po", 
                                "district" = "district")) %>%
  mutate(incumbent_differential = case_when(
    open_seat ~ NA_real_, 
    TRUE ~ past_dem_tp - 2*past_pvi 
  ), district = ifelse(district == 0, 1, district)) %>%
  select(c("year", "state_po", "district", "open_seat", "margin", "incumbent_differential"))
  
# ----Calculating Generic Ballot Results for each year -----

#The average % of the vote 
#an unopposed candidate would have received, had they been opposed
unopposed_prop <- 0.71 

#getting data for all CONTESTED races
contested_party_summaries <- house_including_unopposed %>%
  filter(!(rep_unopposed | dem_unopposed)) %>%
  group_by(year) %>%
  summarize(contested_dem = sum(DEMOCRAT), 
            contested_rep = sum(REPUBLICAN), 
            total_votes = sum(totalvotes),
            mean_votes = mean(totalvotes),
            num_contested = n())

#getting data for all uncontested races
uncontested_party_summaries <- house_including_unopposed %>%
  filter(rep_unopposed | dem_unopposed) %>%
  group_by(year) %>%
  summarize(num_rep_unopposed = sum(rep_unopposed), 
            num_dem_unopposed = sum(dem_unopposed))

generic_ballot <- contested_party_summaries %>%
  full_join(uncontested_party_summaries, by = "year") %>%
  mutate(
    total_dem = contested_dem + num_dem_unopposed*mean_votes*unopposed_prop + num_rep_unopposed*mean_votes*(1-unopposed_prop), 
    total_rep = contested_rep + num_dem_unopposed*mean_votes*(1-unopposed_prop) + num_rep_unopposed*mean_votes*unopposed_prop
  ) %>% #formula that gets the total votes for dems/reps each year, assuming unopposed would get a certain % of the vote
  mutate(dem_pct = 100 * total_dem/(435 * mean_votes), 
         rep_pct = 100 * total_rep/(435 * mean_votes)) %>%
  mutate(margin = dem_pct - rep_pct) %>%
  select(c("year", "margin")) %>%
  mutate(lagged_margin = lag(margin, order_by = year), 
         year = year + 2) %>% #lagging margin
  filter(year >= 2002) %>%
  rename(prev_gb_margin = margin, 
         prev2_gb_margin = lagged_margin) #we want the year to include the GB for EACH of the last 2 elections

#---- Special Elections for all House Elections
pvi_full <- read.csv("cleaned_data/Completed PVI.csv")
specials_no_pvi <- read.csv("data/HistoricalElections/Special Election Data.csv")

specials_summary <- specials_no_pvi %>%
  left_join(pvi_full, by = c("Year" = "year", "State" = "state", "District" = "district")) %>%
  mutate(differential = 100 * (Dem.Percent - Rep.Percent) / (Dem.Percent + Rep.Percent) -
           pvi * 2) %>%
  group_by(Year) %>%
  summarize(mean_specials_differential = mean(differential)) %>%
  rename(year = Year)

write.csv(specials_summary, "cleaned_data/Specials.csv")
write.csv(house_finished, "cleaned_data/House Historical.csv")
write.csv(generic_ballot, "cleaned_data/Generic Ballot.csv")
  

