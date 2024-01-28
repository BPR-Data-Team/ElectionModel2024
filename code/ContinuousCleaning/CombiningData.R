library(tidyverse)

#--- Downloading Datasets ------
#Past Elections
house_historical <- read_csv("cleaned_data/House Historical.csv") %>%
  mutate(office_type = "House") %>%
  rename(state = state_po)

#Cleaning Senate Elections, which were collected fully manually 
sen_historical <- read.csv("cleaned_data/Senate Historical.csv") %>%
  rename(
    state = State, 
    year = Year, 
    margin = Margin, 
    incumbent_margin = Incumbent.Margin, 
    open_seat = Open.Seat., 
    special = Special.Election.
  ) %>%
  #Connecticut 2006 had an independent win -- we don't include that election
  filter(Republican.Total.. != 0 & Democratic.Total.. != 0 &
           !(state == "Connecticut" & year == 2006)) %>%
  mutate(office_type = "Senate", 
         district = 0, 
         margin = as.numeric(margin)) %>%
  select(state, year, office_type, district, margin, open_seat, 
         incumbent_margin, special) %>%
  #State abbreviations are used here, so we match by state
  mutate(state = state.abb[match(state, state.name)])

pres_historical <- read_csv("cleaned_data/President Historical.csv") %>%
  mutate(office_type = "President", 
         district = 0)

#Combining all previous elections
elections_historical <- bind_rows(
  house_historical, pres_historical, sen_historical
) %>%
  #Only senate elections have special elections that we care abt
  mutate(special = ifelse(is.na(special), FALSE, special))

#Fundamentals
covi <- read_csv("cleaned_data/Cost of Voting.csv")[, -c(1)] 
expert <- read.csv("cleaned_data/Expert Ratings.csv")[, -c(1)]
genballot <- read.csv("cleaned_data/Generic Ballot.csv")[, -c(1)]
specials <- read.csv("cleaned_data/Specials.csv")[, -c(1)]
pvi <- read.csv("cleaned_data/Completed PVI.csv")[, -c(1)]
chambers <- read.csv("cleaned_data/Chamber Margins.csv") 
cci <- read.csv("cleaned_data/Consumer Confidence Index.csv")[, -c(1)] %>%
  rename(current_cci = current, 
         previous_cci = previous, 
         change_cci = change)
gas <- read.csv("cleaned_data/Gas Prices.csv")[, -c(1)] %>%
  rename(current_gas = current, 
       previous_gas = previous, 
       change_gas = change)
unemployment <- read.csv("cleaned_data/Unemployment.csv")[, -c(1)] %>%
  rename(current_unemployment = current, 
         previous_unemployment = previous, 
         change_unemployment = change)

#Polls


#Campaign Finance
fec <- read.csv("cleaned_data/fecCandidates20022024.csv")


combination <- elections_historical %>%
  left_join(covi, by = c('state', 'year')) %>%
  left_join(expert, by = c("state" = "State", "district" = "District", "year", 
                           "special", "office_type" = "race")) %>%
  left_join(genballot, by = 'year') %>%
  left_join(specials, by = 'year') %>%
  left_join(pvi, by = c('year', 'state', 'district')) %>%
  left_join(chambers, by = 'year') %>%
  left_join(cci, by = 'year') %>%
  left_join(gas, by = 'year') %>%
  left_join(unemployment, by = 'year') %>%
  select(-`...1`)

write.csv(combination, "cleaned_data/Finalized Dataset.csv")
