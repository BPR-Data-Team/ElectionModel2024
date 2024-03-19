library(tidyverse)

#--- Downloading Datasets ------
#This is by far the most important file in the entire project. It contains all the 
#code that combines House/Senate/Gov/President for both historical and current
#data.

#It works in the following way: first, get the historical data from the cleaned
#files in cleaned_data. Then, get the manually-created current datasets, and 
#split them into a names file (for use later in the UI) and a file that looks
#identical to the historical data, except there is NO margin (obviously...)

#------- HOUSE --------#
house_historical <- read_csv("cleaned_data/House Historical.csv") %>%
  mutate(office_type = "House") %>%
  rename(state = state_po)

#Getting current house data
house_current <- read.csv("data/2024House.csv") %>%
  mutate(District = ifelse(District == "at-large", "1", District), 
         District = as.numeric(District), 
         Year = 2024, 
         State = state.abb[match(State, state.name)])

house_names <- house_current %>% 
  select(Year, State, District, Office_type, R_name, D_name, Unopposed.independent) %>%
  rename(year = Year, state = State, district = District,
         I_name = Unopposed.independent)

house_current <- house_current %>%
  #Matching data from previous years to get incumbent margin
  left_join((house_historical %>% filter(year == 2022)), 
            by = c("District" = "district", "State" = "state")) %>%
  #Need to fix this step, as I do with every other incumbent margin step
  mutate(incumbent_differential = case_when(
    Incumbent == "TRUE" ~ margin, 
    TRUE ~ NA_real_
  ), Incumbent = ifelse(Incumbent %in% c("TRUE", "FALSE"), 
                        Incumbent, "FALSE"), 
  Incumbent = as.logical(Incumbent)) %>%
  select(c(Year, State, District, Office_type, 
           Incumbent, incumbent_differential, Sabato)) %>%
  rename(year = Year, state = State, district = District, open_seat = Incumbent, 
         open_seat = Incumbent, final_rating = Sabato,
         office_type = Office_type)

house_all <- bind_rows(house_historical, house_current) %>%
  select(year, state, district, office_type, open_seat, incumbent_differential, 
         final_rating, margin)


#------- SENATE --------#
sen_historical <- read.csv("cleaned_data/Senate Historical.csv") %>%
  rename(
    state = State, 
    year = Year, 
    margin = Margin, 
    incumbent_differential = Incumbent.Margin, 
    open_seat = Open.Seat., 
    special = Special.Election.
  ) %>%
  #Connecticut 2006 had an independent win -- we don't include that election
  filter(Republican.Total.. != 0 & Democratic.Total.. != 0 &
           !(state == "Connecticut" & year == 2006) & 
           state != "Maine") %>%
  mutate(office_type = "Senate", 
         district = 0, 
         margin = as.numeric(margin)) %>%
  select(state, year, office_type, district, margin, open_seat, 
         incumbent_differential, special) %>%
  #State abbreviations are used here, so we match by state
  mutate(state = state.abb[match(state, state.name)])

sen_current <- read.csv("data/2024Senate.csv") %>%
  filter(State != "California Special") %>%
  mutate(District = 0,
         Year = 2024, 
         Special = str_detect(State, "Special"), 
         State = str_remove(State, " Special"),
         State = state.abb[match(State, state.name)]) 

sen_names <- sen_current %>% 
  select(Year, State, District, Office_type, R_name, D_name, Unopposed.independent) %>%
  rename(year = Year, state = State, district = District,
         I_name = Unopposed.independent)

sen_current <- sen_current %>%
  #Matching data from previous years to get incumbent margin
  left_join((sen_historical %>% filter(year == 2022)), 
            by = c("District" = "district", "State" = "state")) %>%
  #Need to fix this step, as I do with every other incumbent margin step
  mutate(incumbent_differential = case_when(
    Incumbent == "TRUE" ~ margin, 
    TRUE ~ NA_real_
  ), Incumbent = ifelse(Incumbent %in% c("TRUE", "FALSE"), 
                        Incumbent, "FALSE"), 
  open_seat = !as.logical(Incumbent)) %>%
  select(c(Year, State, District, Office_type, 
           open_seat, incumbent_differential, Sabato, Special)) %>%
  rename(year = Year, state = State, district = District, final_rating = Sabato,
         office_type = Office_type, special = Special)

sen_all <- bind_rows(sen_historical, sen_current)


#------ GOVERNOR -------#
gov_historical <- read.csv("cleaned_data/GovernorHistorical.csv") %>%
  mutate(district = 0, 
         office_type = "Governor")

gov_current <- read.csv("data/2024Governor.csv") %>%
  mutate(year = 2024, district = 0, 
         state = state.abb[match(State, state.name)]) %>%
  rename(office_type = Office_type)

gov_names <- gov_current %>%
  select(year, state, office_type, R_name, D_name, Unopposed.independent)

gov_current <- gov_current %>%
  mutate(open_seat = !Incumbent, 
         district = 0) %>%
  select(year, state, district, office_type, open_seat, Incumbent_margin) %>%
  rename(incumbent_differential = Incumbent_margin)

gov_all <- bind_rows(gov_historical, gov_current) %>%
  select(-X)

#------ PRESIDENT ------#

pres_historical <- read_csv("cleaned_data/President Historical.csv") %>%
  mutate(office_type = "President", 
         district = 0)

pres_current <- read.csv("data/2024President.csv") %>%
  mutate(office_type = "President")

#Getting name dataframe for president
pres_names <- pres_current %>%
  select(year, state, district, office_type, R_name, D_name, I_name)

pres_current <- pres_current %>%
  select(year, state, district, office_type, open_seat)

pres_all <- bind_rows(pres_historical, pres_current)


#Combining all previous elections
all_elections <- bind_rows(
  house_all, sen_all, gov_all, pres_all
) %>%
  #Only senate elections have special elections that we care abt
  mutate(special = ifelse(is.na(special), FALSE, special)) %>%
  select(-...1)

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
#GET CPI DATA FROM ALEX!

fec <- read.csv("cleaned_data/fecData20022024.csv") %>%
  rename(year = CAND_ELECTION_YR, 
         state = CAND_OFFICE_ST, 
         office_type = CAND_OFFICE,
         district = CAND_OFFICE_DISTRICT,
         party = CAND_PTY_AFFILIATION, 
         receipts = allReceipts, 
         disbursements = allDisbursements, 
         indiv_contributions = allIndivContributions
  ) %>%
  mutate(district = ifelse(district == 0 & office_type == 'H', 
                           1, district), 
         office_type = case_when(
           office_type == "H" ~ "House", 
           office_type == "S" ~ "Senate"
         )) %>%
  pivot_wider(id_cols = c(year, state, office_type, district),
              names_from = party,
              values_from = c(receipts, disbursements, indiv_contributions), 
              values_fn = sum) %>%
  select(!contains("IND"))

#POLLS
polls <- read.csv("cleaned_data/538 Polls.csv")

#DEMOGRAPHICS
demographics <- read.csv("cleaned_data/Demographics.csv")


combination <- all_elections %>%
  left_join(covi, by = c('state', 'year')) %>% #2024 included
  left_join(expert, by = c("state" = "State", "district" = "District", "year", 
                           "special", "office_type" = "race")) %>% #2024 not included 
  left_join(genballot, by = 'year') %>% #2024 included
  left_join(specials, by = 'year') %>% #2024 included?
  left_join(pvi, by = c('year', 'state', 'district')) %>% #2024 included
  left_join(chambers, by = 'year') %>% #2024 included
  left_join(cci, by = 'year') %>% #2024 included
  left_join(gas, by = 'year') %>% #2024 included
  left_join(unemployment, by = 'year') %>% #2024 included
  left_join(fec, by = c('state', 'year', 'district', 'office_type'))
  #left_join(demographics, by = c('state', 'year', 'district')) %>%

write.csv(combination, "cleaned_data/Finalized Dataset.csv")
