library(tidyverse)
library(missForest)
library(FNN)
library(openssl)

#--- Downloading Datasets ------
#This is by far the most important file in the entire project. It contains all the 
#code that combines House/Senate/Gov/President data with fundamentals/polling data


#---- ELECTION DATA -----
house <- read.csv("cleaned_data/AllHouse.csv") %>%
  rename(state = state_po) %>%
  mutate(office_type = "House") %>%
  select(year, state, district, office_type, open_seat, incumbent_differential, 
         margin)

senate <- read.csv("cleaned_data/AllSenate.csv") %>%
  mutate(office_type = "Senate") %>%
  select(year, state, district, office_type, open_seat, incumbent_differential,
         special_election, margin)

governor <- read.csv("cleaned_data/AllGovernor.csv") %>%
  mutate(office_type = "Governor",
         district = 0) %>%
  select(year, state, district, office_type, open_seat, incumbent_differential, 
         margin)

president <- read.csv("cleaned_data/AllPresident.csv") %>%
  mutate(office_type = "President") %>%
  select(year, state, district, office_type, open_seat, incumbent_differential, 
         margin)

all_elections <- bind_rows(house, senate, governor, president) %>%
  rename(special = special_election) %>%
  mutate(special = ifelse(is.na(special), FALSE, special))


#---- FUNDAMENTALS DATA -----
covi <- read_csv("cleaned_data/Cost of Voting.csv")[, -c(1)] 
#Need to fix expert ratings -- Chai will get me that
expert <- read.csv("cleaned_data/Expert Ratings.csv")[, -c(1)]
genballot <- read.csv("cleaned_data/Generic Ballot.csv")[, -c(1)] %>%
  #obviously don't want current margin... that doesn't exist yet!
  select(-c(gen_margin, gen_dem_tp))

specials <- read.csv("cleaned_data/Specials.csv")[, -c(1)]


#PVI contains proprietary information -- needs to be decoded using secret key
pvi_key <- Sys.getenv("PVI_Key")
pvi_encoded <- 


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

cpi <- read.csv("cleaned_data/CPI.csv") 

inflation <- cpi %>%
  select(year, change) %>%
  rename(inflation = change)

#Campaign Finance -- dealing with inflation as well
fec <- read.csv("cleaned_data/fecData20022024.csv") %>%
  select(-is_open) %>% 
  mutate(state = ifelse(state == "US", list(state.abb), as.list(state))) %>%
  unnest(cols = c(state))

fec_summary <- fec %>% group_by(year, office_type) %>% summarise(
  across(matches("DEM|REP"), ~sum(., na.rm = TRUE))
) %>%
  mutate(
    receipt_ratio = log(receipts_DEM / receipts_REP),
    disbursement_ratio = log(disbursements_DEM / disbursements_REP),
    contribution_ratio = log(individual_contributions_DEM / individual_contributions_REP)
  )

g1 <- lm(year ~ receipt_ratio + disbursement_ratio + contribution_ratio, data = fec_summary)


#POLLS... wow this is only two lines lol
polls <- read.csv("cleaned_data/AllPolls.csv") %>% select(-X) %>%
  mutate(office_type = str_remove(office_type, "U\\.S\\. "))

genpolls <- read.csv("cleaned_data/GenPolling.csv") %>% select(-X)

#DEMOGRAPHICS
demographics <- read.csv("cleaned_data/Demographics.csv") %>% select(-X) %>%
  #Weird combinations where both district and state-level dems are the same
  #Specifically for 1-district states
  unique()

#LEFT TO DO:
# - Incorporate Chai's Rating Code
combination <- all_elections %>%
  left_join(covi, by = c('state', 'year')) %>% #2024 included
  left_join(expert, by = c("state" = "State", "district" = "District", "year",
                            "special", "office_type" = "race")) %>% #2024 not included
  left_join(genballot, by = 'year') %>% #2024 included
  left_join(genpolls, by = 'year') %>%
  left_join(specials, by = 'year') %>% #2024 included?
  left_join(pvi, by = c('year', 'state', 'district')) %>% #2024 included
  left_join(chambers, by = 'year') %>% #2024 included
  left_join(cci, by = 'year') %>% #2024 included
  left_join(gas, by = 'year') %>% #2024 included
  left_join(unemployment, by = 'year') %>%  #2024 included
  left_join(fec, by = c('state', 'year', 'district', 'office_type'), relationship = 'many-to-many') %>%
  left_join(polls, by = c('state', 'year', 'district' = 'seat_number', 'office_type')) %>%
  left_join(demographics, by = c('state', 'year', 'district')) %>%
  left_join(inflation, by = 'year') %>%
  mutate(isMidterm = year %% 4 != 0) %>%
  filter(!is.na(state))

#--- DATA ENGINEERING
engineered <- combination %>% 
  #Lots of missing data and totally useless in general
  select(-c(maxpollhours, noonlineregistration, nopermanentabsentee)) %>% 
  mutate(incumbent_differential = ifelse(is.na(incumbent_differential), 
                                         0, incumbent_differential), 
         genballot_predicted_margin = pvi * 2 + weighted_genpoll + incumbent_differential, 
         genballot_predicted_lower = pvi * 2 + weighted_genpoll_lower + incumbent_differential, 
         genballot_predicted_upper = pvi * 2 + weighted_genpoll_upper + incumbent_differential,
         specials_predicted_margin = pvi * 2 + mean_specials_differential + incumbent_differential,
         num_polls = replace_na(num_polls, 0), 
         receipts_genballot_interaction = genballot_predicted_margin * receipts, 
         disbursements_genballot_interaction = genballot_predicted_margin * disbursements, 
         democrat_in_presidency = year %in% c(2010, 2012, 2014, 2016, 2022, 2024), 
         gas_democrat_interaction = democrat_in_presidency * current_gas, 
         cci_democrat_interaction = democrat_in_presidency * current_cci, 
         poll_fundamental_agree = sign(genballot_predicted_margin * unweighted_estimate)) %>%
  filter(!is.na(pvi)) %>%
  filter(!(state == "AK" & office_type == "House" & year == 2022) &
           !(state == "LA" & office_type == "House") & 
           !(state == "CA" & year == 2012 & district %in% c(8, 15, 30, 31, 35, 40, 43, 44)) & 
           !(state == "CA" & year == 2014 & district %in% c(4,17,19, 25,34, 35, 40, 44)) & 
           !(state == "CA" & year == 2016 & district %in% c(17, 29, 32, 34, 37, 44, 46)))

write.csv(engineered, "cleaned_data/Engineered Dataset.csv")



