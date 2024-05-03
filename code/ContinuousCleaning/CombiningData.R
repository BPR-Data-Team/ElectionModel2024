library(tidyverse)
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
  unnest(cols = c(state)) %>%
  mutate(district = ifelse(state == "ME" & office_type == "President" & year == 2024, 
                           list(c(0, 1, 2)), as.list(district)), 
         district = ifelse(state == "NE" & office_type == "President" & year == 2024, 
                           list(c(0, 1, 2, 3)), district))%>%
  unnest(cols = c(district))

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
         poll_fundamental_agree = sign(genballot_predicted_margin * unweighted_estimate), 
         finance_fundamental_agree = sign(genballot_predicted_margin * receipts),
         expert_rating_democrat = case_when(
           grepl(" D", final_rating) ~ 1, 
           grepl("Toss", final_rating) ~ 0, 
           grepl(" R", final_rating) ~ -1
         )) %>%
  filter(!is.na(pvi)) %>%
  filter(!(state == "AK" & office_type == "House" & year == 2022) &
           !(state == "LA" & office_type == "House") & 
           !(state == "CA" & year == 2012 & district %in% c(8, 15, 30, 31, 35, 40, 43, 44)) & 
           !(state == "CA" & year == 2014 & district %in% c(4,17,19, 25,34, 35, 40, 44)) & 
           !(state == "CA" & year == 2016 & district %in% c(17, 29, 32, 34, 37, 44, 46)))


#Polling should have an increased impact as the election gets closer
days_until_election <- as.numeric(as.Date("2024-11-04") - today())

#Prior to the election, polls should be weighted as the following:
poll_weight <- (200 - days_until_election) / 200

#The following should be added to lower bound and upper bound
bounds_increase <- 5 * days_until_election / 200

#Creating a genballot based off of individual race polls -- if races are considerably 
#more right/left-wing, we'd expect the generic ballot to be as well!
genballot_polling_individual <- engineered %>%
  filter(!is.na(weighted_estimate)) %>%
  group_by(year) %>%
  summarize(genballot_individual = case_when(
  cur_group() == 2024 ~ poll_weight * mean(weighted_estimate - (pvi * 2 + incumbent_differential)), 
  TRUE ~ mean(weighted_estimate - (pvi * 2 + incumbent_differential))))

#Creating a genballot feature based off of campaign finance -- trying three different weights
genballot_campaign_finance <- engineered %>%
  filter(!is.na(receipts_DEM) & !is.na(receipts_REP)) %>%
  group_by(year) %>%
  summarize(total_receipts_DEM = sum(receipts_DEM), 
            total_receipts_REP = sum(receipts_REP)) %>%
  mutate(genballot_campaign5 = 5*log(total_receipts_DEM/total_receipts_REP), 
         genballot_campaign10 = 10*log(total_receipts_DEM/total_receipts_REP), 
         genballot_campaign15 = 15*log(total_receipts_DEM/total_receipts_REP)) %>%
  select(year, genballot_campaign5, genballot_campaign10, genballot_campaign15)

final <- engineered %>%
  left_join(genballot_polling_individual, by = c('year')) %>%
  left_join(genballot_campaign_finance, by = c('year')) %>%
  mutate(
    average_genballot = (genballot_individual + weighted_genpoll + genballot_campaign10) / 3,
    genballot_individual_predicted_margin = pvi * 2 + genballot_individual + incumbent_differential, 
    genballot_campaign5_predicted_margin = pvi * 2 + genballot_campaign5 + incumbent_differential, 
    genballot_campaign10_predicted_margin = pvi * 2 + genballot_campaign10 + incumbent_differential, 
    genballot_campaign15_predicted_margin = pvi * 2 + genballot_campaign15 + incumbent_differential, 
    average_genballot_predicted_margin = pvi * 2 + average_genballot + incumbent_differential
  ) 

#We don't just want generic ballot polls to have a reduced effect -- we want 
#All polls to have a reduced effect! This deals with individual polls
decreasing_poll_efficacy <- final %>%
  filter(year == 2024) %>%
  mutate(across(c(phone_unweighted, online_unweighted, 
                  unweighted_estimate, weighted_estimate), 
                ~ (pvi * 2 + incumbent_differential) + poll_weight * (. - (pvi * 2 + incumbent_differential))), 
         across(c(unweighted_ci_lower, weighted_ci_lower), ~ . - bounds_increase), 
         across(c(unweighted_ci_upper, weighted_ci_upper), ~ . + bounds_increase))

#Final dataset
final <- final %>%
  filter(year != 2024) %>%
  bind_rows(decreasing_poll_efficacy)


name_dataset <- read.csv("data/AllRaces.csv") %>%
  select(State, District, Office_type, R_name, D_name, Weird) %>% 
  rename(state = State, 
         district = District,
         office_type = Office_type, 
         rep_name = R_name, 
         dem_name = D_name,
         weird = Weird) %>%
  mutate(district = ifelse(district == "at-large", 1, district), 
         state = trimws(state),
         state = ifelse(str_length(state) == 2, state, state.abb[match(state, state.name)])) %>%
  filter(state != "NE" | weird == "") %>%
  mutate(rep_name = ifelse(grepl("/|n/a", rep_name) | rep_name == "", "Unknown", rep_name), 
         dem_name = ifelse(grepl("/|n/a", dem_name) | dem_name == "", "Unknown", dem_name), 
         state = ifelse(is.na(state), "NE", state), 
         district = as.numeric(district), 
         district = ifelse(is.na(district), 0, district))

write.csv(final, "cleaned_data/Engineered Dataset.csv")
write.csv(name_dataset, "cleaned_data/Names Dataset.csv")



