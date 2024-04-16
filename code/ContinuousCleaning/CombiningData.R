library(tidyverse)
library(missForest)
library(FNN)

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
  rename(year = CAND_ELECTION_YR, 
         state = CAND_OFFICE_ST, 
         office_type = CAND_OFFICE,
         district = CAND_OFFICE_DISTRICT,
         party = CAND_PTY_AFFILIATION, 
         receipts = allReceipts, 
         disbursements = allDisbursements, 
         indiv_contributions = allIndivContributions)  %>%
  #Want to keep district numbers correct -- Senate/Gov have district 0
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
  select(!contains("IND")) %>%
  #We want campaign finance to be comparable across years, so we account for inflation
  left_join(cpi, by = c('year' = 'year')) %>%
  mutate(across(matches("DEM|REP"), ~. * 100 / current)) %>%
  select(-c("X", 'current', 'previous', 'change'))

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
  left_join(unemployment, by = 'year') %>% #2024 included
  left_join(fec, by = c('state', 'year', 'district', 'office_type')) %>%
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
         receipts_ratio = case_when(
           receipts_DEM == 0 ~ -6, 
           receipts_REP == 0 ~ 6, 
           TRUE ~ log(receipts_DEM / receipts_REP)), 
         disbursements_ratio = case_when(
           disbursements_DEM == 0 ~ -6, 
           disbursements_REP == 0 ~ 6, 
           TRUE ~ log(disbursements_DEM / disbursements_REP)),
         total_receipts = receipts_DEM + receipts_REP,
         total_disbursements = disbursements_DEM + disbursements_REP,
         genballot_predicted_margin = pvi * 2 + weighted_genpoll + incumbent_differential, 
         genballot_predicted_lower = pvi * 2 + weighted_genpoll_lower + incumbent_differential, 
         genballot_predicted_upper = pvi * 2 + weighted_genpoll_upper + incumbent_differential,
         specials_predicted_margin = pvi * 2 + mean_specials_differential + incumbent_differential,
         num_polls = replace_na(num_polls, 0), 
         receipts_genballot_interaction = genballot_predicted_margin * receipts_ratio, 
         disbursements_genballot_interaction = genballot_predicted_margin * disbursements_ratio, 
         democrat_in_presidency = year %in% c(2010, 2012, 2014, 2016, 2022, 2024), 
         gas_democrat_interaction = democrat_in_presidency * current_gas, 
         cci_democrat_interaction = democrat_in_presidency * current_cci, 
         poll_fundamental_agree = sign(genballot_predicted_margin * unweighted_estimate)) %>%
  filter(!is.na(pvi))

#DOING MORE DATA ENGINEERING
#Many races do not have polls, but we can learn about what their polls might be by
#Looking at nearby races (in demographics)
demographics <- c("black_pct", "impoverished_pct", "renting_pct", "median_age")



# Function to calculate similar polling using weighted KNN
calculate_similar_polling_year <- function(data, k = 5) {
  # Find k-nearest neighbors and distances
  #Get.knnx takes a data matrix and a query data matrix

  data_with_polls <- data %>%
    filter(!is.na(unweighted_estimate)) %>%
    select(demographics)
  
  query_data <- data %>% select(demographics)
  
  neighbors_info <- get.knnx(data_with_polls, query_data, 
                             k = k, algorithm = "cover_tree")
  
  sapply(1:nrow(data), function(idx) {
    indices <- neighbors_info$nn.index[idx, ]
    distances <- neighbors_info$nn.dist[idx, ]
    neighbor_polling_differential <- data$unweighted_estimate[indices] - data$genballot_predicted_margin[indices]
    
    if (any(!is.na(neighbor_polls))) {
      return(weighted.mean(neighbor_polling_differential, w = 1 / (distances^2 + 1e-10), na.rm = TRUE))
    } else {
      return(NA)
    }
  })
}

get_dataset_with_similar_polls <- function(year_to_check) {
  similar_polls <- engineered %>% 
    filter(year == year_to_check) %>%
    mutate(across(c(black_pct, impoverished_pct, renting_pct, median_age), scale)) %>%  # Assuming these are your demographic columns
    mutate(similar_poll_differential = calculate_similar_polling(.))
  
  return (similar_polls)
}

all_years <- seq(2002, 2024, 2)
engineered <- reduce(map(all_years, ~get_dataset_with_similar_polls(.)), bind_rows) %>%
  mutate(combined_prediction = genballot_predicted_margin + similar_poll_differential)



write.csv(combination, "cleaned_data/Finalized Dataset.csv")
write.csv(engineered, "cleaned_data/Engineered Dataset.csv")
