library(tidyverse)

past_data <- read.csv('cleaned_data/Finalized Dataset.csv') %>% 
  filter(year == 2022)

house <- read.csv("data/2024-races-house.csv") %>%
  mutate(District = ifelse(District == "at-large", "1", District), 
         District = as.numeric(District), 
         Year = 2024, 
         State = state.abb[match(State, state.name)]) %>%
  left_join(past_data, by = c("District" = "district", "State" = "state")) %>%
  mutate(incumbent_margin = case_when(
    Incumbent == TRUE ~ margin, 
    TRUE ~ NA_real_
  )) %>%
  select(c(Year, State, District, Office_type, R_name, D_name, Unopposed.independent, 
           Incumbent, incumbent_margin, Sabato)) %>%
  rename(year = Year, state = State, district = District, open_seat = Incumbent, 
         I_name = Unopposed.independent, open_seat = Incumbent, final_rating = Sabato)