library(tidyverse)
library(lubridate)

#Reading in presidential data from 538's Github
uncleaned_pres <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv")
pres_cleaned <- uncleaned_pres %>%
  mutate(end_date = as.Date(end_date, "%m/%d/%y")) %>%
  filter(today() - end_date <= 21) %>% # only choosing polls conducted within the past 21 days
  select(c("poll_id", "question_id", "pollster_rating_id", "methodology", "state", "sample_size", "partisan",
           "population", "party", "answer", "pct")) %>%
  mutate(party = case_when(
    party %in% c("DEM", "REP") ~ party, 
    TRUE ~ "IND"
  )) %>%
  group_by(poll_id, question_id, party) %>%
  summarize(pct = sum(pct), 
            across(everything(), .fns = ~.))
  
    