library(tidyverse)
library(lubridate)
library(readxl)

actual_margins <- read.csv("cleaned_data/generic_ballot.csv")

natl_polls <- read_csv("data/raw-polls.csv") %>%
  filter(location == "US" & year >= 2002 & type_simple == "House-G") %>%
  select(c("year", "race", "pollster_rating_id", "methodology", 
           "samplesize", "cand1_pct", "cand2_pct")) %>%
  left_join(actual_margins, by = 'year')
