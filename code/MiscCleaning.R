library(tidyverse)
library(readxl)

covi <- read_excel("../data/COVI Values 1996-2022 website (1).xlsx")

covi <- covi %>%
  select(c("state", "year", "FinalCOVI")) %>%
  mutate(FinalCOVI = case_when(
    year == 2022 ~ InitialCOVI, 
    TRUE ~ FinalCOVI
  ))