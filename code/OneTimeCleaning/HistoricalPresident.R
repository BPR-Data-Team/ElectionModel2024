library(tidyverse)
library(readxl)

load("data/HistoricalElections/electoral_studies_replication_presidential_analysis_data.Rdata")

pres_uncleaned <- electoral_studies_presidential
rm(electoral_studies_presidential)



# pres_history <- read.csv("../data/HistoricalElections/PresidentialHistory.csv") %>%
#   mutate(candidate = case_when(
#     candidate == "MITT, ROMNEY" ~ "ROMNEY, MITT", 
#     TRUE ~ candidate
#   )) #for some reason, mitt romney's name is backwards sometimes

# #getting every major candidate and their party
# candidate_to_party <- pres_history %>%
#   filter(year >= 1996 & party_simplified %in% c("DEMOCRAT", "REPUBLICAN") 
#          & !(candidate %in% c("", "OTHER"))) %>%
#   select(c("candidate", "party_simplified")) %>%
#   unique()
# 
# #calculating the total number of votes in each presidential election
# total_votes <- pres_history %>%
#   select(c("year", "state_po", "totalvotes")) %>%
#   unique()
# 
# #full list of historical presidential data
# cleaned_pres <- pres_history %>%
#   filter(year >= 1996 & !writein) %>%
#   group_by(year, state_po, candidate) %>%
#   summarise(votes = sum(candidatevotes)) %>%
#   right_join(candidate_to_party, by = "candidate") %>%
#   select(-"candidate") %>%
#   pivot_wider(names_from = party_simplified, values_from = votes) %>%
#   left_join(total_votes, by = c("year" = "year", "state_po" = "state_po")) %>%
#   mutate(margin = 100 * (DEMOCRAT - REPUBLICAN) / (totalvotes)) %>%
#   select("year", "state_po", "margin") 
# 
# write.csv(cleaned_pres, "../cleaned_data/historical_president.csv")
