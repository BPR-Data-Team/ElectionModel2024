library(tidyverse)
library(readxl)

covi <- read_excel("data/COVI Values 1996-2022 website (1).xlsx")
senate_results <- read.csv("data/1976-2020-senate.csv")
house_results <- read.csv("data/1976-2022-house.csv")

# ---- Getting generic ballot results -------
unopposed_prop <- 0.71 #The average % of the vote 
                        #an unopposed candidate would have received, had they been opposed

contested_party_summaries <- house_results %>%
  filter(totalvotes > 1, party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(year, party) %>%
  summarize(total_party_votes = sum(candidatevotes))

contested_total_summaries <- house_results %>%
  filter(totalvotes > 1, party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(year) %>%
  summarize(mean_votes = mean(totalvotes))

uncontested_party_summaries <- house_results %>%
  filter(totalvotes <= 1, party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(year, party) %>%
  summarize(party_uncontested = n())

uncontested_total_summaries <-  house_results %>%
  filter(totalvotes <= 1, party %in% c("DEMOCRAT", "REPUBLICAN")) %>%
  group_by(year) %>%
  summarize(total_uncontested = n())

generic_ballot <- contested_party_summaries %>%
  full_join(contested_total_summaries, by = "year") %>%
  full_join(uncontested_party_summaries, by = c("year", "party")) %>%
  full_join(uncontested_total_summaries, by = "year") %>%
  mutate(party_uncontested = replace_na(party_uncontested, 0)) %>%
  mutate(opposite_uncontested = total_uncontested - party_uncontested) %>%
  mutate(modeled_vote = total_party_votes +
           (party_uncontested * mean_votes * unopposed_prop) +
           (opposite_uncontested * mean_votes * (1 - unopposed_prop))) %>%
  select(year, party, modeled_vote) %>%
  pivot_wider(names_from = "party",
              values_from = "modeled_vote") %>%
  mutate(pct_margin = (DEMOCRAT - REPUBLICAN) / (DEMOCRAT + REPUBLICAN) * 100) %>%
  select(year, pct_margin) %>%
  write_csv("cleaned_data/generic_ballot.csv")




