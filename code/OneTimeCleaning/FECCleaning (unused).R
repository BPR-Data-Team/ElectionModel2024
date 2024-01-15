library(tidyverse)
library(glue)

clean_fec <- function(year) {
  read.csv(glue("data/fec data/year{year}.csv")) %>%
    rename(state = Candidate.state, 
           district = Candidate.district, 
           party = Party.affiliation, 
           receipts = Total.receipts, 
           individual_contributions = Total.individual.contributions) %>%
    mutate(year = year, 
           office_type = case_when(
             district == 0 ~ "Senate", 
             TRUE ~ "House"
           )) %>%
    filter(party %in% c("DEM", "REP")) %>%
    group_by(year, office_type, state, district, party) %>%
    summarize(receipts = sum(receipts),
              individual_contributions = sum(individual_contributions)) %>%
    pivot_wider(names_from = party,
                values_from = c(receipts, individual_contributions))
}

years <- seq(2002, 2022, by=2)
full_fec <- reduce(map(years, clean_fec), bind_rows)
write.csv(full_fec, "cleaned_data/Historical FEC.csv")
