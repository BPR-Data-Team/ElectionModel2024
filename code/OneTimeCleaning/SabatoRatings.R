library(tidyverse)
library(readxl)

party_abbrev <- function(party_vector) {
  party_vector <- str_replace(party_vector, "Democratic", "D")
  party_vector <- str_replace(party_vector, "Republican", "R")
}

#---- 2002 Elections (Senate, Governor)
sen_2002 <- read_excel("data/HistoricalElections/Sabato_Ratings/2002 Crystal Ball Ratings.xlsx", 
                       sheet = 'Sen') %>%
  rename(final_rating = `37560`, 
         initial_rating = `37500`, 
         State = `Senate race`) %>%
  mutate(year = 2002, 
         race = "Senate", 
         final_rating = ifelse(is.na(final_rating), initial_rating, final_rating), 
         State = str_remove(State, "\\*")) %>%
  select(year, State, final_rating, race)

gov_2002 <- read_excel("data/HistoricalElections/Sabato_Ratings/2002 Crystal Ball Ratings.xlsx", 
                       sheet = 'Gov', skip = 1) %>%
  rename(final_rating = `37560`,
         initial_rating = `37502`,
         State = `...1`) %>%
  mutate(year = 2002,
         race = "Governor",
         final_rating = ifelse(is.na(final_rating), initial_rating, final_rating),
         State = str_remove(State, "\\*")) %>%
  select(year, State, final_rating, race) %>%
  filter(!is.na(State))

#---- 2004 Elections (All)
sen_2004 <- read_excel("data/HistoricalElections/Sabato_Ratings/2004 Crystal Ball Ratings (what exists).xlsx", 
                                                      sheet = "Senate") %>%
  rename(final_rating = `38292`) %>%
  mutate(year = 2004, race = "Senate") %>%
  mutate(State = state.abb[match(State, state.name)]) %>%
  select(year, State, final_rating, race)

house_2004 <- read_excel("data/HistoricalElections/Sabato_Ratings/2004 Crystal Ball Ratings (what exists).xlsx", 
                      sheet = "House") %>%
  mutate(final_rating = ifelse(is.na(`38292`), `38085`, `38292`), 
         District = ifelse(CD == "AL", 0, CD), 
         race = "House", 
         year = 2004) %>%
  select(year, State, District, final_rating, race) %>%
  filter(!is.na(final_rating))

gov_2004 <- read_excel("data/HistoricalElections/Sabato_Ratings/2004 Crystal Ball Ratings (what exists).xlsx", 
                       sheet = "Gov") %>%
  rename(final_rating = `38292`) %>%
  mutate(year = 2004, race = "Governor") %>%
  select(year, State, final_rating, race)

pres_2004 <- read_excel("data/HistoricalElections/Sabato_Ratings/2004 Crystal Ball Ratings (what exists).xlsx", 
                       sheet = "Pres") %>%
  rename(final_rating = `38292`) %>%
  mutate(year = 2004, race = "President") %>%
  select(year, State, final_rating, race)


#---- 2006 Elections (Senate, Governor, House)
sen_2006 <- read_excel("data/HistoricalElections/Sabato_Ratings/2006 Crystal Ball Ratings.xlsx", 
                                         sheet = "2006Sen") %>%
  mutate(
    final_rating = coalesce(`New rating...12`, `New rating...10`, `New rating...8`, `New rating...6`, Rating),
    final_rating = str_replace(final_rating, "Democratic|I\\/D", "D"), 
    final_rating = str_replace(final_rating, "Republican", "R"), 
    final_rating = str_replace(final_rating, "Lieberman\\*", "I"),
    year = 2006, 
    race = "Senate"
  ) %>%
  select(year, State, final_rating, race)

gov_2006 <- read_excel("data/HistoricalElections/Sabato_Ratings/2006 Crystal Ball Ratings.xlsx", 
                       sheet = "2006Govs", skip = 1) %>%
  filter(!is.na(State)) %>%
  select(State, `Safe D...20`:`Safe R...26`) %>%
  pivot_longer(`Safe D...20`:`Safe R...26`, names_to = "final_rating") %>%
  filter(!is.na(value)) %>%
  mutate(final_rating = str_remove(final_rating, "\\..*"),  
         year = 2006, race = "Governor") %>%
  select(year, State, final_rating, race)

house_2006 <- read_excel("data/HistoricalElections/Sabato_Ratings/2006 Crystal Ball Ratings.xlsx", 
                         sheet = "2006HouseFINAL") %>%
  separate(CD, c("State", "District"), sep = "-") %>%
  rename(final_rating = `11/6/2006 rating`) %>%
  mutate(year = 2006, race = "House", District = str_replace(District, "AL", "00")) %>%
  select(year, State, District, final_rating, race)


#---- 2008 Elections (All)
sen_2008 <- read_excel("data/HistoricalElections/Sabato_Ratings/2008 Crystal Ball Ratings.xlsx", 
                       sheet = "2008 Senate ratings") %>%
  mutate(final_rating = coalesce(`Fourth Rating`, `Third Rating`, `Second Rating`,`First Rating`), 
         year = 2008, race = "Senate")  %>%
  rename(State = `Senate race`) %>%
  mutate(final_rating = party_abbrev(final_rating), 
         special = str_detect(State, "\\*"), 
         State = str_remove(State, "\\*")) %>%
  select(year, State, final_rating, race, special)

gov_2008 <- read_excel("data/HistoricalElections/Sabato_Ratings/2008 Crystal Ball Ratings.xlsx", 
                       sheet = "2008 Gov ratings") %>%
  mutate(final_rating = coalesce(`Next Rating...8`, `Next Rating...6`, Rating), 
         final_rating = party_abbrev(final_rating), 
         year = 2008, race = "Governor") %>%
  select(year, State, final_rating, race)

pres_2008 <- read_excel("data/HistoricalElections/Sabato_Ratings/2008 Crystal Ball Ratings.xlsx", 
                        sheet = "2008 elec college") %>%
  rename(State = ...1, final_rating = FINAL) %>%
  mutate(year = 2008, race = "President") %>%
  mutate(abbreviation = c(state.abb, "DC")[match(State, c(state.name, "District of Columbia"))]) %>%
  select(year, abbreviation, final_rating, race) %>%
  rename(State = abbreviation)

house_2008 <- read_excel("data/HistoricalElections/Sabato_Ratings/2008 Crystal Ball Ratings.xlsx", 
                         sheet = "2008 House ratings") %>%
  separate(Seat, c("State", "District"), sep = "-") %>%
  mutate(final_rating = coalesce(`Next Rating...8`, `Next Rating...6`, Rating),
         final_rating = party_abbrev(final_rating),
         year = 2008, race = "House", 
         District = str_replace(District, "AL", "00")) %>%
  select(year, State, District, final_rating, race) %>%
  filter(!is.na(final_rating))

#---- 2010 Elections (Senate, Gov, House)

sen_2010 <- read_excel("data/HistoricalElections/Sabato_Ratings/2010 Crystal Ball Ratings.xlsx", 
                       sheet = "Senate", skip = 3) %>%
  rename(State = ...1, final_rating = `FINAL 2010 RATING 11/1/10`) %>%
  mutate(final_rating = party_abbrev(final_rating),
         year = 2010, race = "Senate") %>%
  mutate(abbreviation = state.abb[match(State, state.name)]) %>%
  select(year, abbreviation, final_rating, race) %>%
  rename(State = abbreviation) %>%
  mutate(special = is.na(State), 
         State = ifelse(is.na(State), "NY", State))

gov_2010 <- read_excel("data/HistoricalElections/Sabato_Ratings/2010 Crystal Ball Ratings.xlsx", 
                       sheet = "Gov", skip = 1) %>%
  rename(State = ...1) %>%
  select(State, `Safe D...18`:`Safe R...24`) %>%
  pivot_longer(`Safe D...18`:`Safe R...24`, names_to = "final_rating") %>%
  filter(!is.na(value) & !is.na(State)) %>%
  mutate(final_rating = str_remove(final_rating, "\\..*"),  
         year = 2010, race = "Governor", 
         special = str_detect(State, "\\*"), 
         State = str_remove(State, "\\*")) %>%
  mutate(abbreviation = state.abb[match(State, state.name)]) %>%
  select(year, abbreviation, final_rating, race) %>%
  rename(State = abbreviation)

house_2010 <- read_excel("data/HistoricalElections/Sabato_Ratings/2010 Crystal Ball Ratings.xlsx", 
                         sheet = "HouseFINAL") %>%
  separate(CD, c("State", "District"), sep = "-") %>%
  rename(final_rating = `Final rating (11/1/2010)`) %>%
  mutate(year = 2010, race = "House", District = str_replace(District, "AL", "00"), 
         final_rating = party_abbrev(final_rating)) %>%
  select(year, State, District, final_rating, race)


#---- 2012 Elections (All)
sen_2012 <- read_excel("data/HistoricalElections/Sabato_Ratings/2011-12 Crystal Ball Ratings.xlsx", 
                       sheet = "2012 Senate") %>%
  mutate(final_rating = coalesce(`New rating...16`, `New rating...14`, `New rating...12`,
                                 `New rating...10`, `New rating...8`,`New rating...6`, 
                                 `New rating...4`), 
         final_rating = party_abbrev(final_rating), 
         final_rating = str_replace(final_rating, "D\\/I", "D"), 
         final_rating = str_replace(final_rating, "I\\/D", "I"), 
         year = 2012, race = "Senate") %>%
  rename(State = `Senate seat`) %>%
  select(year, State, final_rating, race)

gov_2012 <- read_excel("data/HistoricalElections/Sabato_Ratings/2011-12 Crystal Ball Ratings.xlsx", 
                       sheet = "2012 Govs") %>%
  filter(!is.na(State)) %>%
  mutate(final_rating = coalesce(`New rating...10`, `New rating...8`,`New rating...6`, 
                                 `New rating...4`), 
         final_rating = party_abbrev(final_rating), 
         year = 2012, race = "Governor",
         special = str_detect(State, "\\*"), 
         State = str_remove(State, "\\*")) %>%
  select(year, State, final_rating, race, special)

house_2012 <- read_excel("data/HistoricalElections/Sabato_Ratings/2011-12 Crystal Ball Ratings.xlsx", 
                        sheet = "2012 House") %>%
  separate(Seat, c("State", "District"), sep = "-") %>%
  mutate(final_rating = coalesce(`Next Rating...14`, `Next Rating...12`,
                                 `Next Rating...10`, `Next Rating...8`,`Next Rating...6`, 
                                 Rating),
    year = 2012, race = "House", District = str_replace(District, "AL", "00"), 
    final_rating = party_abbrev(final_rating)) %>%
  select(year, State, District, final_rating, race)

pres_2012 <- read_excel("data/HistoricalElections/Sabato_Ratings/2011-12 Crystal Ball Ratings.xlsx", 
                        sheet = "2012 Electoral College") %>%
  rename(State = ...1, final_rating = `Final rating`) %>%
  mutate(year = 2012, race = "President") %>%
  mutate(abbreviation = c(state.abb, "DC")[match(State, c(state.name, "District of Columbia"))]) %>%
  select(year, abbreviation, final_rating, race) %>%
  rename(State = abbreviation)

#---- 2014 Elections
sen_2014 <- read_excel("data/HistoricalElections/Sabato_Ratings/2014 Crystal Ball ratings.xlsx", 
                       sheet = "Senate") %>%
  rename(State = ...1, final_rating = FINAL) %>%
  mutate(year = 2014, race = "Senate", 
         special = State %in% c("Oklahoma S", "South Carolina S", "Hawaii"), 
         State = case_when(
           State == "Oklahoma S" ~ "Oklahoma",
           State == "South Carolina S" ~ "South Carolina", 
           State == "Lousiana" ~ "Louisiana", 
           TRUE ~ State
         )) %>%
  mutate(abbreviation = state.abb[match(State, state.name)]) %>%
  select(year, abbreviation, final_rating, special, race) %>%
  rename(State = abbreviation)

gov_2014 <- read_excel("data/HistoricalElections/Sabato_Ratings/2014 Crystal Ball ratings.xlsx", 
                       sheet = "Governor") %>%
  mutate(abbreviation = state.abb[match(State, state.name)],  
         abbreviation = ifelse(is.na(abbreviation), "NH", abbreviation),
         year = 2014, race = "Governor") %>%
  select(year, abbreviation, FINAL, race) %>%
  rename(State = abbreviation,
         final_rating = FINAL)
  
house_2014 <- read_excel("data/HistoricalElections/Sabato_Ratings/2014 Crystal Ball ratings.xlsx", 
                         sheet = "HouseFINAL") %>%
  separate(CD, c("State", "District"), sep = "-") %>%
  mutate(District = ifelse(District == "AL", "0", District),
         year = 2014, race = "House") %>%
  select(year, State, District, `Final Rating`, race) %>%
  rename(final_rating = `Final Rating`) %>%
  filter(!is.na(final_rating))


#---- 2016 Elections
sen_2016 <- read_excel("data/HistoricalElections/Sabato_Ratings/2016 Crystal Ball ratings.xlsx", 
              sheet = "Senate") %>%
  rename(State = ...1, final_rating = Final) %>%
  mutate(year = 2016, race = "Senate") %>%
  mutate(abbreviation = state.abb[match(State, state.name)]) %>%
  select(year, abbreviation, final_rating, race) %>%
  rename(State = abbreviation)

gov_2016 <- read_excel("data/HistoricalElections/Sabato_Ratings/2016 Crystal Ball ratings.xlsx", 
                       sheet = "Governor") %>%
  rename(State = State...1, final_rating = Final) %>%
  mutate(year = 2016, race = "Governor", 
         special = str_detect(State, "Special"), 
         State = str_remove(State, " Special")) %>%
  mutate(abbreviation = state.abb[match(State, state.name)]) %>%
  select(year, abbreviation, final_rating, race) %>%
  rename(State = abbreviation)

house_2016 <- read_excel("data/HistoricalElections/Sabato_Ratings/2016 Crystal Ball ratings.xlsx", 
                         sheet = "House") %>%
  separate(CD, c("State", "District"), sep = "-") %>%
  mutate(District = ifelse(District == "AL", "0", District),
         year = 2016, race = "House") %>%
  select(year, State, District, Final, race) %>%
  rename(final_rating = Final) %>%
  filter(!is.na(final_rating))


pres_2016 <- read_excel("data/HistoricalElections/Sabato_Ratings/2016 Crystal Ball ratings.xlsx", 
                        sheet = "Elec college") %>%
  rename(State = ...1, final_rating = Final) %>%
  mutate(year = 2016, race = "President") %>%
  mutate(abbreviation = c(state.abb, "DC")[match(State, c(state.name, "District of Columbia"))]) %>%
  select(year, abbreviation, final_rating, race) %>%
  rename(State = abbreviation) %>%
  filter(!is.na(final_rating))

#---- Rest of data, manually collected
sen_rest = read_excel("data/HistoricalElections/Sabato_Ratings/2018-2022 Crystal Ball Senate.xlsx") %>%
  mutate(special = str_detect(State, "\\*"), 
         State = str_remove(State, "\\*"))
gov_rest = read_excel("data/HistoricalElections/Sabato_Ratings/2018-2022 Crystall Ball Gubernatorial.xlsx")
house_rest <- read_excel("data/HistoricalElections/Sabato_Ratings/2018-2022 Crystal Ball.xlsx") %>%
  mutate(District = as.character(District))
pres_rest <- read_excel("data/HistoricalElections/Sabato_Ratings/2020 Crystal Ball Electoral College.xlsx") %>%
  mutate(race = "President")

#---- Combining all data into one dataframe
sen_total <- bind_rows(list(sen_2002, sen_2004, sen_2006, sen_2008, sen_2010, 
                            sen_2012, sen_2014, sen_2016, sen_rest))
gov_total <- bind_rows(list(gov_2002, gov_2004, gov_2006, gov_2008, gov_2010, 
                            gov_2012, gov_2014, gov_2016, gov_rest))
house_total <- bind_rows(list(house_2004, house_2006, house_2008, house_2010, 
                              house_2012, house_2014, house_2016, house_rest))
pres_total <- bind_rows(list(pres_2004, pres_2008, pres_2012, pres_2016, pres_rest))

combined_ratings <- bind_rows(sen_total, gov_total, house_total, pres_total) %>%
  mutate(
    State = case_when(
    State %in% c(state.name, "District of Columbia") ~ 
      c(state.abb, "DC")[match(State, c(state.name, "District of Columbia"))],
    State == "Dist. Of Col." ~ "DC",
    TRUE ~ State
  ), special = ifelse(is.na(special), FALSE, special), 
  District = ifelse(is.na(District), "0", District), 
  District = as.numeric(District), 
  District = ifelse(race == "House" & District == 0, 1, District)
  ) 

write.csv(combined_ratings, "cleaned_data/Expert Ratings.csv")