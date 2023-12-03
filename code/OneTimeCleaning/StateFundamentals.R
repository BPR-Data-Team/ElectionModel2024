library(tidyverse)
library(readxl)

# ---- Working with COVI Master Data ------ #
covi_2000 <- read_excel("data/Master 1996 2022 Election voting requirements.xlsx", 
                        sheet = "2000")
covi_2004 <- read_excel("data/Master 1996 2022 Election voting requirements.xlsx", 
                        sheet = "2004")
covi_2008 <- read_excel("data/Master 1996 2022 Election voting requirements.xlsx", 
                        sheet = "2008")
covi_2012 <- read_excel("data/Master 1996 2022 Election voting requirements.xlsx", 
                        sheet = "2012")
covi_2016 <- read_excel("data/Master 1996 2022 Election voting requirements.xlsx", 
                        sheet = "2016")
covi_2020 <- read_excel("data/Master 1996 2022 Election voting requirements.xlsx", 
                        sheet = "2020")
covi_2022 <- read_excel("data/Master 1996 2022 Election voting requirements.xlsx", 
                        sheet = "2022")
covi_values <- read_excel("data/COVI.xlsx") %>%
  mutate(COVI_num = case_when(
    is.na(FinalCOVI) ~ InitialCOVI, 
    TRUE ~ FinalCOVI
  )) %>%
  select(c('state', 'year', 'COVI_num')) %>%
  filter(year >= 2000)
  

#list of variables to keep
vars <- c("State.y", "Year", "AbsenteeExcuseReq", "PollHours", "AvgPollHours", "MaxPollHours","MinPollHours", "Issue Area #1-Registration Deadlines", "VoterIDLaws", "NoVoterID", "NoAllMailVote", "NoEarlyVote", "NoFelonReg", "NoFelonsRegAfterIncar", 
          "nonstrictID", "nonstrictPhoto", "NOonlineregistration", "NoPermanentAbsentee", 
          "NoPollPlaceReg", "NoPR", "NoSameDayReg", "nostateholiday", 
          "PR16", "PR17", "PR175", "PR60", "PR90", "strictID", "strictPhoto")

state_numbers <- covi_2000 %>%
  select(statenu, State)

#I want to keep a masterdoc of all important covi values, in addition 
full_covi <- bind_rows(covi_2000, covi_2004, covi_2008, covi_2012, covi_2016,
                       covi_2020, covi_2022) %>%
  mutate(NoFelonReg = ifelse(is.na(NoFelonReg), NoFelonsReg, NoFelonReg), 
         NoAllMailVote = ifelse(is.na(NoAllMailVote), noallmailvoting, NoAllMailVote), 
         NoVoterID = ifelse(is.na(NoVoterID), NovoterID, NoVoterID),
         MaxPollHours = ifelse(is.na(MaxPollHours), PollHoursMax, MaxPollHours),
         MinPollHours = ifelse(is.na(MinPollHours), PollHoursMin, MinPollHours),
         AvgPollHours = ifelse(is.na(AvgPollHours), PollHrsAvg, AvgPollHours),
         VoterIDLaws = ifelse(is.na(`Issue Area #5-Voter ID Laws`), `Issue Area #7-Voter ID Laws`, 
                              `Issue Area #5-Voter ID Laws`),
         PollHours = ifelse(is.na(`Issue Area #6-Poll Hours`), `Issue Area #8-Poll Hours`,
                            `Issue Area #6-Poll Hours`),
         .keep = "unused") %>%
  full_join(state_numbers, by = "statenu") %>%
  select(all_of(vars)) %>%
  rename(regdeadlines = `Issue Area #1-Registration Deadlines`, 
         State = State.y) %>%
  #Now combining with the COVI value calculated by the website
  full_join(covi_values, by = c('Year' = 'year', 'State' = 'state')) %>%
  rename_with(tolower) %>%
  rowwise() %>%
  #If a state has a given COVI for 2012 and 2016, we want to use the 2012 values
  #for 2014
  #However, since we have 2022 COVI, we don't need to use 2020 for 2022
  mutate(year = case_when(
    year == 2020 ~ list(year), 
    TRUE ~ list(c(year, year + 2))
  )) %>%
  unnest(year)

write.csv(full_covi, "cleaned_data/cost_of_voting.csv")


#---- Done working with COVI! Now, working on Unemployment Data ----- #
state_unemployment <- read_excel("data/Unemployment.xlsx", skip = 6) %>%
  slice(-1) %>%
  rename(State = ...2, year = ...3, unemployment = rate) %>%
  filter(year >= 2000) %>%
  arrange(State, year) %>%
  group_by(State) %>%
  #year should be 1 greater, since we only get unemployment data post-election
  mutate(unemployment_prev = lag(unemployment, 1),
         year = as.numeric(year) + 1) %>%
  ungroup() %>%
  filter(!is.na(unemployment_prev) & (year %% 2 == 0)) %>%
  mutate(change_unemployment = unemployment - unemployment_prev) %>%
  select(c("State", "year", "unemployment", "change_unemployment"))

write.csv(state_unemployment, "cleaned_data/unemployment.csv")

