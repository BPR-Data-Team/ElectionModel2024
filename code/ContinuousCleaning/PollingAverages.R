library(tidyverse)
library(lubridate)
library(stringr)

days_counting <- 21
population_order <- c('lv', 'rv', 'v', 'a')
numberOfPolls <- 1

#Get poll ratings, extract relevant columns
pollRatings <- read.csv("cleaned_data/Pollster Ratings.csv") %>%
  select(c(year, pollster_rating_id, valid, lower_error_diff, mean_bias)) %>%
  mutate(valid = as.logical(valid))

raw_polls <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw_polls.csv")# pivot_longer(cols = c(cand1_party, cand2_party),

#Cleaning past polls to make them look the same as current polls
cleaned_historical <- raw_polls %>%
  #Not looking at independents
  filter(cand1_party == "DEM" & cand2_party == "REP" & 
            time_to_election <= days_counting) %>%
  select(c(poll_id, question_id, cycle, location, type_simple, 
           pollster_rating_id, methodology, partisan, samplesize, 
           cand1_party, cand2_party, cand1_pct, cand2_pct)) %>%
  #Getting district number from race column
  separate(location, sep = "-", into = c("state", "seat_number")) %>%
  mutate(seat_number = as.numeric(ifelse(is.na(seat_number), 0, seat_number))) %>%
  pivot_wider(names_from = cand1_party, values_from = cand1_pct) %>%
  pivot_wider(names_from = cand2_party, values_from = cand2_pct) %>%
  #We don't want primary polls or generic polls
  filter(!str_detect(type_simple, "-P|US")) %>%
  mutate(office_type = case_when(
    str_detect(type_simple, "Sen") ~ "Senate", 
    str_detect(type_simple, "Gov") ~ "Governor",
    str_detect(type_simple, "Pres") ~ "President",
    str_detect(type_simple, "House") ~ "House"),) %>%
  left_join(pollRatings, by = c("cycle" = 'year', 'pollster_rating_id')) %>%
  rename(sample_size = samplesize) %>%
  select(poll_id, pollster_rating_id, methodology, state, seat_number, sample_size, 
         cycle, office_type, DEM, REP, valid, lower_error_diff, mean_bias)

#Get current polls from online, 538 stream
uncleaned_current <- bind_rows(
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv"), 
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv"), 
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/house_polls.csv"), 
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls.csv")
)

#Initial cleaning
cleaned_current <- uncleaned_current %>%
  filter(as.Date(Sys.Date()) - as.Date(mdy(end_date)) <= days_counting) %>%
  group_by(state) %>%
  mutate(num_polls = n_distinct(poll_id)) %>%
  ungroup() %>%
  select(poll_id, pollster_rating_id, methodology, state, seat_number, question_id, 
         sample_size, population_full, cycle, office_type, party, pct, answer, num_polls)

#For each question, we only care about the answer that has the max result:
#For example a question might ask "Biden vs Trump vs I" AND "Biden v Trump", 
#Which is very annoying -- #We take the first
max_pct_sums <- cleaned_current %>% 
  group_by(poll_id,pollster_rating_id,question_id,state,seat_number) %>% 
  summarize(total_pct = sum(pct)) %>% 
  filter(total_pct == max(total_pct))

#Merging back into the cleaned polls
cleaned_current <- cleaned_current %>%
  #Only keeps poll questions with max total responses, by poll
  right_join(max_pct_sums, by = c('poll_id', 'pollster_rating_id', 'question_id', 
                                  'state', 'seat_number')) 

#Eventually we'll use the independent polls, but not now
independent_polls <- cleaned_current %>% 
  filter(!(party %in% c("DEM", "REP"))) 
  
cleaned_current <- cleaned_current %>% 
  mutate(party = ifelse(party %in% c("DEM", "REP"), party, "IND")) %>%
  #filter(party %in% c("DEM", "REP")) %>%
  #Gets sum of percent by party rather than by candidate
  group_by(poll_id, pollster_rating_id, methodology, state, seat_number, question_id, 
           sample_size, population_full, cycle, office_type, party) %>%
  summarize(pct = sum(pct)) %>%
  #This time, not including question_id -- we want the median over all questions
  #In a given poll
  group_by(poll_id, pollster_rating_id, methodology, state, seat_number, 
           sample_size, population_full, cycle, office_type, party) %>%
  summarize(pct = round(mean(pct), 2)) %>%
  pivot_wider(id_cols = c(poll_id, pollster_rating_id, methodology, state, seat_number,
                          sample_size, population_full, cycle, office_type),
              names_from = party,
              values_from = pct) %>%
  #Some polls get values for RV and LV voters, so we are only taking the best
  #from each poll
  mutate(population_full = factor(population_full, levels = population_order)) %>%
  arrange(population_full) %>%
  group_by(poll_id, state, seat_number, cycle, office_type) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-population_full) %>%
  #Combining with pollRatings
  left_join(pollRatings, by = c('cycle' = 'year', 'pollster_rating_id')) %>%
  mutate(valid = ifelse(is.na(valid), FALSE, valid), 
         state = state.abb[match(state, state.name)]) %>%
  arrange(valid, desc(lower_error_diff)) # Arrange rows

all_polls <- cleaned_current %>%
  bind_rows(cleaned_historical) %>%
  mutate(IND = ifelse(is.na(IND), 0, IND), 
         mean_bias = ifelse(is.na(mean_bias), 0, mean_bias))

#Splitting up cleaning polls to get 2 types of averages for each race
#valid_weighted, which looks at the softmax-weighted average for valid pollsters only
#all_unweighted, which doesn't care about weights/validity
poll_averages <- all_polls %>%
  mutate(margin = DEM - REP, 
         phone = str_detect(methodology, "Phone|IVR"), 
         online = str_detect(methodology, "Online|Mail|Email|Text")) %>%
  group_by(state, seat_number, cycle, office_type) %>%
  #Deal with the fact that only valid pollsters should be weighted at all
  mutate(valid = ifelse(is.na(valid), FALSE, valid), 
         weight = ifelse(valid, exp(lower_error_diff), 0) /
           sum(ifelse(valid, exp(lower_error_diff), 0), na.rm = TRUE)) %>%
  summarize(unconvinced_pct = mean(100 - (DEM + REP - IND), na.rm = TRUE),
            valid_weighted_ba = weighted.mean(margin - mean_bias, ifelse(valid, weight, 0), 
                                           na.rm = TRUE), 
            phone_unweighted_ba = mean(ifelse(phone, margin - mean_bias, NA_real_), na.rm = TRUE), 
            online_unweighted_ba = mean(ifelse(online, margin - mean_bias, NA_real_), na.rm = TRUE),
            all_unweighted_ba = mean(margin - mean_bias),
            all_unweighted = mean(margin),
            num_polls = n()) %>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))

# --- This was a previous version of the poll data -- removed so we could focus 
# --- purely on the polling averages

#Final cleaning: Taking the polls and pivoting them to include the top N polls
# cleaned_full <- all_polls %>%
#   select(-IND) %>%
#   left_join(poll_averages, by = c('state', 'seat_number', 
#                                   'cycle', 'office_type')) %>%
#   filter(!is.na(state) & state != "" & state != "US" & cycle %% 2 == 0) %>% 
#   mutate(office_type = str_remove(office_type, "U.S. ")) 
  # group_by(state, seat_number, cycle, office_type) %>%
  # slice_head(n = numberOfPolls) %>%
  # mutate(rank = round(rank(desc(lower_error_diff), ties.method = "random"), 0)) %>%
  # arrange(rank) %>%
  # ungroup() %>%
  # mutate(margin = DEM - REP) %>%
  # pivot_wider(
  #   id_cols = c(state, seat_number, cycle, office_type),
  #   names_from = rank,
  #   values_from = c(margin, methodology),
  #   names_glue = "poll_{rank}_{.value}"
  # ) %>%
  # left_join(poll_averages, by = c('state', 'seat_number', 'cycle',
  #                                 'office_type')) %>%
  # filter(!is.na(state) & state != "" & state != "US" & cycle %% 2 == 0) %>%
  # mutate(office_type = str_remove(office_type, "U.S. ")) 

write.csv(poll_averages, "cleaned_data/AllPolls.csv")
