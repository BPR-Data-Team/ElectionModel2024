library(tidyverse)
library(lubridate)

population_order <- c('lv', 'rv', 'v', 'a')
numberOfPolls <- 5

#Get poll ratings, extract relevant columns
pollRatings <- read.csv("cleaned_data/Pollster Ratings.csv") %>%
  select(c(year, pollster_rating_id, valid, lower_error_diff, mean_bias)) %>%
  mutate(valid = as.logical(valid))

raw_polls <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw_polls.csv")

# Notes from Ariel:
# - don't forget to remove all of the filter statements keeping <21 day old polls in initial office data cleaning
# - the pollRatings df read in above needs to get revamped to not drop past years and merges later in the code need be updated
# - the other things I mentioned over text
# - I can explain the gross if statements if you want me to but that's an annoying error mitigation thing I had to do

uncleaned_polls <- bind_rows(
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv"), 
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv"), 
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/house_polls.csv"), 
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls.csv")
)

cleaned_polls <- uncleaned_polls %>%
  filter(as.Date(Sys.Date()) - as.Date(mdy(end_date)) <= 21) %>%
  group_by(state) %>%
  mutate(num_polls = n_distinct(poll_id)) %>%
  ungroup() %>%
  select(poll_id, pollster_rating_id, methodology, state, seat_number, question_id, 
         sample_size, population_full, cycle, office_type, party, pct, answer, num_polls)

#For each question, we only care about the answer that has the max result:
#For example a question might ask "Biden vs Trump vs I" AND "Biden v Trump", 
#Which is very annoying -- #We take the first
max_pct_sums <- cleaned_polls %>% 
  group_by(poll_id,pollster_rating_id,question_id,state,seat_number) %>% 
  summarize(total_pct = sum(pct)) %>% 
  filter(total_pct == max(total_pct))

#Merging back into the cleaned polls
cleaned_polls <- cleaned_polls %>%
  #Only keeps poll questions with max total responses, by poll
  right_join(max_pct_sums, by = c('poll_id', 'pollster_rating_id', 'question_id', 
                                  'state', 'seat_number')) 

#Eventually we'll use the independent polls, but not now
independent_polls <- cleaned_polls %>% 
  filter(!(party %in% c("DEM", "REP"))) 
  
cleaning_polls <- cleaned_polls %>% 
  filter(party %in% c("DEM", "REP")) %>%
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
  #Combining with poll ratings, and taking the best N (5) polls from each race
  left_join(pollRatings, by = c('cycle' = 'year', 'pollster_rating_id')) %>%
  mutate(valid = ifelse(is.na(valid), FALSE, valid)) %>%
  arrange(valid, desc(lower_error_diff)) # Arrange rows

#Splitting up cleaning polls to get 2 types of averages for each race
#valid_weighted, which looks at the softmax-weighted average for valid pollsters only
#all_unweighted, which doesn't care about weights/validity
poll_averages <- cleaning_polls %>%
  mutate(margin = DEM - REP) %>%
  group_by(state, seat_number, cycle, office_type) %>%
  #Deal with the fact that only valid pollsters should be weighted at all
  mutate(weight = ifelse(valid, exp(lower_error_diff), 0) / 
           sum(ifelse(valid, exp(lower_error_diff), 0), na.rm = TRUE)) %>%
  summarize(unconvinced_pct = mean(100 - (DEM + REP), na.rm = TRUE),
            valid_weighted = weighted.mean(margin, ifelse(valid, weight, 0), na.rm = TRUE), 
            all_unweighted = mean(margin)) %>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .)))


cleaning_polls <- cleaning_polls %>%
  group_by(state, seat_number, cycle, office_type) %>%
  slice_head(n = 5) %>%
  mutate(rank = round(rank(desc(lower_error_diff), ties.method = "random"), 0)) %>%
  arrange(rank) %>%
  ungroup() %>%
  mutate(margin = DEM - REP) %>%
  pivot_wider(
    id_cols = c(state, seat_number, cycle, office_type),
    names_from = rank,
    values_from = c(margin, methodology, population_full),
    names_glue = "poll_{rank}_{.value}"
  ) %>%
  left_join(poll_averages, by = c('state', 'seat_number', 'cycle',
                                  'office_type')) %>%
  filter(state != "") %>%
  mutate(office_type = str_remove(office_type, "U.S. ")) 
