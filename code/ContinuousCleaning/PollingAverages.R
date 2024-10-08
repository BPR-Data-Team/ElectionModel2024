library(tidyverse)
library(lubridate)
library(stringr)
library(meta)
library(metafor)

days_counting <- 50
population_order <- c('lv', 'rv', 'v', 'a')

#Get poll ratings, extract relevant columns
pollRatings <- read.csv("cleaned_data/Pollster Ratings.csv") %>%
  select(c(year, pollster_rating_id, valid, lower_error_diff)) %>%
  mutate(valid = as.logical(valid))

raw_polls <- read.csv("https://raw.githubusercontent.com/fivethirtyeight/data/master/pollster-ratings/raw_polls.csv")# pivot_longer(cols = c(cand1_party, cand2_party),

reverse_logit <- function(num) {
  return (exp(num) / (1 + exp(num)))
}

#Getting softmax weights for only valid polls
valid_softmax <- function(nums, valid) {
  expsums <- ifelse(valid, exp(nums), 0) / sum(ifelse(valid, exp(nums), 0), na.rm = TRUE)
  #weights
  expsums <- ifelse(is.na(expsums), 0, expsums)
  return (expsums)
}

#Performing a meta_analysis on polls, returning a dataset that is later unnested
#Much better than simply averaging polls because this accounts for 
#Heterogeneity between polls
perform_weighted_metaprop <- function(num_democrat, samplesizes, weights) {
  
  
  # Perform the meta-analysis for UNWEIGHTED
  unweighted_meta <- metaprop(event = num_democrat, 
                              n = samplesizes, 
                              sm = "PLO", 
                              method.tau = "DL", 
                              method.ci = "NAsm")
  
  unweighted_estimate <- reverse_logit(unweighted_meta$TE.random)
  unweighted_ci_lower <- reverse_logit(unweighted_meta$lower.random)
  unweighted_ci_upper <- reverse_logit(unweighted_meta$upper.random)
  
  #Perform the weighted meta-analysis
  #When there are no valid pollsters, this FAILS
  #So we do a tryCatch to work with it
  weighted_estimate <- NA
  weighted_ci_lower <- NA
  weighted_ci_upper <- NA
  
  tryCatch({
    logit_event_rate <- log(num_democrat / (samplesizes - num_democrat))
    se_logit_event_rate <- sqrt(1 / num_democrat + 1 / (samplesizes - num_democrat))
    
   
    weighted_meta <- rma(yi = logit_event_rate,
                         sei = se_logit_event_rate,
                         weights = weights,
                         method = "REML")
    
    #Change the weighted estimates if there ARE estimates
    weighted_estimate <- reverse_logit(weighted_meta$beta[, 1])
    weighted_ci_lower <- reverse_logit(weighted_meta$ci.lb)
    weighted_ci_upper <- reverse_logit(weighted_meta$ci.ub)
  }, error = function(e) {
  })
  
  # Return the results as a tibble
  return(tibble(
    unweighted_estimate = 200 * (unweighted_estimate - 0.5),
    unweighted_ci_lower = 200 * (unweighted_ci_lower - 0.5),
    unweighted_ci_upper = 200 * (unweighted_ci_upper - 0.5),
    weighted_estimate = 200 * (weighted_estimate - 0.5),
    weighted_ci_lower = 200 * (weighted_ci_lower - 0.5),
    weighted_ci_upper = 200 * (weighted_ci_upper - 0.5)
  ))
}


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
  filter(!str_detect(type_simple, "-P")) %>%
  mutate(office_type = case_when(
    str_detect(type_simple, "Sen") ~ "Senate", 
    str_detect(type_simple, "Gov") ~ "Governor",
    str_detect(type_simple, "Pres") ~ "President",
    str_detect(type_simple, "House") ~ "House"),
    office_type = ifelse(office_type == "President" & state == "US", 
                         "House", office_type)) %>%
  left_join(pollRatings, by = c("cycle" = 'year', 'pollster_rating_id')) %>%
  rename(sample_size = samplesize) %>%
  select(poll_id, pollster_rating_id, methodology, state, seat_number, sample_size, 
         cycle, office_type, DEM, REP, valid, lower_error_diff)

uncleaned_current_genballot <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/generic_ballot_polls.csv") %>% 
  pivot_longer(cols = c(dem, rep), names_to = "party", values_to = "pct") %>% 
  mutate(party = toupper(party), 
         replace_na(seat_number, 0))

#Get current polls from online, 538 stream
uncleaned_current <- bind_rows(
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv"), 
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv"), 
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/house_polls.csv"), 
  read.csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls.csv"),
  uncleaned_current_genballot
)

  

#Initial cleaning
cleaned_current <- uncleaned_current %>%
  filter(as.Date(Sys.Date()) - as.Date(mdy(end_date)) <= days_counting & population_full == "lv" & 
           (office_type != "U.S. President" | (as.Date(mdy(start_date)) > as.Date("2024-07-21")))) %>%
  #Fixing problems with ME/NE where CD is not counted as a seat number, Same with PR
  mutate(seat_number = case_when(
    str_detect(state, "CD-1") ~ 1, 
    str_detect(state, "CD-2") ~ 2, 
    TRUE ~ seat_number
  ), 
  state = str_remove(state, " CD-[0-9]")) %>%
  filter(state %in% c("", state.name) | is.na(state)) %>%
  group_by(question_id) %>%
  filter(office_type != "U.S. President" | (!any(answer == "Biden") & !(any(answer == "Kennedy")) & any(answer == "Harris") & any(answer == "Trump"))) %>%
  ungroup() %>%
  select(poll_id, pollster_rating_id, methodology, state, seat_number, question_id, 
         sample_size, population_full, cycle, office_type, party, pct, answer)

 #For each question, we only care about the answer that has the max result:
#For example a question might ask "Harris vs Trump vs I" AND "Harris v Trump", 
#Which is very annoying -- #We take the first
max_pct_sums <- cleaned_current %>% 
  group_by(poll_id,pollster_rating_id,question_id,state,seat_number, office_type) %>% 
  summarize(total_pct = sum(pct)) %>% 
  filter(total_pct == max(total_pct))

#Merging back into the cleaned polls
cleaned_current <- cleaned_current %>%
  #Only keeps poll questions with max total responses, by poll
  right_join(max_pct_sums, by = c('poll_id', 'pollster_rating_id', 'question_id', 
                                  'state', 'seat_number', 'office_type')) 

#Eventually we'll use the independent polls, but not now
independent_polls <- cleaned_current %>% 
  filter(!(party %in% c("DEM", "REP"))) 
  
cleaned_current <- cleaned_current %>% 
  mutate(party = ifelse(party %in% c("DEM", "REP"), party, "IND")) %>%
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
  mutate(population_full = factor(population_full, levels = population_order)) %>%
  arrange(population_full) %>%
  group_by(poll_id, state, seat_number, cycle, office_type) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  select(-population_full) %>%
  #Combining with pollRatings
  left_join(pollRatings, by = c('cycle' = 'year', 'pollster_rating_id')) %>%
  mutate(valid = ifelse(is.na(valid), FALSE, valid), 
         state = state.abb[match(state, state.name)], 
         state = ifelse(is.na(state), "US", state), 
         office_type = ifelse(state == "US", "U.S. President", office_type)) %>%
  arrange(valid, desc(lower_error_diff)) # Arrange rows

all_polls <- cleaned_current %>%
  bind_rows(cleaned_historical)

#Splitting up cleaning polls to get 2 types of averages for each race
#valid_weighted, which looks at the softmax-weighted average for valid pollsters only
#all_unweighted, which doesn't care about weights/validity
poll_averages <- all_polls %>%
  mutate(margin = (DEM - REP),
         phone = str_detect(methodology, "Phone|IVR"),
         online = str_detect(methodology, "Online|Mail|Email|Text")) %>%
  group_by(state, seat_number, cycle, office_type) %>%
  #Deal with the fact that only valid pollsters should be weighted at all
  mutate(valid = ifelse(is.na(valid), FALSE, valid),
         #Softmaxing weights
         weight = valid_softmax(lower_error_diff, valid),
         #Num-democrats is required for meta-analyses -- #people who said they'd vote DEM
         #REALLY important to put DEM tp here, because many polls have significant unconvinced
         num_democrat = round(sample_size * (DEM) / (DEM + REP)))

#Conducting meta-analyses on the polling averages
meta_analyses <- poll_averages %>%
  nest() %>%
  summarize(meta_results = map(data, ~perform_weighted_metaprop(.$num_democrat, .$sample_size, .$weight))) %>%
  unnest(cols = meta_results)

full_poll_averages <- poll_averages %>%
 summarize(phone_unweighted = mean(ifelse(phone, margin, NA_real_), na.rm = TRUE),
            online_unweighted = mean(ifelse(online, margin, NA_real_), na.rm = TRUE),
            num_polls = n()) %>%
  mutate(across(everything(), ~ifelse(is.nan(.), NA, .))) %>%
  left_join(meta_analyses, by = c("state", 'seat_number', 'cycle', 'office_type')) %>%
  rename(year = cycle)

#Generic ballot based on only actual genballot polls
generic_polling <- full_poll_averages %>%
  ungroup() %>%
  filter(state == "US" & year > 2000) %>%
  rename(weighted_genpoll = weighted_estimate,
         weighted_genpoll_lower = weighted_ci_lower,
         weighted_genpoll_upper = weighted_ci_upper,
         unweighted_genpoll = unweighted_estimate) %>%
  select(c('year', 'weighted_genpoll','weighted_genpoll_lower',
           'weighted_genpoll_upper', 'unweighted_genpoll'))
#   
# 
write.csv(full_poll_averages, "cleaned_data/AllPolls.csv")
write.csv(generic_polling, "cleaned_data/GenPolling.csv")
