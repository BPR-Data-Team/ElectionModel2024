library(tidyverse)
library(lubridate)
library(readxl)
all_polls <- read_csv("data/HistoricalPolls.csv")

#HYPERPARAMETERS:
years_to_rate = 10
days_to_rate = 21
max_polls_per_race = 5

#takes in a year, throws out ratings for every pollster prior to that year 
ratingfunction <- function(before_year, polls = all_polls) {
  
  #first, filtering data!
  filtered_data <- polls %>%
    filter(year < before_year & year >= before_year - years_to_rate) %>%
    filter(cand1_party == "DEM" & cand2_party == "REP") %>%
    mutate(dem_margin = (cand1_pct - cand2_pct), 
           actual_margin = (cand1_actual - cand2_actual), 
           electiondate = mdy(electiondate), 
           polldate = mdy(polldate)) %>%
    #only looking at polls taken less than a month before the election
    filter(electiondate - polldate <= days_to_rate)
  
  #Calculating the dispersion factor
  #Dispersion for a single pollster in a single race is calculated as follows:
  #Dispersion = |Predicted dem margin - Average predicted dem margin by all other pollsters|
  
  # Calculate margin statistics for each race for all pollsters
  race_stats <- filtered_data %>%
    group_by(race_id) %>%
    summarise(total_margin = sum(dem_margin), 
              total_error = sum(abs(dem_margin - actual_margin)),
              total_polls = n(), .groups = "drop")
  
  # Calculate margin statistics for each pollster within each race
  pollster_stats <- filtered_data %>%
    group_by(race_id, pollster_rating_id) %>%
    summarise(pollster_margin = sum(dem_margin),
              pollster_error = sum(abs(dem_margin - actual_margin)),
              pollster_num_polls = n(), .groups = "drop") %>%
    left_join(race_stats, by = "race_id") %>%
    # Filter races that have more than one pollster
    filter(total_polls > pollster_num_polls) %>%
    mutate(other_polling_average = (total_margin - pollster_margin) / (total_polls - pollster_num_polls),
           pollster_average = pollster_margin / pollster_num_polls, 
           other_error_average = (total_error - pollster_error) / (total_polls - pollster_num_polls),
           pollster_error_average = pollster_error / pollster_num_polls)
    
  
  # Calculate the average dispersion and relative error for each pollster
  pollster_dispersion <- pollster_stats %>%
    group_by(pollster_rating_id) %>%
    summarise(dispersion = mean(abs(pollster_average - other_polling_average)),
              relative_error = mean(pollster_error_average - other_error_average),
              .groups = "drop")
  
  #getting generic ratings for each pollster
  ratings <- filtered_data %>%
    mutate(moe = 100 * 1.96 * 0.5/sqrt(samplesize)) %>%
    group_by(pollster_rating_id, race_id) %>%
    #getting basic statistics, e.g. what percent of calls were outside the MOE
    summarize(
      avg_cand1 = mean(cand1_pct), 
      avg_cand2 = mean(cand2_pct), 
      cand1_actual = mean(cand1_actual), 
      cand2_actual = mean(cand2_actual),
      num_polls = n(), 
      outside_moe = ((avg_cand1 - avg_cand2) - (cand1_actual - cand2_actual)) > 
        2*mean(moe), .groups = "drop"
    ) %>%
    group_by(pollster_rating_id) %>%
    #getting basic ratings, such as the average bias
    summarize(
      avgbias = mean((avg_cand1 - avg_cand2) - (cand1_actual - cand2_actual)), 
      avg_error = mean(abs((avg_cand1 - avg_cand2) - (cand1_actual - cand2_actual))),
      pct_right = mean(sign(avg_cand1 - avg_cand2) == sign(cand1_actual - cand2_actual)), 
      pct_dem = mean(sign(avg_cand1 - avg_cand2) == 1),
      num_polls = sum(num_polls), 
      num_races = n(), 
      polls_per_race = num_polls / num_races, 
      pct_outside_moe = mean(outside_moe)
    ) %>%
    left_join(pollster_dispersion, by = "pollster_rating_id") %>%
    #we can't do analysis on pollsters that only have one race, so remove them
    filter(num_races > 1)
  
  first_last_poll <- all_polls %>%
    filter(year <= before_year) %>%
    group_by(pollster_rating_id) %>%
    summarize(
      years_since_last_poll = before_year - max(year),
      year_of_first_poll = min(year)
    )
  
  ratings <- ratings %>%
    left_join(first_last_poll, by = "pollster_rating_id") %>%
    left_join(roper_affiliated, by = "pollster_rating_id")
  
  #renaming the columns according to their year, but we want to keep the name pollster so we can join with others
  return(ratings)
  
}

#first, cleaning polls
clean_polls <- all_polls %>%
  separate(type_simple, c("type", "Primary/General")) %>%
  separate(location, c("state", "district")) %>%
  filter(`Primary/General` == "G") %>%
  mutate(days_until = as.integer(mdy(electiondate) - mdy(polldate))) %>% 
  select(race_id, year, state, district, type, pollster_rating_id, methodology, partisan, days_until, electiondate, samplesize, cand1_party, 
         cand2_party, cand1_pct, cand2_pct, cand1_actual, cand2_actual, error, bias, rightcall) %>%
  group_by(race_id) %>%
  mutate(bias = (cand1_pct - cand2_pct) - (cand1_actual - cand2_actual))

#This is the major function: for every year, adds ratings to every poll 
#corresponding to that pollster's rating for all previous years
years <- c(2010, 2012, 2014, 2016, 2018, 2020, 2022)
polls_with_ratings <- map(years, function (y) { 
  clean_polls %>%
    filter(year > y & year <= y + 2) %>%
    left_join(ratingfunction(y), by = "pollster_rating_id")
})

#getting all ratings
polls_with_ratings <- reduce(polls_with_ratings, bind_rows) %>%
  #Make a column that says whether this pollster had ratings
  mutate(had_ratings = !is.na(avgbias)) %>%
  group_by(year) %>%
  #for polls that don't have ratings, replace ratings with the average ratings of that year
  mutate(across(avgbias:year_of_first_poll, .fns = ~ ifelse(is.na(.), mean(., na.rm = TRUE), .))) %>%
  ungroup()

#not important -- in case I want to look at ratings for specific years
ratings_pre_2010 <- ratingfunction(2010)
ratings_pre_2012 <- ratingfunction(2012)
ratings_pre_2014 <- ratingfunction(2014)
ratings_pre_2016 <- ratingfunction(2016)
ratings_pre_2018 <- ratingfunction(2018)
ratings_pre_2020 <- ratingfunction(2020) %>%
  full_join(pollster_to_id, by = "pollster_rating_id")




