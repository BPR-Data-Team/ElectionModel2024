library(tidycensus)
library(tidyverse)


census_api_key(Sys.getenv("CENSUS_API_KEY"))

#2022 data should be used for 2022, but 2019 data sbould be used for 2020
#year_to_get is what I put into the API, but I use year_to_name in the dataset
clean_acs <- function(year_to_get, year_to_name) {
  acs_data <- bind_rows(
    get_acs(
    geography = "congressional district", 
    variables = variables, 
    year = year_to_get, 
    survey = 'acs1', 
    output = 'wide'), 
    get_acs(
      geography = "state", 
      variables = variables, 
      year = year_to_get, 
      survey = 'acs1', 
      output = 'wide'))
  
  acs_data %>% 
    mutate(NAME = sub("\\(at Large\\)", "1", NAME),
           state = sub(".*,\\s*(.*)", "\\1", NAME), 
           district = sub(".*\\bCongressional District (\\d+).*", "\\1", NAME),
           .before = "NAME") %>%
    select(matches("[^M]$")) %>%
    rename_with(~ sub("(?<![M])E$", "", ., perl = TRUE)) %>%
    filter(state != "Puerto Rico") %>%
    mutate(
      white_pct = white_pop / total, 
      black_pct = black_pop / total, 
      asian_pct = asian_pop / total, 
      hispanic_pct = hispanic / total, 
      impoverished_pct = impoverished_pop / total, 
      college_pct = college_pop / total,
      renting_pct = renting_pop / total,
      year = year_to_name, 
      district = ifelse(district == state | state == "District of Columbia", "0", district),
      district = as.numeric(district), 
      state = case_when(
        state == "District of Columbia" ~ "DC", 
        TRUE ~ state.abb[match(state,state.name)])
    ) %>%
    mutate() %>%
    select(year, state, district, white_pct, black_pct, asian_pct, hispanic_pct, 
           median_income, impoverished_pct, median_age, renting_pct) %>%
    return() 
}

#All the demographic variables we're using: CANNOT get rural/urban stats from
#ACS Census, so have to use another method
variables <- c(
  total = "B01001_001",
  white_pop = "B02001_002", 
  black_pop = "B02001_003", 
  asian_pop = "B02001_005", 
  hispanic = "B03001_003", 
  median_income = "B19326_001",
  impoverished_pop = "B06012_002", 
  median_age = "B01002_001", 
  college_pop = "B06009_005", 
  renting_pop = "B25008_003")

prev_dems <- reduce(
  map2(c(2005, 2007, 2009, 2012, 2013, 2015, 2017, 2019, 2022),
       c(2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022),
       clean_acs), bind_rows)

#ACS 2023 data comes out in September, so we use 2022 data until then
current_dems <- tryCatch(
  clean_acs(2023, 2024), 
  error = function(e) clean_acs(2022, 2024))

all_dems <- bind_rows(prev_dems, current_dems) 

write.csv(all_dems, "cleaned_data/Demographics.csv")



