library(tidyverse)
library(lubridate)
library(glue)
library(tidycensus)

#Fred data is very similar, so the same function can be used to clean a lot of it
clean_fred_data <- function(dataset) {
  election_years <- seq(2002, 2024, by=2)
  cleaned_dataset <- dataset %>% mutate(date = as.Date(date), 
                                value = as.numeric(value))
  
  #Function that takes a year and gets the 4 most recent 
  #values prior to the election, then turns them into previous/current results
  process_year <- function(year) {
    cleaned_dataset %>% 
      filter(date < as.Date(glue("{year}-11-01"))) %>%
      tail(4) %>%
      summarize(year = year, 
                previous = mean(nth(value, 1), nth(value, 2)), 
                current = mean(nth(value, 3), nth(value, 4)), 
                change = current - previous) %>%
      return()
  }
  
  map(election_years, process_year) %>%
    reduce(rbind) %>%
    return()
}

# -- Cleaing FRED Data ----
cci <- read.csv("data/consumer-sentiment.csv") %>% 
  clean_fred_data() 

unemployment <- read.csv("data/national-unemployment-rate.csv") %>% 
  clean_fred_data()

real_disposable <- read.csv("data/real-disposable-personal-income.csv") %>%
  clean_fred_data()

real_gdp_per_capita <- read.csv("data/real-gdp-per-capita.csv") %>%
  clean_fred_data()

national_gas_price <- read.csv("data/national-regular-gas-price.csv") %>%
  clean_fred_data()


write.csv(cci, "cleaned_data/Consumer Confidence Index.csv")
write.csv(unemployment, "cleaned_data/Unemployment.csv")
write.csv(real_disposable, "cleaned_data/Real Disposable PI.csv")
write.csv(real_gdp_per_capita, "cleaned_data/Real GDP Per Capita.csv")
write.csv(national_gas_price, "cleaned_data/Gas Prices.csv")
