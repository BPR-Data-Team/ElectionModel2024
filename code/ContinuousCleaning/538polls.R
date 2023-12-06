library(tidyverse)
library(lubridate)
grade_order <- c("A+","A","A-","A/B","B+","B","B-","B/C","C+","C","C-","C/D","D+","D","D-","D/F","F+,F","F-")
numberOfPolls <- 5
# PRESIDENTIAL ----------------------------------

#read in data from 538's github
uncleaned_pres <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv")
# return the mean of polls giving a high and low percentage for candidates in each question and filter for the last 21 days
pres_cleaned <- uncleaned_pres %>%
  group_by(poll_id,pollster_id,pollster,fte_grade,start_date,end_date,question_id,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state,party,answer,candidate_name) %>% 
  summarise(pct = mean(pct)) %>% filter(as.Date(Sys.Date()) - as.Date(mdy(end_date)) <= 21)
# pulls number of polls in the last 21 days
num_polls <- n_distinct(pres_cleaned$poll_id)
# gets the total percent for each question and keep questions with the highest total percent for each poll
max_pct_sums <- pres_cleaned %>% group_by(poll_id,pollster_id,question_id,fte_grade) %>% summarize(total_pct = sum(pct)) %>% 
  group_by(poll_id,pollster_id,fte_grade) %>% filter(total_pct == max(total_pct))
# order by poll grade, then total percent; keep the top 5 polls
max_pct_sums$fte_grade <- factor(max_pct_sums$fte_grade, levels = grade_order, ordered = TRUE)
max_pct_sums <- max_pct_sums %>% arrange(fte_grade, desc(total_pct)) %>% ungroup() %>% filter(row_number() <= numberOfPolls)
pres_cleaned <- max_pct_sums %>% left_join(pres_cleaned, by = c("poll_id", "question_id"))
# keep the top party candidate for each question/poll
pres_cleaned <- pres_cleaned %>% group_by(poll_id, party) %>% filter(pct == max(pct)) %>% ungroup()
#pres_cleaned <- pres_cleaned %>% group_by(poll_id,pollster_id.x,question_id,fte_grade.x,pollster,sample_size,pollster_rating_id,methodology,cycle,seat_number,partisan,state) %>% 
pres_cleaned <- pres_cleaned %>%
  pivot_wider(
    id_cols = c("poll_id", "pollster_id.x", "fte_grade.x", "total_pct", "pollster_id.y", "pollster", "fte_grade.y", "sample_size", "pollster_rating_id", "methodology", "partisan", "cycle", "seat_number", "state"),
    names_from = party,
    values_from = pct)
pres_cleaned <- pres_cleaned %>% subset(select = c(pollster,fte_grade.x,methodology,partisan,DEM,REP,IND,cycle,state,seat_number))
colnames(pres_cleaned) <- c("pollster","grade","methodology","pollsterParty","DEM","REP","IND","cycle","state","district")
pres_cleaned$grade <- as.character(pres_cleaned$grade)

# add identifiers
pres_cleaned <- pres_cleaned %>% mutate("state" = "United States",
                                        "district" = 0,
                                        "isMidterm" = FALSE,
                                        "isSenate" = FALSE,
                                        "isHouse" = FALSE,
                                        "isGubernatorial" = FALSE,
                                        "isPresidential" = TRUE,
                                        "numPolls" = num_polls)

# SENATE -----------------------------------------
uncleaned_sen <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv")
sen_cleaned <- uncleaned_sen %>%
  group_by(poll_id,pollster_id,pollster,fte_grade,start_date,end_date,question_id,sample_size,cycle,seat_number,state,party,answer,candidate_name) %>% 
  summarise(pct = mean(pct))

# TODO: Determine which pollsters and from when

sen_cleaned <- sen_cleaned %>% mutate("District" = 0,
                                        "isMidterm" = FALSE,
                                        "isSenate" = TRUE,
                                        "isHouse" = FALSE,
                                        "isGubernatorial" = FALSE,
                                        "isPresidential" = FALSE)
# SENATE DOS -------------------------

uncleaned_sen <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv")
# return the mean of polls giving a high and low percentage for candidates in each question and filter for the last 21 days
sen_cleaned <- uncleaned_sen %>%
  group_by(poll_id,pollster_id,pollster,fte_grade,start_date,end_date,question_id,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state,party,answer,candidate_name) %>% 
  summarise(pct = mean(pct)) %>% filter(as.Date(Sys.Date()) - as.Date(mdy(end_date)) <= 21)
# pulls number of polls in the last 21 days
num_polls_Sen <- n_distinct(pres_cleaned$poll_id)
# gets the total percent for each question and keep questions with the highest total percent for each poll
max_pct_sums <- pres_cleaned %>% group_by(poll_id,pollster_id,question_id,fte_grade) %>% summarize(total_pct = sum(pct)) %>% 
  group_by(poll_id,pollster_id,fte_grade) %>% filter(total_pct == max(total_pct))
# order by poll grade, then total percent; keep the top 5 polls
max_pct_sums$fte_grade <- factor(max_pct_sums$fte_grade, levels = grade_order, ordered = TRUE)
max_pct_sums <- max_pct_sums %>% arrange(fte_grade, desc(total_pct)) %>% ungroup() %>% filter(row_number() <= 5)
pres_cleaned <- max_pct_sums %>% left_join(pres_cleaned, by = c("poll_id", "question_id"))
# keep the top party candidate for each question/poll
pres_cleaned <- pres_cleaned %>% group_by(poll_id, party) %>% filter(pct == max(pct)) %>% ungroup()
#pres_cleaned <- pres_cleaned %>% group_by(poll_id,pollster_id.x,question_id,fte_grade.x,pollster,sample_size,pollster_rating_id,methodology,cycle,seat_number,partisan,state) %>% 
pres_cleaned <- pres_cleaned %>%
  pivot_wider(
    id_cols = c("poll_id", "pollster_id.x", "fte_grade.x", "total_pct", "pollster_id.y", "pollster", "fte_grade.y", "sample_size", "pollster_rating_id", "methodology", "partisan", "cycle", "seat_number", "state"),
    names_from = party,
    values_from = pct)
pres_cleaned <- pres_cleaned %>% subset(select = c(pollster,fte_grade.x,methodology,partisan,DEM,REP,IND,cycle,state,seat_number))
colnames(pres_cleaned) <- c("pollster","grade","methodology","pollsterParty","DEM","REP","IND","cycle","state","district")
pres_cleaned$grade <- as.character(pres_cleaned$grade)

# add identifiers
pres_cleaned <- pres_cleaned %>% mutate("state" = "United States",
                                        "district" = 0,
                                        "isMidterm" = FALSE,
                                        "isSenate" = FALSE,
                                        "isHouse" = FALSE,
                                        "isGubernatorial" = FALSE,
                                        "isPresidential" = TRUE,
                                        "numPolls" = num_polls)


# HOUSE -----------------------------------------
uncleaned_house <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/house_polls.csv")
house_cleaned <- uncleaned_house %>%
  group_by(poll_id,pollster_id,pollster,fte_grade,start_date,end_date,question_id,sample_size,cycle,seat_number,state,party,answer,candidate_name) %>% 
  summarise(pct = mean(pct))

# TODO: Determine which pollsters and from when

house_cleaned <- house_cleaned %>% mutate("District" = seat_number,
                                      "isMidterm" = FALSE,
                                      "isSenate" = FALSE,
                                      "isHouse" = TRUE,
                                      "isGubernatorial" = FALSE,
                                      "isPresidential" = FALSE)

# GUBERNATORIAL -----------------------------------------
uncleaned_gub <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls.csv")
gub_cleaned <- uncleaned_gub %>%
  group_by(poll_id,pollster_id,pollster,fte_grade,start_date,end_date,question_id,sample_size,cycle,seat_number,state,party,answer,candidate_name) %>% 
  summarise(pct = mean(pct))

# TODO: Determine which pollsters and from when

gub_cleaned <- gub_cleaned %>% mutate("District" = 0,
                                          "isMidterm" = FALSE,
                                          "isSenate" = FALSE,
                                          "isHouse" = FALSE,
                                          "isGubernatorial" = TRUE,
                                          "isPresidential" = FALSE)

# HISTORICAL -----------------------------------------
uncleaned_senH <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls_historical.csv")
senH_cleaned <- uncleaned_senH %>%
  group_by(poll_id,pollster_id,pollster,fte_grade,start_date,end_date,question_id,sample_size,cycle,seat_number,state,party,answer,candidate_name) %>% 
  summarise(pct = mean(pct))

# TODO: Determine which pollsters and from when

senH_cleaned <- senH_cleaned %>% mutate("District" = 0,
                                      "isMidterm" = FALSE,
                                      "isSenate" = FALSE,
                                      "isHouse" = FALSE,
                                      "isGubernatorial" = TRUE,
                                      "isPresidential" = FALSE)