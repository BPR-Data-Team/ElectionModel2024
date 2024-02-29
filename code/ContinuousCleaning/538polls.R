library(tidyverse)
library(lubridate)
grade_order <- c("A+","A","A-","A/B","B+","B","B-","B/C","C+","C","C-","C/D","D+","D","D-","D/F","F+,F","F-")
bool_order <- c("True","False")
numberOfPolls <- 5
pollRatings <- read.csv("cleaned_data/Pollster Ratings.csv")
# take most recent poll rating, extract relevant columns
pollRatings <- pollRatings %>% group_by(pollster_rating_id) %>% slice_max(year, n=1) %>% slice_max(X, n=1) %>% ungroup() %>% subset(select = c(pollster_rating_id, valid, lower_error_diff)) 
# making valid column a factor for later sorting
pollRatings$valid <- factor(pollRatings$valid, levels = bool_order, ordered = TRUE)
# PRESIDENTIAL ----------------------------------

#read in data from 538's github
uncleaned_pres <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/president_polls.csv") %>% mutate(
  state = case_when(state == "Maine CD-1" ~ "Maine", state == "Maine CD-2" ~ "Maine", TRUE ~ state)
)
# return the mean of polls giving a high and low percentage for candidates in each question and filter for the last 21 days
pres_cleaned <- uncleaned_pres %>%
  group_by(poll_id,pollster_id,pollster,fte_grade,start_date,end_date,question_id,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state,party,answer,candidate_name) %>% 
  summarise(pct = mean(pct)) %>% filter(as.Date(Sys.Date()) - as.Date(mdy(end_date)) <= 21)
# pulls number of polls in the last 21 days
pres_cleaned <- pres_cleaned %>% group_by(state) %>% mutate(statePolls =  n_distinct(poll_id)) %>%  ungroup()
# gets the total percent for each question and keep questions with the highest total percent for each poll
max_pct_sums <- pres_cleaned %>% group_by(poll_id,pollster_id,question_id,fte_grade,state) %>% summarize(total_pct = sum(pct)) %>% 
  group_by(poll_id,pollster_id,fte_grade,state) %>% filter(total_pct == max(total_pct))
# make grade into factor
max_pct_sums$fte_grade <- factor(max_pct_sums$fte_grade, levels = grade_order, ordered = TRUE)
pres_cleaned <- max_pct_sums %>% left_join(pres_cleaned, by = c("poll_id", "question_id"))
# keep the top party candidate for each question/poll
# pres_cleaned <- pres_cleaned %>% group_by(poll_id, party) %>% filter(pct == max(pct)) %>% ungroup() %>% 
#   mutate(party = case_when(
#   party %in% c("DEM", "REP") ~ party, 
#   TRUE ~ "IND"
# ))
# sum %s for same party candidates
pres_cleaned <- pres_cleaned %>% mutate(party = case_when(party %in% c("DEM", "REP") ~ party,TRUE ~ "IND")) %>% filter(party != "IND") %>% 
  group_by(poll_id,pollster_id.x,question_id,fte_grade.x,state.x,total_pct,pollster_id.y,pollster,fte_grade.y,start_date,end_date,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state.y,party,statePolls) %>% 
  summarise(pct = sum(pct)) %>% group_by(poll_id,pollster_id.x,fte_grade.x,state.x,total_pct,pollster_id.y,pollster,fte_grade.y,start_date,end_date,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state.y,party,statePolls) %>% 
  summarise(pct = median(pct))
#pres_cleaned <- pres_cleaned %>% group_by(poll_id,pollster_id.x,question_id,fte_grade.x,pollster,sample_size,pollster_rating_id,methodology,cycle,seat_number,partisan,state) %>% 
# combine each party pct per poll into one row
# pres_cleaned <- pres_cleaned %>% group_by(poll_id,party) %>% slice(which.max(pct)) %>% ungroup() %>% 
#   pivot_wider(
#     id_cols = c("poll_id", "pollster_id.x", "state.x","fte_grade.x", "total_pct", "pollster_id.y", "pollster", "fte_grade.y", "sample_size", "pollster_rating_id", "methodology", "partisan", "cycle", "seat_number"),
#     names_from = party,
#     values_from = pct)

# pivot, no row select
pres_cleaned <- pres_cleaned %>% 
  pivot_wider(
    id_cols = c("poll_id", "pollster_id.x", "state.x","fte_grade.x", "total_pct", "pollster_id.y", "pollster", "fte_grade.y", "sample_size", "pollster_rating_id", "methodology", "partisan", "cycle", "seat_number"),
    names_from = party,
    values_from = pct)


# merging rating data into cleaned poll data
pres_cleaned <- left_join(pres_cleaned,pollRatings,by = "pollster_rating_id")


# ranking rows by grade and total percent, keeping top 5 polls per state, and eliminating columns
pres_cleaned <- pres_cleaned %>% arrange(valid, desc(lower_error_diff)) %>% ungroup() %>% group_by(state.x) %>%  filter(row_number() <= numberOfPolls) %>% ungroup() %>% subset(select = c(pollster,valid,lower_error_diff,methodology,partisan,DEM,REP,cycle,state.x,seat_number))
# renaming columns
colnames(pres_cleaned) <- c("pollster","valid","lower_error_diff","methodology","pollsterParty","DEM","REP","cycle","state","district")
#pres_cleaned$grade <- as.character(pres_cleaned$grade)
# replacing empty cells w NA
pres_cleaned[pres_cleaned == ""] <- NA
# Each state top 5 polls
pres_cleaned <- pres_cleaned %>% group_by(state,district,cycle) %>% summarise(
  "numPolls" = n(),
  "pollster1.name" = pollster[1],
  "pollster1.valid" = valid[1],
  "pollster1.led" = lower_error_diff[1],
  "pollster1.methodology" = methodology[1],
  "pollster1.partybias" = pollsterParty[1],
  "pollster1.dem" = DEM[1],
  "pollster1.rep" = REP[1],
  "pollster2.name" = pollster[2],
  "pollster2.valid" = valid[2],
  "pollster2.led" = lower_error_diff[2],
  "pollster2.methodology" = methodology[2],
  "pollster2.partybias" = pollsterParty[2],
  "pollster2.dem" = DEM[2],
  "pollster2.rep" = REP[2],
  "pollster3.name" = pollster[3],
  "pollster3.valid" = valid[3],
  "pollster3.led" = lower_error_diff[3],
  "pollster3.methodology" = methodology[3],
  "pollster3.partybias" = pollsterParty[3],
  "pollster3.dem" = DEM[3],
  "pollster3.rep" = REP[3],
  "pollster4.name" = pollster[4],
  "pollster4.valid" = valid[4],
  "pollster4.led" = lower_error_diff[4],
  "pollster4.methodology" = methodology[4],
  "pollster4.partybias" = pollsterParty[4],
  "pollster4.dem" = DEM[4],
  "pollster4.rep" = REP[4],
  "pollster5.name" = pollster[5],
  "pollster5.valid" = valid[5],
  "pollster5.led" = lower_error_diff[5],
  "pollster5.methodology" = methodology[5],
  "pollster5.partybias" = pollsterParty[5],
  "pollster5.dem" = DEM[5],
  "pollster5.rep" = REP[5]
  ) 
# add identifiers
pres_cleaned <- pres_cleaned %>% mutate("district" = 0,
                                        "isMidterm" = FALSE,
                                        "office_type" = "President")

# SENATE -----------------------------------------
uncleaned_sen <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/senate_polls.csv")
# return the mean of polls giving a high and low percentage for candidates in each question and filter for the last 21 days
sen_cleaned <- uncleaned_sen %>%
  group_by(poll_id,pollster_id,pollster,fte_grade,start_date,end_date,question_id,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state,party,answer,candidate_name) %>% 
  summarise(pct = mean(pct)) %>% filter(as.Date(Sys.Date()) - as.Date(mdy(end_date)) <= 21)
# pulls number of polls in the last 21 days
sen_cleaned <- sen_cleaned %>% group_by(state) %>% mutate(statePolls =  n_distinct(poll_id)) %>%  ungroup()
# gets the total percent for each question and keep questions with the highest total percent for each poll
max_pct_sums_sen <- sen_cleaned %>% group_by(poll_id,pollster_id,question_id,fte_grade,state) %>% summarize(total_pct = sum(pct)) %>% 
  group_by(poll_id,pollster_id,fte_grade,state) %>% filter(total_pct == max(total_pct))
# make grade into factor
max_pct_sums_sen$fte_grade <- factor(max_pct_sums_sen$fte_grade, levels = grade_order, ordered = TRUE)
sen_cleaned <- max_pct_sums_sen %>% left_join(sen_cleaned, by = c("poll_id", "question_id"))
# keep the top party candidate for each question/poll
# pres_cleaned <- pres_cleaned %>% group_by(poll_id, party) %>% filter(pct == max(pct)) %>% ungroup() %>% 
#   mutate(party = case_when(
#   party %in% c("DEM", "REP") ~ party, 
#   TRUE ~ "IND"
# ))
# sum %s for same party candidates
sen_cleaned <- sen_cleaned %>% mutate(party = case_when(party %in% c("DEM", "REP") ~ party,TRUE ~ "IND")) %>% filter(party != "IND") %>% 
  group_by(poll_id,pollster_id.x,question_id,fte_grade.x,state.x,total_pct,pollster_id.y,pollster,fte_grade.y,start_date,end_date,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state.y,party,statePolls) %>% 
  summarise(pct = sum(pct)) %>% group_by(poll_id,pollster_id.x,fte_grade.x,state.x,total_pct,pollster_id.y,pollster,fte_grade.y,start_date,end_date,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state.y,party,statePolls) %>% 
  summarise(pct = median(pct))
#pres_cleaned <- pres_cleaned %>% group_by(poll_id,pollster_id.x,question_id,fte_grade.x,pollster,sample_size,pollster_rating_id,methodology,cycle,seat_number,partisan,state) %>% 
# combine each party pct per poll into one row
# pres_cleaned <- pres_cleaned %>% group_by(poll_id,party) %>% slice(which.max(pct)) %>% ungroup() %>% 
#   pivot_wider(
#     id_cols = c("poll_id", "pollster_id.x", "state.x","fte_grade.x", "total_pct", "pollster_id.y", "pollster", "fte_grade.y", "sample_size", "pollster_rating_id", "methodology", "partisan", "cycle", "seat_number"),
#     names_from = party,
#     values_from = pct)

# pivot, no row select
sen_cleaned <- sen_cleaned %>% 
  pivot_wider(
    id_cols = c("poll_id", "pollster_id.x", "state.x","fte_grade.x", "total_pct", "pollster_id.y", "pollster", "fte_grade.y", "sample_size", "pollster_rating_id", "methodology", "partisan", "cycle", "seat_number"),
    names_from = party,
    values_from = pct)

# merging rating data into cleaned poll data
sen_cleaned <- left_join(sen_cleaned,pollRatings,by = "pollster_rating_id")


# ranking rows by grade and total percent, keeping top 5 polls per state, and eliminating columns
sen_cleaned <- sen_cleaned %>% arrange(valid, desc(lower_error_diff)) %>% ungroup() %>% group_by(state.x) %>%  filter(row_number() <= numberOfPolls) %>% ungroup() %>% subset(select = c(pollster,valid,lower_error_diff,methodology,partisan,DEM,REP,cycle,state.x,seat_number))
# renaming columns
colnames(sen_cleaned) <- c("pollster","valid","lower_error_diff","methodology","pollsterParty","DEM","REP","cycle","state","district")
#sen_cleaned$grade <- as.character(pres_cleaned$grade)
# replacing empty cells w NA
sen_cleaned[sen_cleaned == ""] <- NA
# Each state top 5 polls
sen_cleaned <- sen_cleaned %>% group_by(state,district,cycle) %>% summarise(
  "numPolls" = n(),
  "pollster1.name" = pollster[1],
  "pollster1.valid" = valid[1],
  "pollster1.led" = lower_error_diff[1],
  "pollster1.methodology" = methodology[1],
  "pollster1.partybias" = pollsterParty[1],
  "pollster1.dem" = DEM[1],
  "pollster1.rep" = REP[1],
  "pollster2.name" = pollster[2],
  "pollster2.valid" = valid[2],
  "pollster2.led" = lower_error_diff[2],
  "pollster2.methodology" = methodology[2],
  "pollster2.partybias" = pollsterParty[2],
  "pollster2.dem" = DEM[2],
  "pollster2.rep" = REP[2],
  "pollster3.name" = pollster[3],
  "pollster3.valid" = valid[3],
  "pollster3.led" = lower_error_diff[3],
  "pollster3.methodology" = methodology[3],
  "pollster3.partybias" = pollsterParty[3],
  "pollster3.dem" = DEM[3],
  "pollster3.rep" = REP[3],
  "pollster4.name" = pollster[4],
  "pollster4.valid" = valid[4],
  "pollster4.led" = lower_error_diff[4],
  "pollster4.methodology" = methodology[4],
  "pollster4.partybias" = pollsterParty[4],
  "pollster4.dem" = DEM[4],
  "pollster4.rep" = REP[4],
  "pollster5.name" = pollster[5],
  "pollster5.valid" = valid[5],
  "pollster5.led" = lower_error_diff[5],
  "pollster5.methodology" = methodology[5],
  "pollster5.partybias" = pollsterParty[5],
  "pollster5.dem" = DEM[5],
  "pollster5.rep" = REP[5]
) 
# add identifiers
sen_cleaned <- sen_cleaned %>% mutate(
                                        "isMidterm" = FALSE,
                                        "office_type" = "Senate")

# HOUSE -----------------------------------------
uncleaned_house <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/house_polls.csv")
house_cleaned <- uncleaned_house %>%
  group_by(poll_id,pollster_id,pollster,fte_grade,start_date,end_date,question_id,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state,party,answer,candidate_name) %>% 
  summarise(pct = mean(pct)) %>% filter(as.Date(Sys.Date()) - as.Date(mdy(end_date)) <= 21)
# pulls number of polls in the last 21 days
house_cleaned <- house_cleaned %>% group_by(state) %>% mutate(statePolls =  n_distinct(poll_id)) %>%  ungroup()
# gets the total percent for each question and keep questions with the highest total percent for each poll
max_pct_sums_house <- house_cleaned %>% group_by(poll_id,pollster_id,question_id,fte_grade,state) %>% summarize(total_pct = sum(pct)) %>% 
  group_by(poll_id,pollster_id,fte_grade,state) %>% filter(total_pct == max(total_pct))
# make grade into factor
max_pct_sums_house$fte_grade <- factor(max_pct_sums_house$fte_grade, levels = grade_order, ordered = TRUE)
house_cleaned <- max_pct_sums_house %>% left_join(house_cleaned, by = c("poll_id", "question_id"))
# keep the top party candidate for each question/poll
# pres_cleaned <- pres_cleaned %>% group_by(poll_id, party) %>% filter(pct == max(pct)) %>% ungroup() %>% 
#   mutate(party = case_when(
#   party %in% c("DEM", "REP") ~ party, 
#   TRUE ~ "IND"
# ))
# sum %s for same party candidates
house_cleaned <- house_cleaned %>% mutate(party = case_when(party %in% c("DEM", "REP") ~ party,TRUE ~ "IND")) %>% filter(party != "IND") %>% 
  group_by(poll_id,pollster_id.x,question_id,fte_grade.x,state.x,total_pct,pollster_id.y,pollster,fte_grade.y,start_date,end_date,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state.y,party,statePolls) %>% 
  summarise(pct = sum(pct)) %>% group_by(poll_id,pollster_id.x,fte_grade.x,state.x,total_pct,pollster_id.y,pollster,fte_grade.y,start_date,end_date,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state.y,party,statePolls) %>% 
  summarise(pct = median(pct))
#pres_cleaned <- pres_cleaned %>% group_by(poll_id,pollster_id.x,question_id,fte_grade.x,pollster,sample_size,pollster_rating_id,methodology,cycle,seat_number,partisan,state) %>% 
# combine each party pct per poll into one row
# pres_cleaned <- pres_cleaned %>% group_by(poll_id,party) %>% slice(which.max(pct)) %>% ungroup() %>% 
#   pivot_wider(
#     id_cols = c("poll_id", "pollster_id.x", "state.x","fte_grade.x", "total_pct", "pollster_id.y", "pollster", "fte_grade.y", "sample_size", "pollster_rating_id", "methodology", "partisan", "cycle", "seat_number"),
#     names_from = party,
#     values_from = pct)

# pivot, no row select
house_cleaned <- house_cleaned %>% 
  pivot_wider(
    id_cols = c("poll_id", "pollster_id.x", "state.x","fte_grade.x", "total_pct", "pollster_id.y", "pollster", "fte_grade.y", "sample_size", "pollster_rating_id", "methodology", "partisan", "cycle", "seat_number"),
    names_from = party,
    values_from = pct)


# merging rating data into cleaned poll data
house_cleaned <- left_join(house_cleaned,pollRatings,by = "pollster_rating_id")


# ranking rows by grade and total percent, keeping top 5 polls per state, and eliminating columns
house_cleaned <- house_cleaned %>% arrange(valid, desc(lower_error_diff)) %>% ungroup() %>% group_by(state.x) %>%  filter(row_number() <= numberOfPolls) %>% ungroup() %>% subset(select = c(pollster,valid,lower_error_diff,methodology,partisan,DEM,REP,cycle,state.x,seat_number))
# renaming columns
colnames(house_cleaned) <- c("pollster","valid","lower_error_diff","methodology","pollsterParty","DEM","REP","cycle","state","district")
#pres_cleaned$grade <- as.character(pres_cleaned$grade)
# replacing empty cells w NA
house_cleaned[house_cleaned == ""] <- NA
# Each state top 5 polls
house_cleaned <- house_cleaned %>% group_by(state,district,cycle) %>% summarise(
  "numPolls" = n(),
  "pollster1.name" = pollster[1],
  "pollster1.valid" = valid[1],
  "pollster1.led" = lower_error_diff[1],
  "pollster1.methodology" = methodology[1],
  "pollster1.partybias" = pollsterParty[1],
  "pollster1.dem" = DEM[1],
  "pollster1.rep" = REP[1],
  "pollster2.name" = pollster[2],
  "pollster2.valid" = valid[2],
  "pollster2.led" = lower_error_diff[2],
  "pollster2.methodology" = methodology[2],
  "pollster2.partybias" = pollsterParty[2],
  "pollster2.dem" = DEM[2],
  "pollster2.rep" = REP[2],
  "pollster3.name" = pollster[3],
  "pollster3.valid" = valid[3],
  "pollster3.led" = lower_error_diff[3],
  "pollster3.methodology" = methodology[3],
  "pollster3.partybias" = pollsterParty[3],
  "pollster3.dem" = DEM[3],
  "pollster3.rep" = REP[3],
  "pollster4.name" = pollster[4],
  "pollster4.valid" = valid[4],
  "pollster4.led" = lower_error_diff[4],
  "pollster4.methodology" = methodology[4],
  "pollster4.partybias" = pollsterParty[4],
  "pollster4.dem" = DEM[4],
  "pollster4.rep" = REP[4],
  "pollster5.name" = pollster[5],
  "pollster5.valid" = valid[5],
  "pollster5.led" = lower_error_diff[5],
  "pollster5.methodology" = methodology[5],
  "pollster5.partybias" = pollsterParty[5],
  "pollster5.dem" = DEM[5],
  "pollster5.rep" = REP[5]
) 
# add identifiers
house_cleaned <- house_cleaned %>% mutate(
                                        "isMidterm" = FALSE,
                                        "office_type" = "House")

# GUBERNATORIAL -----------------------------------------
uncleaned_gub <- read.csv("https://projects.fivethirtyeight.com/polls-page/data/governor_polls.csv")
gub_cleaned <- uncleaned_gub %>%
  group_by(poll_id,pollster_id,pollster,fte_grade,start_date,end_date,question_id,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state,party,answer,candidate_name) %>% 
  summarise(pct = mean(pct)) %>% filter(as.Date(Sys.Date()) - as.Date(mdy(end_date)) <= 21)
# pulls number of polls in the last 21 days
gub_cleaned <- gub_cleaned %>% group_by(state) %>% mutate(statePolls =  n_distinct(poll_id)) %>%  ungroup()
# gets the total percent for each question and keep questions with the highest total percent for each poll
max_pct_sums_gub <- gub_cleaned %>% group_by(poll_id,pollster_id,question_id,fte_grade,state) %>% summarize(total_pct = sum(pct)) %>% 
  group_by(poll_id,pollster_id,fte_grade,state) %>% filter(total_pct == max(total_pct))
# make grade into factor
max_pct_sums_gub$fte_grade <- factor(max_pct_sums_gub$fte_grade, levels = grade_order, ordered = TRUE)
gub_cleaned <- max_pct_sums_gub %>% left_join(gub_cleaned, by = c("poll_id", "question_id"))
# keep the top party candidate for each question/poll
# pres_cleaned <- pres_cleaned %>% group_by(poll_id, party) %>% filter(pct == max(pct)) %>% ungroup() %>% 
#   mutate(party = case_when(
#   party %in% c("DEM", "REP") ~ party, 
#   TRUE ~ "IND"
# ))
# sum %s for same party candidates
gub_cleaned <- gub_cleaned %>% mutate(party = case_when(party %in% c("DEM", "REP") ~ party,TRUE ~ "IND")) %>% filter(party != "IND") %>% 
  group_by(poll_id,pollster_id.x,question_id,fte_grade.x,state.x,total_pct,pollster_id.y,pollster,fte_grade.y,start_date,end_date,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state.y,party,statePolls) %>% 
  summarise(pct = sum(pct)) %>% group_by(poll_id,pollster_id.x,fte_grade.x,state.x,total_pct,pollster_id.y,pollster,fte_grade.y,start_date,end_date,sample_size,pollster_rating_id,methodology,partisan,cycle,seat_number,state.y,party,statePolls) %>% 
  summarise(pct = median(pct))
#pres_cleaned <- pres_cleaned %>% group_by(poll_id,pollster_id.x,question_id,fte_grade.x,pollster,sample_size,pollster_rating_id,methodology,cycle,seat_number,partisan,state) %>% 
# combine each party pct per poll into one row
# pres_cleaned <- pres_cleaned %>% group_by(poll_id,party) %>% slice(which.max(pct)) %>% ungroup() %>% 
#   pivot_wider(
#     id_cols = c("poll_id", "pollster_id.x", "state.x","fte_grade.x", "total_pct", "pollster_id.y", "pollster", "fte_grade.y", "sample_size", "pollster_rating_id", "methodology", "partisan", "cycle", "seat_number"),
#     names_from = party,
#     values_from = pct)

# pivot, no row select
gub_cleaned <- gub_cleaned %>% 
  pivot_wider(
    id_cols = c("poll_id", "pollster_id.x", "state.x","fte_grade.x", "total_pct", "pollster_id.y", "pollster", "fte_grade.y", "sample_size", "pollster_rating_id", "methodology", "partisan", "cycle", "seat_number"),
    names_from = party,
    values_from = pct)


# merging rating data into cleaned poll data
gub_cleaned <- left_join(gub_cleaned,pollRatings,by = "pollster_rating_id")


# ranking rows by grade and total percent, keeping top 5 polls per state, and eliminating columns
gub_cleaned <- gub_cleaned %>% arrange(valid, desc(lower_error_diff)) %>% ungroup() %>% group_by(state.x) %>%  filter(row_number() <= numberOfPolls) %>% ungroup() %>% subset(select = c(pollster,valid,lower_error_diff,methodology,partisan,DEM,REP,cycle,state.x,seat_number))
# renaming columns
colnames(gub_cleaned) <- c("pollster","valid","lower_error_diff","methodology","pollsterParty","DEM","REP","cycle","state","district")
#pres_cleaned$grade <- as.character(pres_cleaned$grade)
# replacing empty cells w NA
gub_cleaned[gub_cleaned == ""] <- NA
# Each state top 5 polls
gub_cleaned <- gub_cleaned %>% group_by(state,district,cycle) %>% summarise(
  "numPolls" = n(),
  "pollster1.name" = pollster[1],
  "pollster1.valid" = valid[1],
  "pollster1.led" = lower_error_diff[1],
  "pollster1.methodology" = methodology[1],
  "pollster1.partybias" = pollsterParty[1],
  "pollster1.dem" = DEM[1],
  "pollster1.rep" = REP[1],
  "pollster2.name" = pollster[2],
  "pollster2.valid" = valid[2],
  "pollster2.led" = lower_error_diff[2],
  "pollster2.methodology" = methodology[2],
  "pollster2.partybias" = pollsterParty[2],
  "pollster2.dem" = DEM[2],
  "pollster2.rep" = REP[2],
  "pollster3.name" = pollster[3],
  "pollster3.valid" = valid[3],
  "pollster3.led" = lower_error_diff[3],
  "pollster3.methodology" = methodology[3],
  "pollster3.partybias" = pollsterParty[3],
  "pollster3.dem" = DEM[3],
  "pollster3.rep" = REP[3],
  "pollster4.name" = pollster[4],
  "pollster4.valid" = valid[4],
  "pollster4.led" = lower_error_diff[4],
  "pollster4.methodology" = methodology[4],
  "pollster4.partybias" = pollsterParty[4],
  "pollster4.dem" = DEM[4],
  "pollster4.rep" = REP[4],
  "pollster5.name" = pollster[5],
  "pollster5.valid" = valid[5],
  "pollster5.led" = lower_error_diff[5],
  "pollster5.methodology" = methodology[5],
  "pollster5.partybias" = pollsterParty[5],
  "pollster5.dem" = DEM[5],
  "pollster5.rep" = REP[5]
) 
# add identifiers
gub_cleaned <- gub_cleaned %>% mutate(
                                        "isMidterm" = FALSE,
                                        "office_type" = "Gubernatorial")