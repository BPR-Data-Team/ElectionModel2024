# loading in libraries
library("tidyverse")
library("utils")

# making empty dataframes. rows will be appended to these in later loops
finances <- data.frame()
candidiates <- data.frame()
cpi <- read.csv("cleaned_data/CPI.csv") 

# programmatically loops through 12 years of fec data on campaign finance
for (i in 1:12) {
  url <- paste("https://www.fec.gov/files/bulk-downloads/20",sprintf("%02d", i * 2),"/weball", sprintf("%02d", i * 2), ".zip", sep = "")
  destination <- paste("data/fec data/","finance", sprintf("%02d", i * 2), ".zip", sep = "")
  
  # Download the zip file
  download.file(url, destination, mode = "wb")
  
  # Unzip the downloaded file
  unzip(destination, exdir = "data/fec data/")
  
  # delete the zip
  file.remove(destination)
  
  # read in the pipe delimited file to a dataframe 
  finances_year <- read_delim(file = paste("data/fec data/","weball", sprintf("%02d", i * 2), ".txt", sep = ""),delim = "|", col_names = FALSE, show_col_types = FALSE) %>% 
    mutate("YEAR" = as.double(paste("20",sprintf("%02d", i * 2), sep = ""))) %>% 
    setNames(c("CandidateID","CandidateN","Incumbent","Party","Party_affiliation","Total_receipts","Transfers from authorized committees","Total_disbursements","Transfers to authorized committees","Beginning cash","Ending cash","Contributions from candidate","Loans from candidate","Other loans","Candidate loan repayments","Other loan repayments","Debts owed by","Total_individual_contributions","Candidate state","Candidate district","Special election status","Primary election status","Runoff election status","General election status","General election percentage","Contributions from other political committees","Contributions from party committees","Coverage end date","Refunds to individuals","Committee Refunds","YEAR")) %>% 
    subset(select = c(1, 3, 6:12, 18, 31))
  
  # append the filing year dataframe to the general one
  finances <- rbind(finances, finances_year)
}

finances <- finances %>% filter(!is.na(Total_receipts) & Total_receipts > 0)

# programmatically loops through 12 years of fec data on candidate info
for (i in 1:12) {
  url <- paste("https://www.fec.gov/files/bulk-downloads/20",sprintf("%02d", i * 2),"/cn", sprintf("%02d", i * 2), ".zip", sep = "")
  destination <- paste("data/fec data/","candidate", sprintf("%02d", i * 2), ".zip", sep = "")

  # Download the zip file
  download.file(url, destination, mode = "wb")

  # Unzip the downloaded file
  unzip(destination, exdir = "data/fec data")

  # delete the zip file
  file.remove(destination)
  file.rename("data/fec data/cn.txt",paste("data/fec data/","cn", sprintf("%02d", i * 2), ".txt", sep = ""))

  # reading in the delimited file to a df
  candidate_year <- read_delim(file = paste("data/fec data/","cn", sprintf("%02d", i * 2), ".txt", sep = ""),delim = "|", col_names = FALSE, show_col_types = FALSE) %>% 
    subset(select = c(1, 3:8)) %>% 
    mutate("filing_cycle" = sprintf("%02d", i * 2)) %>% 
    setNames(c("CAND_ID", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI", "filing_cycle"))

  # appending the year specific df to the general df
  candidiates <- rbind(candidiates, filter(candidate_year, CAND_ELECTION_YR >= 2002))
}

# some filing years overlap on campaign cycle data with incongruous data.
# this takes the most recent filing cycle data for each election cycle
candidiates <- candidiates %>% group_by(CAND_ID, CAND_ELECTION_YR) %>% slice(which.max(filing_cycle)) %>% ungroup()

# combining candidate info with finance info. stop here for candidate level data
allYearsJoined <- left_join(finances,candidiates,by = c("CandidateID" = "CAND_ID", "YEAR" = "CAND_ELECTION_YR"))

partyDistillation <- allYearsJoined %>% 
  mutate(CAND_PTY_AFFILIATION = case_when(CAND_PTY_AFFILIATION %in% c("DEM", "REP") ~ CAND_PTY_AFFILIATION,TRUE ~ "IND")) %>%
  filter(CAND_PTY_AFFILIATION != "IND") %>%
  group_by(YEAR, CAND_OFFICE_ST, CAND_OFFICE, CAND_OFFICE_DISTRICT) %>%
  mutate("open_status" = (any(CAND_ICI == "O", na.rm = TRUE))) %>% ungroup() %>%
  rename(year = YEAR, 
         state = CAND_OFFICE_ST, 
         office_type = CAND_OFFICE,
         district = CAND_OFFICE_DISTRICT,
         is_open = open_status,
         party = CAND_PTY_AFFILIATION, 
         receipts = Total_receipts, 
         from_committee_transfers = `Transfers from authorized committees`,
         disbursements = Total_disbursements,
         to_committee_transfers = `Transfers to authorized committees`,
         beginning_cash = `Beginning cash`,
         ending_cash = `Ending cash`,
         candidate_contributions = `Contributions from candidate`,
         individual_contributions = Total_individual_contributions) %>%
  #Want to keep district numbers correct -- Senate/Gov have district 0
  mutate(district = ifelse(district == 0 & office_type == 'H',
                           1, district),
         office_type = case_when(
           office_type == "H" ~ "House",
           office_type == "S" ~ "Senate", TRUE ~ "President"
         )) %>%
  pivot_wider(id_cols = c(year, state, office_type, district, is_open),
              names_from = party,
              values_from = c(receipts, from_committee_transfers, disbursements, 
                              to_committee_transfers, beginning_cash,
                              ending_cash, candidate_contributions,
                              individual_contributions),
              values_fn = sum) %>%
  left_join(cpi, by = c('year' = 'year')) %>%
  mutate(across(matches("DEM|REP"), ~. * 100 / current)) %>%
  select(-c("X", 'current', 'previous', 'change'))


calculate_log_proportion <- function(data, metric) {
  metric_dem <- paste0(metric, "_DEM")
  metric_rep <- paste0(metric, "_REP")
  
  # Create temporary copies of the Democratic and Republican columns
  dem_values <- data[[metric_dem]]
  rep_values <- data[[metric_rep]]
  
  # Replace negative values with 0 in the temporary copies, excluding NA values
  dem_values[!is.na(dem_values) & dem_values < 1] <- 1
  rep_values[!is.na(rep_values) & rep_values < 1] <- 1
  
  log_proportion <- log(round(dem_values) / round(rep_values))
  
  # Handling cases with NA
  ifelse(is.na(data[[metric_dem]]) | is.na(data[[metric_rep]]),
         ifelse(is.na(data[[metric_dem]]), ifelse(is.na(data[[metric_rep]]), NA, -6), 6),
         log_proportion)
}

partyDistillation <- partyDistillation %>% mutate(
  "receipts" = calculate_log_proportion(partyDistillation, "receipts"),
  "from_committee_transfers" = calculate_log_proportion(partyDistillation, "from_committee_transfers"),
  "disbursements" = calculate_log_proportion(partyDistillation, "disbursements"),
  "to_committee_transfers" = calculate_log_proportion(partyDistillation, "to_committee_transfers"),
  "beginning_cash" = calculate_log_proportion(partyDistillation, "beginning_cash"),
  "ending_cash" = calculate_log_proportion(partyDistillation, "ending_cash"),
  "candidate_contributions" = calculate_log_proportion(partyDistillation, "candidate_contributions"),
  "individual_contributions" = calculate_log_proportion(partyDistillation, "individual_contributions")
) %>% filter(!is.na(district))



## export
write.csv(partyDistillation,file = "cleaned_data/fecData20022024.csv")