# loading in libraries
library("tidyverse")
library("utils")

# making empty dataframes. rows will be appended to these in later loops
finances <- data.frame()
candidiates <- data.frame()

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
  finances_year <- read_delim(file = paste("data/fec data/","weball", sprintf("%02d", i * 2), ".txt", sep = ""),delim = "|", col_names = FALSE, show_col_types = FALSE) %>% mutate("YEAR" = as.double(paste("20",sprintf("%02d", i * 2), sep = ""))) 
  colnames(finances_year) <- c("CandidateID","CandidateN","Incumbent","Party","Party_affiliation","Total_receipts","Transfers from authorized committees","Total_disbursements","Transfers to authorized committees","Beginning cash","Ending cash","Contributions from candidate","Loans from candidate","Other loans","Candidate loan repayments","Other loan repayments","Debts owed by","Total_individual_contributions","Candidate state","Candidate district","Special election status","Primary election status","Runoff election status","General election status","General election percentage","Contributions from other political committees","Contributions from party committees","Coverage end date","Refunds to individuals","Committee Refunds","YEAR")
  finances_year <- finances_year %>% subset(select = c("CandidateID","Total_receipts","Total_disbursements","Total_individual_contributions","YEAR"))
  
  # append the filing year dataframe to the general one
  finances <- rbind(finances, finances_year)
}

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
  candidate_year <- read_delim(file = paste("data/fec data/","cn", sprintf("%02d", i * 2), ".txt", sep = ""),delim = "|", col_names = FALSE, show_col_types = FALSE) %>% subset(select = c(1:8))
  candidate_year <- candidate_year %>% mutate("filing_cycle" = sprintf("%02d", i * 2))
  colnames(candidate_year) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI", "filing_cycle")
  
  # appending the year specific df to the general df
  candidiates <- rbind(candidiates, filter(candidate_year, CAND_ELECTION_YR >= 2002))
}

# some filing years overlap on campaign cycle data with incongruous data. 
# this takes the most recent filing cycle data for each election cycle
candidiates <- candidiates %>% group_by(CAND_ID, CAND_ELECTION_YR) %>% slice(which.max(filing_cycle)) %>% ungroup()

# combining candidate info with finance info. stop here for candidate level data
allYearsJoined <- left_join(candidiates,finances,by = c("CAND_ID" = "CandidateID","CAND_ELECTION_YR" = "YEAR")) %>% filter(!is.na(Total_receipts) & Total_receipts > 0)

# grouping down to the party level
partyDistillation <- allYearsJoined %>% mutate(CAND_PTY_AFFILIATION = case_when(CAND_PTY_AFFILIATION %in% c("DEM", "REP") ~ CAND_PTY_AFFILIATION,TRUE ~ "IND")) %>% group_by(CAND_ELECTION_YR, CAND_OFFICE_ST, CAND_OFFICE, CAND_OFFICE_DISTRICT, CAND_PTY_AFFILIATION) %>% summarise(
  "allReceipts" = sum(Total_receipts),
  "allDisbursements" = sum(Total_disbursements),
  "allIndivContributions" = sum(Total_individual_contributions),
)

## export
write.csv(partyDistillation,file = "cleaned_data/fecData20022024.csv")