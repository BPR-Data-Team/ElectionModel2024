library("tidyverse") 

# make a function? instead of copying each command to each dataset?? i don't think so

## reading in FEC bulk campaign data
year02 <- read_delim(file = "data/fec candidate data/2002.txt",delim = "|", col_names = FALSE)
year04 <- read_delim(file = "data/fec candidate data/2004.txt",delim = "|", col_names = FALSE)
year08 <- read_delim(file = "data/fec candidate data/2008.txt",delim = "|", col_names = FALSE)
year10 <- read_delim(file = "data/fec candidate data/2010.txt",delim = "|", col_names = FALSE)
year12 <- read_delim(file = "data/fec candidate data/2012.txt",delim = "|", col_names = FALSE)
year14 <- read_delim(file = "data/fec candidate data/2014.txt",delim = "|", col_names = FALSE)
year06 <- read_delim(file = "data/fec candidate data/2006.txt",delim = "|", col_names = FALSE)
year16 <- read_delim(file = "data/fec candidate data/2016.txt",delim = "|", col_names = FALSE)
year18 <- read_delim(file = "data/fec candidate data/2018.txt",delim = "|", col_names = FALSE)
year20 <- read_delim(file = "data/fec candidate data/2020.txt",delim = "|", col_names = FALSE)
year22 <- read_delim(file = "data/fec candidate data/2022.txt",delim = "|", col_names = FALSE)
year24 <- read_delim(file = "data/fec candidate data/2024.txt",delim = "|", col_names = FALSE)

## very very efficiently naming columns
colnames(year02) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year04) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year06) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year08) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year10) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year12) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year14) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year16) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year18) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year20) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year22) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")
colnames(year24) <- c("CAND_ID", "CAND_NAME", "CAND_PTY_AFFILIATION",	"CAND_ELECTION_YR", "CAND_OFFICE_ST",	"CAND_OFFICE",	"CAND_OFFICE_DISTRICT",	"CAND_ICI",	"CAND_STATUS",	"CAND_PCC",	"CAND_ST1",	"CAND_ST2",	"CAND_CITY",	"CAND_ST",	"CAND_ZIP")

## keeping the first eight columns of each dataset
year02 <- year02 %>% subset(select = c(1:8))
year04 <- year04 %>% subset(select = c(1:8))
year06 <- year06 %>% subset(select = c(1:8))
year08 <- year08 %>% subset(select = c(1:8))
year10 <- year10 %>% subset(select = c(1:8))
year12 <- year12 %>% subset(select = c(1:8))
year14 <- year14 %>% subset(select = c(1:8))
year16 <- year16 %>% subset(select = c(1:8))
year18 <- year18 %>% subset(select = c(1:8))
year20 <- year20 %>% subset(select = c(1:8))
year22 <- year22 %>% subset(select = c(1:8))
year24 <- year24 %>% subset(select = c(1:8))

## combining datasets and keeping cycles after 2002 bc some were pre 2000s
allYears <- rbind(year02,year04,year06,year08,year10,year12,year14,year16,year18,year20,year22,year24)
allYears <- filter(allYears, CAND_ELECTION_YR >= 2002)

## reading in bulk FEC finance data on each candidate with inferior identification info
finance_year04 <- read_delim(file = "data/fec data/weball04.txt",delim = "|", col_names = FALSE)
finance_year02 <- read_delim(file = "data/fec data/weball02.txt",delim = "|", col_names = FALSE)
finance_year06 <- read_delim(file = "data/fec data/weball06.txt",delim = "|", col_names = FALSE)
finance_year08 <- read_delim(file = "data/fec data/weball08.txt",delim = "|", col_names = FALSE)
finance_year10 <- read_delim(file = "data/fec data/weball10.txt",delim = "|", col_names = FALSE)
finance_year12 <- read_delim(file = "data/fec data/weball12.txt",delim = "|", col_names = FALSE)
finance_year14 <- read_delim(file = "data/fec data/weball14.txt",delim = "|", col_names = FALSE)
finance_year16 <- read_delim(file = "data/fec data/weball16.txt",delim = "|", col_names = FALSE)
finance_year18 <- read_delim(file = "data/fec data/weball18.txt",delim = "|", col_names = FALSE)
finance_year20 <- read_delim(file = "data/fec data/weball20.txt",delim = "|", col_names = FALSE)
finance_year22 <- read_delim(file = "data/fec data/weball22.txt",delim = "|", col_names = FALSE)
finance_year24 <- read_delim(file = "data/fec data/weball24.txt",delim = "|", col_names = FALSE)

# using the filing cycle as the year
finance_year02 <- finance_year02 %>% mutate("YEAR" = 2002)
finance_year04 <- finance_year04 %>% mutate("YEAR" = 2004)
finance_year06 <- finance_year06 %>% mutate("YEAR" = 2006)
finance_year08 <- finance_year08 %>% mutate("YEAR" = 2008)
finance_year10 <- finance_year10 %>% mutate("YEAR" = 2010)
finance_year12 <- finance_year12 %>% mutate("YEAR" = 2012)
finance_year14 <- finance_year14 %>% mutate("YEAR" = 2014)
finance_year16 <- finance_year16 %>% mutate("YEAR" = 2016)
finance_year18 <- finance_year18 %>% mutate("YEAR" = 2018)
finance_year20 <- finance_year20 %>% mutate("YEAR" = 2020)
finance_year22 <- finance_year22 %>% mutate("YEAR" = 2022)
finance_year24 <- finance_year24 %>% mutate("YEAR" = 2024)

## cleaning and combining finance data
all_finance_years <- rbind(finance_year02,finance_year04,finance_year06,finance_year08,finance_year10,finance_year12,finance_year14,finance_year16,finance_year18,finance_year20,finance_year22,finance_year24)
colnames(all_finance_years) <- c("CandidateID","CandidateN","Incumbent","Party","Party_affiliation","Total_receipts","Transfers from authorized committees","Total_disbursements","Transfers to authorized committees","Beginning cash","Ending cash","Contributions from candidate","Loans from candidate","Other loans","Candidate loan repayments","Other loan repayments","Debts owed by","Total_individual_contributions","Candidate state","Candidate district","Special election status","Primary election status","Runoff election status","General election status","General election percentage","Contributions from other political committees","Contributions from party committees","Coverage end date","Refunds to individuals","Committee Refunds","YEAR")
all_finance_years <- all_finance_years %>% subset(select = c("CandidateID","Total_receipts","Total_disbursements","Total_individual_contributions","YEAR"))

## merging finance data into identification info
allYearsJoined <- left_join(allYears,all_finance_years,by = c("CAND_ID" = "CandidateID","CAND_ELECTION_YR" = "YEAR"))

## export
write.csv(allYearsJoined,file = "cleaned_data/fecCandidates20022024.csv")
