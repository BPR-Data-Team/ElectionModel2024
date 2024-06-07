library(tidyverse)

preds <- read.csv(file = "cleaned_data/Predictions over time.csv")

preds <- preds %>% 
  pivot_longer(
    cols = !c(state, district, office_type), 
    names_to = "date",
    values_to = "median_margin",
    names_repair = "minimal") %>% 
  subset(select = c(1, 2, 3, 5, 4))

write.csv(preds, file = "cleaned_data/Predictions over time.csv", row.names = FALSE)
