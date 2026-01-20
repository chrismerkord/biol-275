
library(tidyverse)

wnv_data <-
  read_csv("data/West Nile virus hospitalizations by case type and year of illness onset, 2004-2024.csv") |> 
  rename(year = Year) |> 
  pivot_longer(
    cols = c("Neuroinvasive", "Non_neuroinvasive"),
    names_to = "case_type",
    values_to = "reported_cases"
  ) |> 
  complete(
    year = 2002:2024, 
    case_type
  ) |> 
  print()

write_csv(wnv_data, "data/wnv-data.csv")
