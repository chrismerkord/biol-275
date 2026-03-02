
library(tidyverse)
library(haven)

# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/DPQ_L.htm#DPQ020

dpq_data <- 
  read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/DPQ_L.xpt") |> 
  select(SEQN, DPQ020) |> 
  mutate(
    depressed = factor(
      case_match(
        DPQ020,
        0 ~ "No",
        1 ~ "Yes",
        2 ~ "Yes",
        3 ~ "Yes",
        7 ~ NA_character_,
        9 ~ NA_character_
      ),
      levels = c("No", "Yes")
    )
  ) |> 
  drop_na() |>
  print()

# https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/SLQ_L.htm#SLD012

slq_data <-
  read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/SLQ_L.xpt") |> 
  select(SEQN, SLD012) |> 
  mutate(
    sleep = factor(
      case_when(
        SLD012 <= 7 ~ "Short",
        SLD012 > 7 ~ "Adequate"
      ),
      levels = c("Short", "Adequate")
    ) 
  ) |>
  drop_na() |>
  print()

data <-
  inner_join(slq_data, dpq_data, by = "SEQN") |> 
  select(sleep, depressed) |> 
  print()

tab <- table(data)

risk_table <- prop.table(tab, margin = 1)
risk_table

risk_short  <- risk_table["Short", "Yes"]
risk_long   <- risk_table["Adequate", "Yes"]

RR <- risk_short / risk_long
RR

chisq.test(tab)

mosaicplot(
  tab,
  main = "Sleep duration vs. depressive symptoms (NHANES)",
  xlab = "Sleep (risk factor)",
  ylab = "Depressed (outcome)",
  las = 1,
  color = c("#0072b2", "#e69f00")
)
