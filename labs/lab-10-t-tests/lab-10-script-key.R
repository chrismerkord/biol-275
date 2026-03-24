

# load packages -----------------------------------------------------------

library(tidyverse) # for data manipulation and visualization
library(haven) # for reading SAS XPT files
library(palmerpenguins) # for penguin data
library(lterdatasampler) # for bison data
library(PairedData) # for shoulder data


# one-sample test ----------------------------------------------------------

# uses the NHANES sleep dataet, which contains a variable SLD012 that 
# measures the number of hours of sleep a person gets on a typical night. 
# We will test whether the average number of hours of sleep is different from 8 hours.

slq_data <-
  read_xpt("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2021/DataFiles/SLQ_L.xpt") |> 
  select(SLD012) |> 
  drop_na() |>
  slice_head(n = 100) |> 
  print()

slq_data |>
  ggplot(aes(x = SLD012)) +
  geom_histogram(binwidth = 1, boundary = 0) +
  labs(
    x = "Hours of sleep",
    y = "Frequency",
    title = "Distribution of sleep duration"
  ) +
  theme_minimal()

t.test(slq_data$SLD012, mu = 8)


# two-sample test ----------------------------------------------------------

bison_data <-
  knz_bison |> 
  drop_na(animal_sex, animal_weight) |>
  slice_head(n = 100, by = animal_sex) |> 
  print()

bison_data |>
  ggplot(aes(x = animal_weight)) +
  geom_histogram(binwidth = 50) +
  facet_wrap(~animal_sex, ncol=1)

# not normal

wilcox.test(animal_weight ~ animal_sex, data = bison_data)


# paired t-test -----------------------------------------------------------

shoulder_data <- 
  tibble(Shoulder) |> 
  rowwise() |>
  mutate(
    strongarm = max(Left, Right),
    weakarm = min(Left, Right)
  ) |>
  print()

# normal  

swimmers <- 
  shoulder_data |> 
  filter(Group == "Swimmer") |>
  print()

nonswimmers <-
  shoulder_data |> 
  filter(Group == "Control") |>
  print()

t.test(nonswimmers$Left, nonswimmers$Right, paired = TRUE)
t.test(nonswimmers$strongarm, nonswimmers$weakarm, paired = TRUE)
