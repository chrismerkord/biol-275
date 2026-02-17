
# Load packages ----

library(tidyverse)
library(lterdatasampler)


# Import and clean dataset ----

knz_bison_clean <- 
  knz_bison |> 
  filter(!is.na(animal_sex))


# Analysis ----

knz_bison_summary <-
  knz_bison_clean |> 
  summarize(
    n_animal_sex = n(),
    .by = animal_sex
  ) |> 
  mutate(
    n_total = sum(n_animal_sex),
    p_hat = n_animal_sex / n_total,
    se = sqrt(p_hat * (1 - p_hat) / n_total),
    p_prime = (n_animal_sex + 2) / (n_total + 4),
    ci_lower = p_prime - 1.96 * sqrt(p_prime * (1 - p_prime) / (n_total + 4)),
    ci_upper = p_prime + 1.96 * sqrt(p_prime * (1 - p_prime) / (n_total + 4))
  ) |> 
  print()


# Graph estimates and CIs ----

ggplot(knz_bison_summary) +
  geom_pointrange(aes(x = animal_sex, y = p_hat, ymin = ci_lower, ymax = ci_upper)) +
  labs(
    x = "Sex",
    y = "Proportion",
    title = "Estimated proportion of bison by sex",
    subtitle = "95% confidence intervals calculated using the Agresti-Coull method"
  )


# Hypothesis test ----

n_female <- 
  knz_bison_summary |> 
  filter(animal_sex == "F") |> 
  pull(n_animal_sex)

n_total <-
  knz_bison_summary |> 
  filter(animal_sex == "F") |> 
  pull(n_total)

prop.test(x = n_female, n = n_total, p = 0.5, correct = FALSE)

