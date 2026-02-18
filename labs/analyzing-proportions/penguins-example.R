
# Load packages ----

library(tidyverse)
library(palmerpenguins)


# Import and clean dataset ----

penguins_clean <- 
  penguins |> 
  filter(!is.na(sex))


# Analysis ----

penguins_summary <-
  penguins_clean |> 
  summarize(
    n_sex = n(),
    .by = sex
  ) |> 
  mutate(
    n_total = sum(n_sex),
    p_hat = n_sex / n_total,
    se = sqrt(p_hat * (1 - p_hat) / n_total),
    p_prime = (n_sex + 2) / (n_total + 4),
    ci_lower = p_prime - 1.96 * sqrt(p_prime * (1 - p_prime) / (n_total + 4)),
    ci_upper = p_prime + 1.96 * sqrt(p_prime * (1 - p_prime) / (n_total + 4))
  ) |> 
  print()


# Graph estimates and CIs ----

ggplot(penguins_summary) +
  geom_pointrange(aes(x = sex, y = p_hat, ymin = ci_lower, ymax = ci_upper)) +
  labs(
    x = "Sex",
    y = "Proportion",
    title = "Estimated proportion of penguins by sex",
    subtitle = "95% confidence intervals calculated using the Agresti-Coull method"
  )


# Hypothesis test ----

n_female <- 
  penguins_summary |> 
  filter(sex == "female") |> 
  pull(n_sex)

n_total <-
  penguins_summary |> 
  filter(sex == "female") |> 
  pull(n_total)

prop.test(x = n_female, n = n_total, p = 0.5, correct = FALSE)

