
# setup -------------------------------------------------------------------

library(fs)
library(here)

generated_data_path <- here("data", "generated")

# create data folder if it does not exist
if (!dir_exists(generated_data_path)) {
  dir_create(generated_data_path)
}

# set random seed for reproducibility
set.seed(275)


# simulate drug trial data ------------------------------------------------
# Simulate FDA-style drug efficacy trial data (long format) with header metadata


## load packages ----------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(stringr)


## trial design -----------------------------------------------------------
# Trial design (typical minimal structure)

n_subj <- 120

visits <- tibble::tibble(
  AVISITN = c(0, 4, 8),
  AVISIT  = c("Baseline", "Week 4", "Week 8")
)

arms <- tibble::tibble(
  ARMCD = c("PBO", "LD", "HD"),
  ARM   = c("Placebo", "Low Dose", "High Dose")
)


## create subject table ---------------------------------------------------

# Create subject-level table (one row per participant)

adsl <- tibble::tibble(
  STUDYID = "WNV-EX-001",
  USUBJID = str_c(STUDYID, "-", str_pad(1:n_subj, width = 4, pad = "0")),
  SITEID  = sample(c("1001", "1002", "1003", "1004"), n_subj, replace = TRUE),
  ARMCD   = sample(arms$ARMCD, n_subj, replace = TRUE, prob = c(1, 1, 1))
) %>%
  left_join(arms, by = "ARMCD")


## build dataset ----------------------------------------------------------

# Build long-format efficacy dataset (one row per participant per visit)

adlb <- adsl %>%
  tidyr::crossing(visits) %>%
  mutate(
    PARAMCD = "BIOMARK",
    PARAM   = "Inflammatory Biomarker (arbitrary units)",
    # baseline biomarker with site-to-site differences
    BASE = round(rnorm(n(), mean = 55, sd = 10), 1),
    
    # treatment effect grows over time; placebo small change
    trt_effect =
      case_when(
        ARMCD == "PBO" ~ 0.8,
        ARMCD == "LD"  ~ 3.0,
        ARMCD == "HD"  ~ 5.5
      ),
    
    # expected mean change at each visit (baseline = 0)
    mu_change =
      case_when(
        AVISITN == 0 ~ 0,
        AVISITN == 4 ~ -1.0 * trt_effect,
        AVISITN == 8 ~ -2.0 * trt_effect
      ),
    
    # observed outcome with measurement noise
    AVAL = round(BASE + mu_change + rnorm(n(), mean = 0, sd = 6), 1),
    
    # store baseline value per subject for realism
    BASE = if_else(AVISITN == 0, AVAL, NA_real_)
  ) %>%
  group_by(USUBJID) %>%
  mutate(
    BASE = BASE[AVISITN == 0][1]
  ) %>%
  ungroup()


# add missing values ------------------------------------------------------

# Add missing follow-up outcomes (no missing at baseline)
#   - modest dropout + intermittent missingness at Weeks 4/8

# subject-level dropout probability (higher in High Dose)
dropout_subj <- adsl %>%
  transmute(
    USUBJID,
    p_dropout = case_when(
      ARMCD == "PBO" ~ 0.06,
      ARMCD == "LD"  ~ 0.08,
      ARMCD == "HD"  ~ 0.12
    ),
    drop_out = rbinom(n(), size = 1, prob = p_dropout)
  )

adlb <- adlb %>%
  left_join(dropout_subj, by = "USUBJID") %>%
  mutate(
    # intermittent missingness (applies only to follow-up visits)
    p_miss = case_when(
      AVISITN == 0 ~ 0,
      AVISITN == 4 ~ 0.05,
      AVISITN == 8 ~ 0.06
    ),
    miss_intermit = rbinom(n(), size = 1, prob = p_miss),
    
    # if dropped out, Week 8 is missing; sometimes Week 4 missing too
    AVAL = case_when(
      AVISITN == 0 ~ AVAL,
      drop_out == 1 & AVISITN == 8 ~ NA_real_,
      drop_out == 1 & AVISITN == 4 & rbinom(n(), 1, 0.35) == 1 ~ NA_real_,
      miss_intermit == 1 ~ NA_real_,
      TRUE ~ AVAL
    ),
    
    # replace missing values with NR (not reported)
    AVAL = if_else(is.na(AVAL), "NR", as.character(AVAL))
    
  ) %>%
  select(
    STUDYID, USUBJID, SITEID,
    ARMCD, ARM,
    PARAMCD, PARAM,
    AVISITN, AVISIT,
    BASE, AVAL
  ) %>%
  arrange(USUBJID, AVISITN)


# write csv ---------------------------------------------------------------

# Write CSV with two metadata (non-data) lines at top

out_file <- fs::path(here::here("data", "generated"), "drug-efficacy-data.csv")

meta_lines <- c(
  "Simulated clinical trial dataset for BIOL 275 (NOT real patient data).",
  "Format: long; one record per participant per visit."
)

data_lines <- readr::format_csv(adlb, na = "NR")

writeLines(c(meta_lines, data_lines), out_file)


# simulate bird data ------------------------------------------------------

# ------------------------------------------------------------------------------
# Generate practice dataset for Lab 03 (import + skip + NA + factors)
# Writes: data/practice-trial.csv
# ------------------------------------------------------------------------------

library(readr)
library(dplyr)
library(tidyr)
library(stringr)

points <- tibble(
  point_id = str_c("PT-", str_pad(1:12, width = 2, pad = "0"))
)

species <- tibble(
  species = c("Grasshopper Sparrow", "Lark Sparrow", "Baltimore Oriole")
)

# Beaufort wind levels (ordinal categorical)
wind_levels <- 0:4

# Build long dataset: one row per point x species x visit
# Two visits (dates) to create repeated measures without complexity
visits <- tibble(
  visit = c(1, 2),
  date = as.Date(c("2025-06-05", "2025-06-19")),
  time = c("06:15", "06:45")
)

df <- points %>%
  crossing(species, visits) %>%
  mutate(
    datetime = paste(date, time),
    wind = sample(wind_levels, n(), replace = TRUE, prob = c(0.20, 0.30, 0.25, 0.15, 0.10))
  )

# Species-specific expected counts (lambda) with wind penalty
# - higher wind -> fewer detections, on average
lambda_by_species <- tibble(
  species = c("Grasshopper Sparrow", "Lark Sparrow", "Baltimore Oriole"),
  lambda0 = c(3.0, 1.6, 2.2)
)

df <- df %>%
  left_join(lambda_by_species, by = "species") %>%
  mutate(
    lambda = lambda0 * exp(-0.25 * wind),
    bird_count = rpois(n(), lambda = lambda),
    bird_count = pmin(bird_count, 15L) # keep counts in a realistic range for teaching
  ) %>%
  select(point_id, species, datetime, wind, bird_count) %>%
  arrange(point_id, species, datetime)

# Introduce missing wind measurements at random
# (missingness only in wind; keep it modest)
set.seed(275)
miss_idx <- sample(seq_len(nrow(df)), size = 10, replace = FALSE)

df_out <- df %>%
  mutate(
    wind = as.character(wind),
    wind = replace(wind, miss_idx, "Not Recorded")
  )

# Write as CSV lines with 2 metadata lines at the top; missing coded as "ND"
out_file <- "data/point-count-data.csv"

meta_lines <- c(
  "Simulated bird point count data; not real survey results)."
)

data_lines <- readr::format_csv(df_out)

writeLines(c(meta_lines, data_lines), out_file)
