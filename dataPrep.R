# PURPOSE: Clean, analyze, model, and visualize racial disparities in 
#          preventable hospitalizations using healthcare access and 
#          socioeconomic indicators across US counties

# Load Libraries
library(tidyverse)
library(janitor)
library(scales)
library(usmap)
library(broom)
library(MASS)
library(glmmTMB)
library(car)
library(knitr)
library(DHARMa)
library(sandwich)
library(lmtest)
library(readxl)
library(patchwork)
library(ggridges)
library(kableExtra)
library(forcats)


# Read data
national_data <- read_csv("analytic_data2025_v2.csv")

#############################
# DATA CLEANING + FEATURE CREATION
#############################

# Clean variable names (convert to snake_case)
data <- national_data |> clean_names()

# Select variables of interest (clinical care, race, SES)
data_clean <- data |>
  dplyr::select(
    state_fips_code, county_fips_code, x5_digit_fips_code, 
    state_abbreviation, name, 
    primary_care_physicians_raw_value, ratio_of_population_to_primary_care_physicians,
    mental_health_providers_raw_value, ratio_of_population_to_mental_health_providers,
    dentists_raw_value, ratio_of_population_to_dentists, 
    preventable_hospital_stays_raw_value, preventable_hospital_stays_aian, 
    preventable_hospital_stays_asian_pacific_islander, preventable_hospital_stays_black, 
    preventable_hospital_stays_hispanic, preventable_hospital_stays_white, 
    mammography_screening_raw_value, mammography_screening_aian, 
    mammography_screening_asian_pacific_islander, mammography_screening_black, 
    mammography_screening_hispanic, mammography_screening_white, 
    uninsured_raw_value, uninsured_adults_raw_value, uninsured_children_raw_value, 
    high_school_completion_raw_value, unemployment_raw_value, income_inequality_raw_value, 
    other_primary_care_providers_raw_value, high_school_graduation_raw_value,
    percent_american_indian_or_alaska_native_raw_value, percent_asian_raw_value, 
    percent_hispanic_raw_value, percent_native_hawaiian_or_other_pacific_islander_raw_value, 
    percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value, 
    percent_not_proficient_in_english_raw_value, percent_rural_raw_value, 
    population_raw_value
  ) |>

  # Ensure numeric columns are properly formatted
  mutate(across(
    .cols = -c(state_fips_code, county_fips_code, x5_digit_fips_code, state_abbreviation, name),
    .fns = ~ as.numeric(as.character(.x))
  )) |>

  # Estimate subgroup population sizes from percentages
  mutate(
    pop_aian        = (percent_american_indian_or_alaska_native_raw_value / 100) * population_raw_value,
    pop_asian_pacific = (percent_asian_raw_value / 100 + percent_native_hawaiian_or_other_pacific_islander_raw_value / 100) * population_raw_value,
    pop_black       = (percent_non_hispanic_black_raw_value / 100) * population_raw_value,
    pop_hispanic    = (percent_hispanic_raw_value / 100) * population_raw_value,
    pop_white       = (percent_non_hispanic_white_raw_value / 100) * population_raw_value
  ) |>

  # Convert provider ratios to supply per 10k people
  mutate(
    physician_supply_per_10k = 10000 / ratio_of_population_to_primary_care_physicians,
    mental_health_providers_per_10k = 10000 / ratio_of_population_to_mental_health_providers,
    dentists_per_10k = 10000 / ratio_of_population_to_dentists
  ) |>

  # Rename variables for clarity
  rename(
    income_inequality = income_inequality_raw_value,
    unemployment_rate = unemployment_raw_value
  )

# Remove first row if needed (possible header/data issue)
data_clean <- data_clean[-1, ]

# Quick structure check
glimpse(data_clean)

#############################
# MISSING DATA CHECK
#############################

# Compute % missing for each column
na_percent <- sapply(data_clean, function(x) round(mean(is.na(x)) * 100, 1))
na_percent[na_percent > 0]  # Print only columns with NA

#############################
# FEATURE ENGINEERING: RACE-WEIGHTED METRICS
#############################

# Compute weighted average outcomes for POC groups
no_na_data <- data_clean |>
  dplyr::select(-high_school_graduation_raw_value) |>  # Drop unused column
  mutate(
    pop_poc = pop_aian + pop_asian_pacific + pop_black + pop_hispanic,

    preventable_hospital_stays_poc = case_when(
      pop_poc > 0 ~ (
        preventable_hospital_stays_aian * pop_aian +
        preventable_hospital_stays_asian_pacific_islander * pop_asian_pacific +
        preventable_hospital_stays_black * pop_black +
        preventable_hospital_stays_hispanic * pop_hispanic
      ) / pop_poc,
      TRUE ~ NA_real_
    ),

    mammography_screening_poc = case_when(
      pop_poc > 0 ~ (
        mammography_screening_aian * pop_aian +
        mammography_screening_asian_pacific_islander * pop_asian_pacific +
        mammography_screening_black * pop_black +
        mammography_screening_hispanic * pop_hispanic
      ) / pop_poc,
      TRUE ~ NA_real_
    )
  ) |>

  # Drop race-specific variables no longer needed
  dplyr::select(-c(
    preventable_hospital_stays_aian,
    preventable_hospital_stays_asian_pacific_islander,
    preventable_hospital_stays_black,
    preventable_hospital_stays_hispanic,
    mammography_screening_aian,
    mammography_screening_asian_pacific_islander,
    mammography_screening_black,
    mammography_screening_hispanic,
    pop_aian, pop_asian_pacific, pop_black, pop_hispanic
  ))

# Check final dataset size before dropping rows with NAs
dim(no_na_data)

# Drop any remaining NAs
done_data <- no_na_data |> drop_na()

#############################
# FINAL ANALYSIS DATASET
#############################

# Select variables for modeling or EDA
analysis_data <- done_data |>
  dplyr::select(
    # Identifiers
    state_fips_code, county_fips_code, name, state_abbreviation,

    # Outcomes
    preventable_hospital_stays_white, preventable_hospital_stays_poc,

    # Healthcare Access Predictors
    physician_supply_per_10k, mental_health_providers_per_10k, 
    dentists_per_10k, uninsured_raw_value,
    mammography_screening_white, mammography_screening_poc,

    # Socioeconomic Controls
    income_inequality, unemployment_rate, 
    percent_rural_raw_value, high_school_completion_raw_value,

    # Population (for offset or rate calculations)
    pop_white, pop_poc, population_raw_value
  ) |>

  # Convert percentage columns to proportion scale (0–1)
  mutate(across(
    c(uninsured_raw_value, percent_rural_raw_value, high_school_completion_raw_value),
    ~ . / 100
  ))

# Final row/column check
dim(analysis_data)
