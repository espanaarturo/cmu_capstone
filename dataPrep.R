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


# 1. DATA CLEANING

# Load data and clean variable names
national_data <- read_csv("analytic_data2025_v2.csv")
data <- national_data |> clean_names()

# Select and preprocess variables
# Includes clinical, socioeconomic, and demographic measures
data_clean <- data |> 
  dplyr::select(state_fips_code, county_fips_code, x5_digit_fips_code, 
         state_abbreviation, name, primary_care_physicians_raw_value, 
         ratio_of_population_to_primary_care_physicians,
         mental_health_providers_raw_value, ratio_of_population_to_mental_health_providers,
         dentists_raw_value, ratio_of_population_to_dentists, preventable_hospital_stays_raw_value,
         preventable_hospital_stays_aian, preventable_hospital_stays_asian_pacific_islander, 
         preventable_hospital_stays_black, preventable_hospital_stays_hispanic, 
         preventable_hospital_stays_white, mammography_screening_raw_value, 
         mammography_screening_aian, mammography_screening_asian_pacific_islander, 
         mammography_screening_black, mammography_screening_hispanic, 
         mammography_screening_white, uninsured_raw_value, uninsured_adults_raw_value, 
         uninsured_children_raw_value, high_school_completion_raw_value, 
         unemployment_raw_value, income_inequality_raw_value, 
         other_primary_care_providers_raw_value, high_school_graduation_raw_value,
         percent_american_indian_or_alaska_native_raw_value, percent_asian_raw_value, 
         percent_hispanic_raw_value, percent_native_hawaiian_or_other_pacific_islander_raw_value, 
         percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value, 
         percent_not_proficient_in_english_raw_value, percent_rural_raw_value, 
         population_raw_value) |> 
  mutate(across(
    .cols = -c(state_fips_code, county_fips_code, x5_digit_fips_code, 
               state_abbreviation, name),
    .fns = ~ as.numeric(as.character(.x))
  )) |> 
  mutate(
    # Compute subgroup populations
    pop_aian = percent_american_indian_or_alaska_native_raw_value / 100 * population_raw_value,
    pop_asian_pacific = (percent_asian_raw_value / 100 + 
                         percent_native_hawaiian_or_other_pacific_islander_raw_value / 100) * population_raw_value,
    pop_black = percent_non_hispanic_black_raw_value / 100 * population_raw_value,
    pop_hispanic = percent_hispanic_raw_value / 100 * population_raw_value,
    pop_white = percent_non_hispanic_white_raw_value / 100 * population_raw_value,

    # Healthcare provider density per 10k population
    physician_supply_per_10k = 10000 / ratio_of_population_to_primary_care_physicians,
    mental_health_providers_per_10k = 10000 / ratio_of_population_to_mental_health_providers,
    dentists_per_10k = 10000 / ratio_of_population_to_dentists
  ) |> 
  rename(
    income_inequality = income_inequality_raw_value,
    unemployment_rate = unemployment_raw_value
  )

data_clean <- data_clean[-1, ]  # Drop 1st row (often labels)

# Missing data diagnostics
na_percent <- sapply(data_clean, function(x) round(mean(is.na(x)) * 100, 1))
na_percent[na_percent > 0]  # Inspect NA percentages

# Create weighted average hospitalization/screening metrics for POC
done_data <- data_clean |> 
  dplyr::select(-high_school_graduation_raw_value) |> 
  mutate(
    pop_poc = pop_aian + pop_asian_pacific + pop_black + pop_hispanic,
    preventable_hospital_stays_poc = case_when(
      pop_poc > 0 ~ (
        preventable_hospital_stays_aian * pop_aian + 
        preventable_hospital_stays_asian_pacific_islander * pop_asian_pacific + 
        preventable_hospital_stays_black * pop_black + 
        preventable_hospital_stays_hispanic * pop_hispanic) / pop_poc,
      TRUE ~ NA_real_
    ),
    mammography_screening_poc = case_when(
      pop_poc > 0 ~ (
        mammography_screening_aian * pop_aian + 
        mammography_screening_asian_pacific_islander * pop_asian_pacific + 
        mammography_screening_black * pop_black + 
        mammography_screening_hispanic * pop_hispanic) / pop_poc,
      TRUE ~ NA_real_
    )
  ) |> 
  dplyr::select(-c(preventable_hospital_stays_aian,
                   preventable_hospital_stays_asian_pacific_islander,
                   preventable_hospital_stays_black,
                   preventable_hospital_stays_hispanic,
                   mammography_screening_aian,
                   mammography_screening_asian_pacific_islander,
                   mammography_screening_black,
                   mammography_screening_hispanic,
                   pop_aian, pop_asian_pacific, pop_black, pop_hispanic)) |> 
  drop_na()

# Final analysis dataset
analysis_data <- done_data |> 
  dplyr::select(
    state_fips_code, county_fips_code, name, state_abbreviation,
    preventable_hospital_stays_white, preventable_hospital_stays_poc,
    physician_supply_per_10k, mental_health_providers_per_10k, 
    dentists_per_10k, uninsured_raw_value,
    mammography_screening_white, mammography_screening_poc,
    income_inequality, unemployment_rate, 
    percent_rural_raw_value, high_school_completion_raw_value,
    pop_white, pop_poc, population_raw_value
  ) |> 
  mutate(across(c(uninsured_raw_value, percent_rural_raw_value, 
                  high_school_completion_raw_value), ~ ./100))
