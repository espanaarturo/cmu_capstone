
#Run at start
library(tidyverse)
library(naniar)
library(janitor)
library(VIM)
library(stats)
library(stringr)
library(scales)
library(leaflet)
library(tigris)
library(glmmTMB)
library(broom)
library(glmmTMB)
library(car)
library(sf)
library(randomForest)
library(biscale)
library(cowplot)
library(rpart)
library(rpart.plot)

national_data <- read_csv("analytic_data2025_v2.csv")

#Clean names
cn_national_data <- national_data |> 
  clean_names()

#Subset variables of interest
cn_national_data_subset <- cn_national_data |> 
  dplyr::select(
    state_fips_code,
    state_abbreviation,
    name,
    population_raw_value,
    county_fips_code,
    flu_vaccinations_raw_value,
    primary_care_physicians_raw_value,
    ratio_of_population_to_primary_care_physicians,
    mental_health_providers_raw_value,
    ratio_of_population_to_mental_health_providers,
    dentists_raw_value,
    ratio_of_population_to_dentists,
    preventable_hospital_stays_raw_value,
    mammography_screening_raw_value,
    uninsured_raw_value,
    uninsured_adults_raw_value,
    severe_housing_problems_raw_value,
    percentage_of_households_with_overcrowding,
    percentage_of_households_with_high_housing_costs,
    broadband_access_raw_value,
    library_access_raw_value,
    some_college_raw_value,
    high_school_completion_raw_value,
    children_in_poverty_raw_value,
    child_care_cost_burden_raw_value,
    child_care_centers_raw_value,
    frequent_physical_distress_raw_value,
    frequent_mental_distress_raw_value,
    limited_access_to_healthy_foods_raw_value,
    food_insecurity_raw_value,
    insufficient_sleep_raw_value,
    teen_births_raw_value,
    excessive_drinking_raw_value,
    adult_smoking_raw_value,
    adult_obesity_raw_value,
    physical_inactivity_raw_value,
    uninsured_children_raw_value,
    other_primary_care_providers_raw_value,
    ratio_of_population_to_primary_care_providers_other_than_physicians,
    traffic_volume_raw_value,
    homeownership_raw_value,
    severe_housing_cost_burden_raw_value,
    severe_housing_problems_raw_value,
    access_to_parks_raw_value,
    census_participation_raw_value,
    high_school_graduation_raw_value,
    reading_scores_raw_value,
    math_scores_raw_value,
    living_wage_raw_value,
    residential_segregation_black_white_raw_value,
    disconnected_youth_raw_value,
    lack_of_social_and_emotional_support_raw_value,
    population_raw_value,
    children_in_single_parent_households_raw_value,
    percent_below_18_years_of_age_raw_value,
    percent_65_and_older_raw_value,
    percent_female_raw_value,
    percent_american_indian_or_alaska_native_raw_value,
    percent_asian_raw_value,
    percent_hispanic_raw_value,
    percent_native_hawaiian_or_other_pacific_islander_raw_value,
    percent_non_hispanic_black_raw_value,
    percent_non_hispanic_white_raw_value,
    percent_disability_functional_limitations_raw_value,
    percent_not_proficient_in_english_raw_value,
    percent_rural_raw_value,
    children_eligible_for_free_or_reduced_price_lunch_raw_value,
    median_household_income_raw_value,
    income_inequality_raw_value,
    unemployment_raw_value
  )

##CLEAN DATA SET
cn_counties_national_subset <- cn_national_data_subset |> 
  filter(str_detect(name, "County|Municipality|Census|Borough|Region|District|Parish|city"))

char_cols <- c("state_abbreviation", "name")

cols_to_convert <- setdiff(names(cn_counties_national_subset), char_cols)

cn_counties_national_subset[cols_to_convert] <- lapply(
  cn_counties_national_subset[cols_to_convert],
  function(x) {
    x <- as.character(x)
    x[x %in% c("NA", "", "N/A", "*", "Suppressed")] <- NA
    as.numeric(x)
  }
)

##TO VIEW MISSING VALUES (IGNORE)
vars <- colnames(cn_counties_national_subset)

var_groups <- split(vars, cut(seq_along(vars), 4, labels = FALSE))

for (i in 1:4) {
  data_subset <- cn_counties_national_subset[, var_groups[[i]]]
  print(gg_miss_var(data_subset) + ggtitle(paste("Missingness - Group", i)))
}

#Map Stuff
options(tigris_use_cache = TRUE)

# Get shapefile for all US counties
counties_sf <- counties(cb = TRUE, year = 2023, class = "sf")

cn_counties_national_subset <- cn_counties_national_subset |> 
  mutate(
    state_fips_code = str_pad(state_fips_code, 2, pad = "0"),
    county_fips_code = str_pad(county_fips_code, 3, pad = "0"),
    fips = paste0(state_fips_code, county_fips_code)
  )

map_data <- counties_sf |> 
  left_join(cn_counties_national_subset, by = c("GEOID" = "fips"))

#US County Map
pal <- colorNumeric(
  palette = "magma",
  domain = map_data$preventable_hospital_stays_raw_value,
  na.color = "gray90"
)

# Load state shapefile
states_sf <- states(cb = TRUE, year = 2023, class = "sf")

# Exclude Alaska (02) and Hawaii (15)
map_data_contig <- map_data %>%
  filter(!STATEFP %in% c("02", "15"))

states_sf_contig <- states_sf %>%
  filter(!STATEFP %in% c("02", "15"))

#PLOT
ggplot() +
  geom_sf(data = map_data_contig, aes(fill = preventable_hospital_stays_raw_value), color = NA) +
  geom_sf(data = states_sf_contig, fill = NA, color = "white", size = 0.15) +
  scale_fill_viridis_c(option = "magma", na.value = "gray90", name = "Preventable\nHospital Stays") +
  coord_sf(
    crs = st_crs(2163),
    xlim = c(-2.4e6, 2.5e6),
    ylim = c(-2.2e6, 1.5e6),
    expand = FALSE
  ) +
  theme_minimal(base_size = 12) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "cm"),
    legend.position = "right"
  ) +
  labs(
    caption = "Source: County Health Rankings, 2023"
  )

#Race Data
race_cols <- c(
  "percent_american_indian_or_alaska_native_raw_value",
  "percent_asian_raw_value",
  "percent_hispanic_raw_value",
  "percent_native_hawaiian_or_other_pacific_islander_raw_value",
  "percent_non_hispanic_black_raw_value",
  "percent_non_hispanic_white_raw_value"
)

threshold_top1 <- 0.7
threshold_top2 <- 0.6

race_ranks <- cn_counties_national_subset |>
  pivot_longer(
    cols = all_of(race_cols),
    names_to = "race",
    values_to = "percent"
  ) |>
  group_by(state_fips_code, county_fips_code) |>
  arrange(desc(percent)) |>
  mutate(rank = row_number()) |>
  ungroup()

top_races_summary <- race_ranks |>
  filter(rank <= 2) |>
  group_by(state_fips_code, county_fips_code) |>
  summarise(
    top_races = paste0(
      gsub("percent_|_raw_value", "", race),
      collapse = ", "
    ),
    top1_race = gsub("percent_|_raw_value", "", race[1]),
    top1_percent = percent[1],
    top2_total_percent = sum(percent, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    racial_makeup = case_when(
      top1_percent >= threshold_top1 ~ top1_race,
      top2_total_percent >= threshold_top2 ~ top_races,
      TRUE ~ "no clear racial majority"
    )
  ) |>
  dplyr::select(state_fips_code, county_fips_code, racial_makeup)

cn_counties_national_subset <- cn_counties_national_subset |>
  left_join(top_races_summary, by = c("state_fips_code", "county_fips_code"))

predict_cn_subset <- cn_counties_national_subset |> 
  dplyr::select(
    state_abbreviation, name, population_raw_value, preventable_hospital_stays_raw_value,
    uninsured_raw_value, dentists_raw_value, other_primary_care_providers_raw_value,
    broadband_access_raw_value, mental_health_providers_raw_value,
    primary_care_physicians_raw_value, mammography_screening_raw_value,
    severe_housing_problems_raw_value, high_school_completion_raw_value,
    unemployment_raw_value,
    percent_disability_functional_limitations_raw_value, percent_not_proficient_in_english_raw_value,
    percent_rural_raw_value, racial_makeup, income_inequality_raw_value, median_household_income_raw_value,
    percent_american_indian_or_alaska_native_raw_value,
    percent_asian_raw_value, percent_hispanic_raw_value, percent_native_hawaiian_or_other_pacific_islander_raw_value,
    percent_non_hispanic_black_raw_value, percent_non_hispanic_white_raw_value
  ) |>
  mutate(uninsured_raw_value_pct = uninsured_raw_value * 100)

#RANDOM FOREST VARIABLE IMPORTANCE PLOT

# Create model-ready subset
rf_subset <- predict_cn_subset |> 
  dplyr::select(
    preventable_hospital_stays_raw_value,
    primary_care_physicians_raw_value,
    uninsured_raw_value,
    mental_health_providers_raw_value,
    other_primary_care_providers_raw_value,
    dentists_raw_value,
    mammography_screening_raw_value,
    severe_housing_problems_raw_value,
    high_school_completion_raw_value,
    percent_disability_functional_limitations_raw_value,
    percent_not_proficient_in_english_raw_value,
    percent_rural_raw_value,
    unemployment_raw_value
  ) |> 
  na.omit()

# Create random forest model
set.seed(42)
rf_model <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_subset,
  importance = TRUE,
  ntree = 500
)

# Variable name mapping
var_name_lookup <- c(
  "primary_care_physicians_raw_value" = "Primary Care Physicians",
  "uninsured_raw_value" = "Uninsured Rate",
  "mental_health_providers_raw_value" = "Mental Health Providers",
  "dentists_raw_value" = "Dentists",
  "other_primary_care_providers_raw_value" = "Other Primary Care Providers",
  "mammography_screening_raw_value" = "Mammography Screening",
  "severe_housing_problems_raw_value" = "Severe Housing Problems",
  "high_school_completion_raw_value" = "High School Completion",
  "percent_disability_functional_limitations_raw_value" = "Disability Rate",
  "percent_not_proficient_in_english_raw_value" = "Limited English Proficiency",
  "percent_rural_raw_value" = "Rural Population (%)",
  "unemployment_raw_value" = "Unemployment Rate"
)

# Create importance plot
imp <- importance(rf_model)
imp_df <- data.frame(
  Variable = rownames(imp),
  Importance = imp[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup)) |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 10)

# Plot variable importance
ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance, fill = Variable)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top Predictors of Preventable Hospital Stays",
    subtitle = "Random Forest Variable Importance",
    x = "Predictor Variable",
    y = "Importance (% Increase in MSE)",
    caption = "Data: County Health Rankings 2023"
  ) +
  scale_fill_viridis_d(option = "plasma") +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


############
# [Previous code remains the same until after predict_cn_subset is created]

# Create subsets for White and POC majority counties
white_counties <- predict_cn_subset |> 
  filter(racial_makeup == "non_hispanic_white")

poc_counties <- predict_cn_subset |> 
  filter(racial_makeup %in% c("non_hispanic_black", "hispanic", 
                              "non_hispanic_black, non_hispanic_white",
                              "hispanic, non_hispanic_white"))

# Create model-ready subsets
rf_white <- white_counties |> 
  dplyr::select(
    preventable_hospital_stays_raw_value,
    primary_care_physicians_raw_value,
    uninsured_raw_value,
    mental_health_providers_raw_value,
    other_primary_care_providers_raw_value,
    dentists_raw_value,
    mammography_screening_raw_value,
    severe_housing_problems_raw_value,
    high_school_completion_raw_value,
    percent_disability_functional_limitations_raw_value,
    percent_not_proficient_in_english_raw_value,
    percent_rural_raw_value,
    unemployment_raw_value
  ) |> 
  na.omit()

rf_poc <- poc_counties |> 
  dplyr::select(
    preventable_hospital_stays_raw_value,
    primary_care_physicians_raw_value,
    uninsured_raw_value,
    mental_health_providers_raw_value,
    other_primary_care_providers_raw_value,
    dentists_raw_value,
    mammography_screening_raw_value,
    severe_housing_problems_raw_value,
    high_school_completion_raw_value,
    percent_disability_functional_limitations_raw_value,
    percent_not_proficient_in_english_raw_value,
    percent_rural_raw_value,
    unemployment_raw_value
  ) |> 
  na.omit()

# Variable name mapping
var_name_lookup <- c(
  "primary_care_physicians_raw_value" = "Primary Care Physicians",
  "uninsured_raw_value" = "Uninsured Rate",
  "mental_health_providers_raw_value" = "Mental Health Providers",
  "dentists_raw_value" = "Dentists",
  "other_primary_care_providers_raw_value" = "Other Primary Care Providers",
  "mammography_screening_raw_value" = "Mammography Screening",
  "severe_housing_problems_raw_value" = "Severe Housing Problems",
  "high_school_completion_raw_value" = "High School Completion",
  "percent_disability_functional_limitations_raw_value" = "Disability Rate",
  "percent_not_proficient_in_english_raw_value" = "Limited English Proficiency",
  "percent_rural_raw_value" = "Rural Population (%)",
  "unemployment_raw_value" = "Unemployment Rate"
)

# Create models
set.seed(42)
rf_model_white <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_white,
  importance = TRUE,
  ntree = 500
)

set.seed(42)
rf_model_poc <- randomForest(
  preventable_hospital_stays_raw_value ~ .,
  data = rf_poc,
  importance = TRUE,
  ntree = 500
)

# Create importance data frames
imp_white <- importance(rf_model_white)
imp_df_white <- data.frame(
  Variable = rownames(imp_white),
  Importance = imp_white[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup),
         Group = "White Majority Counties") |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

imp_poc <- importance(rf_model_poc)
imp_df_poc <- data.frame(
  Variable = rownames(imp_poc),
  Importance = imp_poc[,"%IncMSE"]
) |> 
  mutate(Variable = str_replace_all(Variable, var_name_lookup),
         Group = "POC Majority Counties") |> 
  arrange(desc(Importance)) |> 
  slice_head(n = 5)

# Combine for plotting
imp_combined <- bind_rows(imp_df_white, imp_df_poc)

# Create comparison plot
ggplot(imp_combined, aes(x = reorder(Variable, Importance), y = Importance, fill = Group)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Predictors of Preventable Hospital Stays",
    subtitle = "Comparison Between White and POC Majority Counties",
    x = "Predictor Variable",
    y = "Importance (% Increase in MSE)",
    caption = "Data: County Health Rankings 2023 | Analysis: Random Forest"
  ) +
  scale_fill_manual(values = c("White Majority Counties" = "#1f77b4", 
                               "POC Majority Counties" = "#ff7f0e")) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    legend.position = "top",
    legend.title = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

# Create separate plots for each group
# White Majority Counties Plot with enhanced labels
white_plot <- ggplot(imp_df_white, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "viridis") +
  labs(
    x = "Predictor Variable",
    y = "Importance (% Increase in MSE)",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 28),  # Double size and bold
    axis.title.y = element_text(face = "bold", size = 28),  # Double size and bold
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

# POC Majority Counties Plot with enhanced labels
poc_plot <- ggplot(imp_df_poc, aes(x = reorder(Variable, Importance), y = Importance, fill = Importance)) +
  geom_col() +
  coord_flip() +
  scale_fill_viridis_c(option = "magma") +
  labs( 
    x = "Predictor Variable",
    y = "Importance (% Increase in MSE)",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title.x = element_text(face = "bold", size = 28),  # Double size and bold
    axis.title.y = element_text(face = "bold", size = 28),  # Double size and bold
    axis.text.x = element_text(size = 20),
    axis.text.y = element_text(size = 20),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    legend.position = "none"
  )

# Display plots side by side
library(patchwork)
white_plot + poc_plot + plot_layout(ncol = 2)

white_plot
poc_plot
