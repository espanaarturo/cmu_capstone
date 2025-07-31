source("dataPrep.R")
## Data ##
data <- national_data
# Load required libraries
library(patchwork)    
library(ggridges) 

# 1. Histograms of Key Predictors

# Define predictors to visualize
predictors <- c(
  "physician_supply_per_10k", "mental_health_providers_per_10k",
  "dentists_per_10k", "uninsured_raw_value",
  "mammography_screening_white", "mammography_screening_poc",
  "income_inequality", "unemployment_rate",
  "percent_rural_raw_value", "high_school_completion_raw_value"
)

# Human-readable labels for each plot
predictor_labels <- c(
  "Primary Care Physicians\n(per 10k)", "Mental Health Providers\n(per 10k)",
  "Dentists\n(per 10k)", "Uninsured Rate",
  "Mammography Screening\n(White)", "Mammography Screening\n(POC)",
  "Income Inequality\n(Gini Index)", "Unemployment Rate",
  "Percent Rural", "High School Completion"
)

# Create histograms with density curves
eda_plots <- lapply(seq_along(predictors), function(i) {
  var <- predictors[i]
  label <- predictor_labels[i]
  
  p <- ggplot(analysis_data, aes_string(x = var)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30,
                   fill = "#1f77b4", color = "white", alpha = 0.8) +
    geom_density(color = "#ff7f0e", linewidth = 1) +
    labs(x = label, y = "Density") +
    theme_minimal(base_size = 12) +
    theme(axis.text.y = element_blank(), panel.grid.major.y = element_blank())

  # Apply pseudo-log scale if variable is right-skewed
  if (median(analysis_data[[var]], na.rm = TRUE) < mean(analysis_data[[var]], na.rm = TRUE)) {
    p <- p + scale_x_continuous(trans = scales::pseudo_log_trans())
  }
  
  return(p)
})

# Display all histograms in a grid
histo <- wrap_plots(eda_plots, ncol = 3) +
  plot_annotation(title = "Figure 1. Distribution of Healthcare Access and Socioeconomic Predictors")
histo


# 2. Violin Plot of Outcomes by Race

violin_plot <- analysis_data %>%
  mutate(rate_white = preventable_hospital_stays_white,
         rate_poc = preventable_hospital_stays_poc) %>%
  pivot_longer(cols = c(rate_white, rate_poc), names_to = "group", values_to = "hospitalization_rate") %>%
  mutate(group = factor(ifelse(group == "rate_white", "White", "POC"))) %>%
  ggplot(aes(x = group, y = hospitalization_rate, fill = group)) +
  geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), color = "white", alpha = 0.7) +
  geom_boxplot(width = 0.1, fill = "white", outlier.shape = NA) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "yellow", color = "black") +
  scale_fill_manual(values = c("White" = "#377eb8", "POC" = "#e41a1c"), guide = "none") +
  labs(subtitle = "Hospital stays per 100,000 population", x = "Racial Group", y = "Hospital Stays") +
  theme_minimal(base_size = 14)
print(violin_plot)


# 3. Distribution of Access Variables (Before and After Log Transform)


# Plot before transformation
access_vars <- c("physician_supply_per_10k", "mental_health_providers_per_10k",
                 "dentists_per_10k", "uninsured_raw_value", 
                 "mammography_screening_white", "mammography_screening_poc")

for (var in access_vars) {
  print(
    ggplot(analysis_data, aes_string(x = var)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      labs(title = paste("Distribution of", var), x = var, y = "Number of Counties") +
      theme_minimal()
  )
}

# Apply log1p transformation to skewed variable
model_analysis_data <- analysis_data |>
  mutate(mental_health_providers_per_10k = ifelse(mental_health_providers_per_10k < 0, NA, mental_health_providers_per_10k)) |>
  mutate(log_mental_health_per_10k = log1p(mental_health_providers_per_10k)) |>
  drop_na(log_mental_health_per_10k)


# 4. Scatterplot: Uninsured Rate vs Preventable Hospitalizations


plot_data <- analysis_data |>
  mutate(
    uninsured_pct = uninsured_raw_value * 100,
    rate_white = preventable_hospital_stays_white / pop_white * 10000,
    rate_poc   = preventable_hospital_stays_poc / pop_poc * 10000
  ) |>
  filter(pop_white > 1000, pop_poc > 1000, rate_white < 300, rate_poc < 300)

ggplot(plot_data) +
  geom_point(aes(x = uninsured_pct, y = rate_white, color = "White"), alpha = 0.6) +
  geom_point(aes(x = uninsured_pct, y = rate_poc, color = "POC"), alpha = 0.6) +
  geom_smooth(aes(x = uninsured_pct, y = rate_white, color = "White"), method = "lm", se = FALSE) +
  geom_smooth(aes(x = uninsured_pct, y = rate_poc, color = "POC"), method = "lm", se = FALSE) +
  labs(title = "Uninsured Rate vs Preventable Hospitalizations",
       x = "Uninsured Rate (%)", y = "Hospital Stays per 10,000",
       color = "Racial Group") +
  scale_color_manual(values = c("White" = "#377eb8", "POC" = "#e41a1c")) +
  theme_minimal()


# 5. Choropleth Map: Disparity Ratio (POC/White)


library(usmap)
library(viridis)

state_map_data <- analysis_data |>
  group_by(state = state_abbreviation) |>
  summarize(
    white_rate = sum(preventable_hospital_stays_white, na.rm = TRUE) / sum(pop_white, na.rm = TRUE) * 10000,
    poc_rate   = sum(preventable_hospital_stays_poc, na.rm = TRUE) / sum(pop_poc, na.rm = TRUE) * 10000,
    disparity_ratio = poc_rate / white_rate
  ) |>
  filter(is.finite(disparity_ratio), !is.na(state))

plot_usmap(data = state_map_data, values = "disparity_ratio") +
  scale_fill_viridis_c(name = "POC/White Disparity Ratio", trans = "log10", option = "magma") +
  labs(title = "State-Level Disparities in Preventable Hospital Stays")


# 6. Correlation Heatmap


cor_matrix <- analysis_data |>
  dplyr::select(
    preventable_hospital_stays_white, preventable_hospital_stays_poc,
    physician_supply_per_10k, mental_health_providers_per_10k,
    dentists_per_10k, uninsured_raw_value, income_inequality,
    percent_rural_raw_value, mammography_screening_white, mammography_screening_poc
  ) |>
  na.omit() |>
  cor()

cor_long <- as.data.frame(cor_matrix) |>
  rownames_to_column("Var1") |>
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")

ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "#d73027", high = "#1a9850", mid = "white", midpoint = 0, limits = c(-1,1)) +
  labs(title = "Correlation Heatmap") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 7. Regression Plot: Predictors by Race


# Standardize predictors
clean_data <- analysis_data |>
  mutate(
    rate_white = preventable_hospital_stays_white / pop_white * 10000,
    rate_poc = preventable_hospital_stays_poc / pop_poc * 10000
  ) |>
  filter(is.finite(rate_white), is.finite(rate_poc)) |>
  mutate(across(
    .cols = c(physician_supply_per_10k, mental_health_providers_per_10k, 
              dentists_per_10k, uninsured_raw_value, mammography_screening_white,
              mammography_screening_poc, income_inequality, unemployment_rate,
              percent_rural_raw_value, high_school_completion_raw_value),
    .fns = ~ scale(.)[,1],
    .names = "z_{.col}"
  ))

# Separate models by group
model_white <- lm(rate_white ~ z_physician_supply_per_10k + z_mental_health_providers_per_10k +
                    z_dentists_per_10k + z_uninsured_raw_value + z_mammography_screening_white +
                    z_income_inequality + z_unemployment_rate + z_percent_rural_raw_value + 
                    z_high_school_completion_raw_value, data = clean_data)

model_poc <- lm(rate_poc ~ z_physician_supply_per_10k + z_mental_health_providers_per_10k +
                  z_dentists_per_10k + z_uninsured_raw_value + z_mammography_screening_poc +
                  z_income_inequality + z_unemployment_rate + z_percent_rural_raw_value + 
                  z_high_school_completion_raw_value, data = clean_data)

# Visualize regression coefficients
bind_rows(
  tidy(model_white) |> mutate(group = "White"),
  tidy(model_poc) |> mutate(group = "POC")
) |>
  filter(term != "(Intercept)") |>
  mutate(term = recode(term,
    "z_physician_supply_per_10k" = "Physician Supply",
    "z_mental_health_providers_per_10k" = "Mental Health Providers",
    "z_dentists_per_10k" = "Dentists",
    "z_uninsured_raw_value" = "Uninsured Rate",
    "z_mammography_screening_white" = "Mammography (White)",
    "z_mammography_screening_poc" = "Mammography (POC)",
    "z_income_inequality" = "Income Inequality",
    "z_unemployment_rate" = "Unemployment Rate",
    "z_percent_rural_raw_value" = "Percent Rural",
    "z_high_school_completion_raw_value" = "HS Completion"
  )) |>
  ggplot(aes(x = estimate, y = term, color = group)) +
  geom_point(position = position_dodge(0.5), size = 3) +
  geom_errorbarh(aes(xmin = estimate - std.error, xmax = estimate + std.error),
                 position = position_dodge(0.5), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Predictors of Preventable Hospitalizations by Racial Group",
       x = "Standardized Effect Size", y = "", color = "Group") +
  theme_light()
