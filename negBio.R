source("dataPrep.R")
source("EDA.R")

# Load required libraries
library(MASS)         
library(glmmTMB)      
library(car)          
library(DHARMa)       
library(broom)        
library(knitr)       
library(sandwich)     
library(lmtest)       
library(kableExtra) 

# Step 1: Helper Functions

# Function to fit NB model with glm.nb or fallback to glmmTMB
fit_nb_model <- function(data, outcome, offset_var, screening_var) {
  formula <- as.formula(paste(
    outcome, "~ physician_supply_per_10k + mental_health_providers_per_10k +",
    "dentists_per_10k + uninsured_raw_value +", screening_var, "+",
    "income_inequality + log_unemployment + percent_rural_raw_value +",
    "high_school_completion_raw_value + offset(", offset_var, ")"
  ))
  tryCatch({
    nb_model <- glm.nb(formula, data = data, control = glm.control(maxit = 100))
    message("✅ glm.nb succeeded for ", outcome)
    return(nb_model)
  }, error = function(e) {
    message("⚠️ glm.nb failed for ", outcome, " — using glmmTMB")
    return(glmmTMB(formula, family = nbinom2, data = data))
  })
}

# Function to prepare data: log transform unemployment, offset, and scale predictors
prep_data <- function(data, outcome, pop_var, screening_var) {
  data |>
    mutate(
      !!sym(paste0("log_", pop_var)) := log(!!sym(pop_var)),
      log_unemployment = log(unemployment_rate + 1)
    ) |>
    dplyr::select(
      !!sym(outcome),
      !!sym(paste0("log_", pop_var)),
      physician_supply_per_10k,
      mental_health_providers_per_10k,
      dentists_per_10k,
      uninsured_raw_value,
      !!sym(screening_var),
      income_inequality,
      log_unemployment,
      percent_rural_raw_value,
      high_school_completion_raw_value
    ) |>
    mutate(across(
      -c(!!sym(outcome), !!sym(paste0("log_", pop_var))),
      ~ (. - min(.)) / (max(.) - min(.))  # Min-max scaling
    )) |>
    filter(if_all(everything(), is.finite))  # Drop infinite or NA
}


# Step 2: Prepare & Fit Models


# Prepare data
model_data_white <- prep_data(analysis_data, "preventable_hospital_stays_white", "pop_white", "mammography_screening_white")
model_data_poc   <- prep_data(analysis_data, "preventable_hospital_stays_poc",   "pop_poc",   "mammography_screening_poc")

# Fit models
model_white <- fit_nb_model(model_data_white, "preventable_hospital_stays_white", "log_pop_white", "mammography_screening_white")
model_poc   <- fit_nb_model(model_data_poc,   "preventable_hospital_stays_poc",   "log_pop_poc",   "mammography_screening_poc")


# Step 3: Multicollinearity Check

cat("\nVariance Inflation Factors (White):\n")
print(vif(lm(preventable_hospital_stays_white ~ ., data = model_data_white)))

cat("\nVariance Inflation Factors (POC):\n")
print(vif(lm(preventable_hospital_stays_poc ~ ., data = model_data_poc)))


# Step 4: IRR Table Extraction

summarize_model <- function(model, label) {
  if ("glmmTMB" %in% class(model)) {
    est <- fixef(model)$cond
    ci <- confint(model, method = "Wald")[names(est), ]
  } else {
    est <- coef(model)
    ci <- confint(model)
  }
  irrs <- exp(cbind(IRR = est, Lower = ci[,1], Upper = ci[,2]))
  as.data.frame(irrs) |>
    rownames_to_column("Variable") |>
    filter(Variable != "(Intercept)") |>
    mutate(Race = label, across(where(is.numeric), round, 3))
}

result_white <- summarize_model(model_white, "White")
result_poc   <- summarize_model(model_poc, "POC")

# Display IRRs
bind_rows(result_white, result_poc) |> knitr::kable(caption = "IRRs for Preventable Hospitalizations")

# Step 5: Fit Statistics

get_fit_metrics <- function(model) {
  list(AIC = AIC(model), BIC = BIC(model), deviance = deviance(model))
}

cat("\nModel Fit (White):\n"); print(get_fit_metrics(model_white))
cat("\nModel Fit (POC):\n"); print(get_fit_metrics(model_poc))

# Step 6: DHARMa Residuals


simulate_and_plot <- function(model, label) {
  if ("glmmTMB" %in% class(model)) {
    sim <- simulateResiduals(model)
    plot(sim, main = paste("DHARMa Residuals -", label))
  } else {
    message("Skipping DHARMa for glm.nb model:", label)
  }
}

quartz(); simulate_and_plot(model_white, "White")
quartz(); simulate_and_plot(model_poc, "POC")


# Step 7: Robust Standard Errors


robust_se <- function(model) {
  coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC0"))
}

cat("\nRobust SEs (White):\n")
if (!"glmmTMB" %in% class(model_white)) print(robust_se(model_white))

cat("\nRobust SEs (POC):\n")
if (!"glmmTMB" %in% class(model_poc)) print(robust_se(model_poc))

# Step 8: Residual Plot and Outlier Checks

check_residuals <- function(model_data, model, outcome, label) {
  model_data <- model_data |>
    mutate(resid = residuals(model, type = "pearson"),
           fitted = predict(model, type = "response"))
  
  cat("\nTop 5 Pearson Residuals -", label, "\n")
  model_data |>
    mutate(abs_resid = abs(resid)) |>
    arrange(desc(abs_resid)) |>
    dplyr::select(!!sym(outcome), resid, fitted, abs_resid) |>
    slice_head(n = 5) |> print()
  
  ggplot(model_data, aes(x = fitted, y = resid)) +
    geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = paste("Residuals vs Fitted -", label), x = "Fitted", y = "Pearson Residuals") +
    theme_minimal()
}

quartz(); check_residuals(model_data_white, model_white, "preventable_hospital_stays_white", "White")
quartz(); check_residuals(model_data_poc, model_poc, "preventable_hospital_stays_poc", "POC")


# Step 9: IRR Plot by Race


# Clean variable labels
combined_irrs <- bind_rows(result_white, result_poc) %>%
  mutate(Variable = recode(Variable,
    "physician_supply_per_10k" = "Physician Supply",
    "mental_health_providers_per_10k" = "Mental Health Providers",
    "dentists_per_10k" = "Dentists",
    "uninsured_raw_value" = "Uninsured Rate",
    "mammography_screening_white" = "Mammography (White)",
    "mammography_screening_poc" = "Mammography (POC)",
    "income_inequality" = "Income Inequality",
    "log_unemployment" = "Log Unemployment",
    "percent_rural_raw_value" = "% Rural",
    "high_school_completion_raw_value" = "HS Completion"
  ))

# Plot IRRs with CIs
ggplot(combined_irrs, aes(x = IRR, y = fct_reorder(Variable, IRR), color = Race)) +
  geom_point(size = 3, position = position_dodge(0.5)) +
  geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2, position = position_dodge(0.5)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray40") +
  labs(
    title = "IRRs for Preventable Hospital Stays",
    subtitle = "95% Confidence Intervals by Racial Group",
    x = "IRR", y = "Predictor", color = "Race"
  ) +
  scale_color_manual(values = c("White" = "#377eb8", "POC" = "#e41a1c")) +
  theme_minimal(base_size = 14)
