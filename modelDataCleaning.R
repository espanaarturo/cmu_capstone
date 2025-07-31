library(tidyverse)
library(sf)           
library(spdep)        
library(mgcv)         
library(xgboost)      
library(SHAPforxgboost) 
library(mice)         
library(patchwork)    
library(ggdag)      
library(MASS)
library(broom)
library(dplyr)
source("EDA_import.R")


## Data ##
data <- national_data
# Load required libraries
library(tidyverse)
library(MASS)  # For negative binomial regression
library(broom)







# view(national_data)
# national_data |>
#   dplyr::select(`Uninsured Adults raw value`)












## make a subset ##
library(janitor) 

subset_data <- national_data |>
  clean_names() |>
  slice(-1) |>
  dplyr::select(
    state_abbr = state_abbreviation,
    county_name = name,
    fips = x5_digit_fips_code,
    preventable_stays = preventable_hospital_stays_raw_value,
    uninsured = uninsured_raw_value,
    primary_care = primary_care_physicians_raw_value,
    mammogram = mammography_screening_raw_value,
    broadband = broadband_access_raw_value,
    pct_white = percent_non_hispanic_white_raw_value,
    pct_black = percent_non_hispanic_black_raw_value,
    pct_hispanic = percent_hispanic_raw_value,
    pct_native = percent_american_indian_or_alaska_native_raw_value,
    pct_asian = percent_asian_raw_value,
    pct_islander = percent_native_hawaiian_or_other_pacific_islander_raw_value,
    population = population_raw_value,
    mental = mental_health_providers_raw_value,
    dentists = dentists_raw_value,
    otherproviders = other_primary_care_providers_raw_value,
    flu = flu_vaccinations_raw_value
  ) |>
  mutate(
    across(
      c(preventable_stays, uninsured, primary_care, mammogram,
        broadband, pct_white, pct_black, pct_hispanic, pct_native, pct_asian, pct_islander, population, mental, dentists, otherproviders, flu),
      as.numeric
    ),
    log_population = log(population)
  ) |>
  
  drop_na(
    preventable_stays, uninsured, primary_care, mammogram,
    broadband, pct_white, pct_black, pct_hispanic, pct_native, pct_asian, pct_islander, log_population, mental, dentists, otherproviders, flu
  )

subset_data |>
  dplyr::select(pct_white)
#nrow(subset_data) # 3204
# nrow(subset_data) # 2966 after na drop

# data is clean now 

# modeling 

# check for assumptions
mean(subset_data$preventable_stays)  # 2817.195
var(subset_data$preventable_stays)   # 113761
#checking for linearity
# check for preventable_stays, uninsured, primary_care, mammogram,
# broadband, pct_white, pct_black, pct_hispanic, pct_native, pct_asian, pct_islander, log_population
subset_data <- subset_data |>
  mutate(log_preventable = log(preventable_stays + 1))
library(ggplot2)
# add transformation to uninsured to meet linearity (log is best)
uninsured_linerity <- ggplot(subset_data, aes(x = uninsured, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "Uninsured Rate vs. Log(Preventable Stays + 1)",
       x = "Uninsured Rate", y = "Log(Preventable Stays + 1)")
log_uninsured_linerity <- ggplot(subset_data, aes(x = (log(uninsured) + 1), y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "log Uninsured Rate vs. Log(Preventable Stays + 1)",
       x = " log Uninsured Rate", y = "Log(Preventable Stays + 1)")
sqrt_uninsured_linerity <- ggplot(subset_data, aes(x = sqrt(uninsured), y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "sqrt Uninsured Rate vs. Log(Preventable Stays + 1)",
       x = "sqrt Uninsured Rate", y = "Log(Preventable Stays + 1)")
library(patchwork)
uninsured_linerity | log_uninsured_linerity | sqrt_uninsured_linerity +
  plot_layout(guides = "collect")

# add transformation to primary care to meet linearity (go with primary care)
primarycare_linerity <- ggplot(subset_data, aes(x = primary_care, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "primary_care vs. Log(Preventable Stays + 1)",
       x = "primary_care", y = "Log(Preventable Stays + 1)")
log_primarycare_linerity <- ggplot(subset_data, aes(x = (log(primary_care)+1), y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "primary_care vs. Log(Preventable Stays + 1)",
       x = "primary_care", y = "Log(Preventable Stays + 1)")
sqrt_primarycare_linerity <- ggplot(subset_data, aes(x = sqrt(primary_care), y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "sqrt primary_care vs. Log(Preventable Stays + 1)",
       x = "sqrt primary_care", y = "Log(Preventable Stays + 1)")
primarycare_linerity | primarycare_linerity | primarycare_linerity +
  plot_layout(guides = "collect")

ggplot(subset_data, aes(x = mammogram, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "mammogram vs. Log(Preventable Stays + 1)",
       x = "mammogram", y = "Log(Preventable Stays + 1)")

# add transformation to broadband to meet linearity (go with quadratic fit)
broadband_linerity <- ggplot(subset_data, aes(x = broadband, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "broadband vs. Log(Preventable Stays + 1)",
       x = "broadband", y = "Log(Preventable Stays + 1)")
log_broadband_linerity <- ggplot(subset_data, aes(x = (log(broadband)+1), y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "broadband vs. Log(Preventable Stays + 1)",
       x = "broadband", y = "Log(Preventable Stays + 1)")
sqrt_broadband_linerity <- ggplot(subset_data, aes(x = sqrt(broadband), y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "broadband vs. Log(Preventable Stays + 1)",
       x = "broadband", y = "Log(Preventable Stays + 1)")
poly_broadband_linearity <- ggplot(subset_data, aes(x = broadband, y = log_preventable)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "purple") + # Quadratic fit
  geom_smooth(method = "lm", se = FALSE, color = "blue") + 
  labs(title = "Broadband vs. Log(Preventable Stays) (Quadratic Fit)",
       x = "Broadband Access", y = "Log(Preventable Stays + 1)")
broadband_linerity | log_broadband_linerity | sqrt_broadband_linerity | poly_broadband_linearity+
  plot_layout(guides = "collect")

ggplot(subset_data, aes(x = pct_white, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "pct_white vs. Log(Preventable Stays + 1)",
       x = "pct_white", y = "Log(Preventable Stays + 1)")
ggplot(subset_data, aes(x = pct_black, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "pct_black vs. Log(Preventable Stays + 1)",
       x = "pct_black", y = "Log(Preventable Stays + 1)")
ggplot(subset_data, aes(x = pct_hispanic, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "pct_hispanic vs. Log(Preventable Stays + 1)",
       x = "pct_hispanic", y = "Log(Preventable Stays + 1)")
### native is missing
ggplot(subset_data, aes(x = pct_native, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "pct_native vs. Log(Preventable Stays + 1)",
       x = "pct_native", y = "Log(Preventable Stays + 1)")
ggplot(subset_data, aes(x = pct_asian, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "pct_asian vs. Log(Preventable Stays + 1)",
       x = "pct_asian", y = "Log(Preventable Stays + 1)")
ggplot(subset_data, aes(x = pct_islander, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "pct_islander vs. Log(Preventable Stays + 1)",
       x = "pct_islander", y = "Log(Preventable Stays + 1)")
ggplot(subset_data, aes(x = log_population, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "log_population vs. Log(Preventable Stays + 1)",
       x = "log_population", y = "Log(Preventable Stays + 1)")
# multicollenarity 
subset_data %>% 
  dplyr::select(
    preventable_stays, uninsured, primary_care, mammogram,
    broadband, pct_white, pct_black, pct_hispanic, pct_native, pct_asian, pct_islander, log_population, mental, dentists, otherproviders, flu
  ) %>% 
  str()  # Inspect structure
cor_matrix <- cor(subset_data %>% dplyr::select(
  uninsured, primary_care, mammogram, broadband,
  pct_white, pct_black, pct_hispanic, pct_native, 
  pct_asian, pct_islander, log_population
))
cor_matrix
# pctwhite and pctblack : -0.61
# white and Hispanic: -0.62
#black and white = -0.61


# fitting model
# Include in model: log(uninsured)
# pimary_care, mammogram, quadratic fit for broadband (y~polu(x,2)  
# include mental health providers, densitsts, other primary care providers, flu vaccinations, broadband  

uninsured_linerity <- ggplot(subset_data, aes(x = (log(mental)+1), y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "mental vs. Log(Preventable Stays + 1)",
       x = "mental", y = "Log(Preventable Stays + 1)")
uninsured_linerity

uninsured_linerity <- ggplot(subset_data, aes(x = flu, y = log_preventable)) +
  geom_point() +  
  geom_smooth(method = "loess", se = FALSE, color = "red") +  
  geom_smooth(method = "lm", se = FALSE, color = "blue") +    
  labs(title = "flu vs. Log(Preventable Stays + 1)",
       x = "flus", y = "Log(Preventable Stays + 1)")
uninsured_linerity

# use log(uninsured_linerity) + 1 , log(dentists) +1, log(otherproviders) + 1), flu

library(MASS)
# Rescale problematic predictors (divide by 100 or 1000)


model_data <- subset_data |>
  mutate(
    dentists = dentists/1000,
    otherproviders = otherproviders/1000,
    primary_care = primary_care/1000,
    log_uninsured = log(uninsured + 1),
    log_dentists = log(dentists + 1),
    log_otherproviders = log(otherproviders + 1),
    log_mental = log(mental + 1)
  )

# in include serv housing problems
# look into multilevel modeling random effect modeling. 
final_model <- glm.nb(
  preventable_stays ~ 
    log_uninsured +          
    log_dentists  +           
    log_otherproviders +    
    poly(broadband, 2) +          
    log_mental  +             
    primary_care +                
    mammogram,       
  data = model_data
)

summary(final_model)

plot(fitted(final_model), residuals(final_model, type = "pearson"),
     main = "Residuals vs Fitted")
abline(h = 0,col = "red")

qqnorm(residuals(final_model, type = "pearson"))
qqline(residuals(final_model, type = "pearson"))

plot(cooks.distance(final_model), type = "h",
     main = "Cook's Distance")



# Prepare long-form data for racial group comparison
library(tidyr)
library(ggplot2)

eda_long <- eda_data %>%
  dplyr::select(uninsured, phs_white, phs_black) %>%
  pivot_longer(
    cols = starts_with("phs_"), 
    names_to = "group", 
    values_to = "phs"
  ) %>%
  mutate(
    group = recode(
      group,
      "phs_white" = "White",
      "phs_black" = "Black"
    )
  )

# Plot with customized y-axis breaks
ggplot(eda_long, aes(x = uninsured, y = phs, color = group)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, size = 1.1) +
  scale_color_manual(values = c("White" = "#3366CC", "Black" = "#CC0000")) +
  scale_y_continuous(
    trans = "log1p",
    breaks = c(0, 100, 500, 1000, 5000, 10000, 20000, 40000, 60000),
    labels = scales::comma,
    limits = c(NA, max(eda_long$phs, na.rm = TRUE)) # Keep upper limit dynamic
  ) +
  labs(
    x = "Uninsured Rate",
    y = "Preventable Hospital Stays (log scale)",
    color = "Racial Group",
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank() # Cleaner look
  )



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
