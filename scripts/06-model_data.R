
#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####

library(arrow)
library(ggplot2)
library(tidyverse)
library(lme4)
library(Matrix)
library(brms)
library(caret)
library(Metrics)
library(loo)


#### Read data ####

data <- read_parquet("../red-camera-charge/data/02-analysis_data/analysis_data.parquet")
data_long <- read_parquet("../red-camera-charge/data/02-analysis_data/analysis_data_long.parquet")

data <- data %>%
  mutate_at(vars(contains("Year")), as.numeric)
data <- data %>%
  mutate(Enforcement_Start_Date = as.Date(Enforcement_Start_Date))
data <- data %>%
  mutate(Location_Code = as.numeric(Location_Code))

data_long <- data_long %>%
  mutate_at(vars(contains("Year")), as.numeric)
data_long <- data_long %>%
  mutate(Enforcement_Start_Date = as.Date(Enforcement_Start_Date))
data_long <- data_long %>%
  mutate(Fines = as.numeric(Fines))
data_long <- data_long %>%
  mutate(Location_Code = as.numeric(Location_Code))

glimpse(data)
glimpse(data_long)


#### Prior Distribution ####

prior_samples <- data.frame(
  beta_0 = rnorm(1000, 1000, 500),
  beta_1 = rnorm(1000, 0, 10),
  beta_2 = rnorm(1000, 0, 50),
  beta_3 = rnorm(1000, 0, 10)
)

# Convert to long format
prior_samples_long <- pivot_longer(prior_samples, cols = everything(), 
                                   names_to = "Parameter", values_to = "Value")

# Plot density
ggplot(prior_samples_long, aes(x = Value, fill = Parameter)) +
  geom_density(alpha = 0.5) +
  labs(title = "Prior Distributions for Beta Parameters", 
       x = "Parameter Value", y = "Density", fill = "Parameter") +
  theme_minimal()


#### Model ####

## Does enforcement start time have a significant impact on regional fine trends?

set.seed(304)

# Create an 80% index of the training set
train_indices <- createDataPartition(data_long$Fines, p = 0.8, list = FALSE)

# Split data
train_data <- data_long[train_indices, ]
test_data <- data_long[-train_indices, ]

# Bayesian hierarchical model
bayesian_model <- brm(
  Fines ~ Year + Enforcement + (1 | Ward_Number),
  data = train_data,
  family = gaussian(),
  prior = c(
    prior(normal(1000, 500), class = "Intercept"),
    prior(normal(0, 10), class = "b", coef = "Year"),
    prior(normal(0, 50), class = "b", coef = "Enforcement"),
    prior(student_t(3, 0, 100), class = "sd"),
    prior(student_t(3, 0, 100), class = "sigma")),
  iter = 4000, warmup = 2000, chains = 4, cores = 4,
  seed = 304)


#### Check Convergence ####

summary(bayesian_model)

# Check MCMC Sampling
plot(bayesian_model)


#### Validation ####

predictions <- predict(bayesian_model, newdata = test_data)
predicted_values <- predictions[, "Estimate"]

rmse_value <- rmse(test_data$Fines, predicted_values)
rmse_value


#### Visualization ####

pp_check(bayesian_model)


#### Comparison ####

# Model without interaction
alternative_model <-brm(
  Fines ~ Year * Enforcement + (1 | Ward_Number),
  data = train_data,
  family = gaussian(),
  prior = c(
    prior(normal(1000, 500), class = "Intercept"),
    prior(normal(0, 10), class = "b", coef = "Year"),
    prior(normal(0, 50), class = "b", coef = "Enforcement"),
    prior(normal(0, 10), class = "b", coef = "Year:Enforcement"), 
    prior(student_t(3, 0, 100), class = "sd"),
    prior(student_t(3, 0, 100), class = "sigma")),
  iter = 4000, warmup = 2000, chains = 4, cores = 4,
  seed = 304)

loo(bayesian_model, alternative_model)


#### Posterior ####

posterior <- brm(
  Fines ~ Year + Enforcement + (1 | Ward_Number), 
  data = train_data,                          
  prior = c(                                  
    prior(normal(1000, 500), class = "Intercept"),         
    prior(normal(0, 10), class = "b", coef = "Year"),         
    prior(normal(0, 50), class = "b", coef = "Enforcement"), 
    prior(student_t(3, 0, 100), class = "sd"),             
    prior(student_t(3, 0, 100), class = "sigma")),
  iter = 4000, warmup = 2000, chains = 4, cores = 4,
  seed = 304,
  control = list(adapt_delta = 0.95, max_treedepth = 15))

# Check Rhat
rhat_vals <- rhat(posterior)
print(rhat_vals)
summary(rhat_vals)  

# Visualizations
plot(posterior) 
pp_check(posterior, nsamples = 100)


#### Save Model ####

saveRDS(
  posterior,
  file = "../red-camera-charge/models/posterior_model.rds")




