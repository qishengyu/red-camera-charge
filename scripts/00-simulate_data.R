
#### Preamble ####
# Purpose: Simulates a dataset of red-light camera fines, including fines data 
# for different wards and years, enforcement start dates, and locations.
# Author: Qisheng Yu
# Date: 27 November 2024
# Contact: qisheng.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `tidyverse` package must be installed.
# Any other information needed? Ensure you are working within the 
# `red-camera-charge` R project structure.


#### Workspace setup ####

library(tidyverse)
set.seed(304)


#### Simulate data ####

# Define parameters
wards <- 1:25  # Wards 1 to 25
years <- 2007:2024  # Years 2007 to 2024
locations <- c("Richmond St. and Parliament St.",
               "Lake Shore Blvd. and York St.",
               "Steeles Ave. and Carpenter Rd.",
               "Steeles Ave. and Hilda Ave.",
               "Albion Rd. and Silverstone Dr.",
               "Albion Rd. and Finch Ave.",
               "Dixon Rd. and Kipling Ave.",
               "Steeles Ave. and Islington Ave.",
               "Sheppard Ave. and Wilson Heights Blvd.",
               "Bathurst St. and Sheppard Ave.",
               "Lawrence Ave. and Marlee Ave.",
               "Lawrence Ave. and Bathurst St.",
               "Bayview Ave. and Cummer Ave.",
               "Finch Ave. and Willowdale Ave.",
               "Leslie St. and Lawrence Ave.",
               "Leslie St. and York Mills Rd.",
               "Bayview Ave. and Truman Rd.",
               "Midland Ave. and McNicoll Ave.",
               "Steeles Ave. and Birchmount Rd.",
               "Warden Ave. and Arkona Dr.",
               "Steeles Ave. and Brimley Rd.",
               "College St. and Bathurst St.",
               "Lawrence Ave. and Morningside Ave.",
               "Dixon Rd. and Carlingview Dr.",
               "Overlea Blvd. and Thorncliffe Park Dr.")

# Create dataset
simulation_data <- data.frame(
  Location_Code = 2500 + 1:25, # Sequential location codes
  Ward_Number = wards,         # Wards 1 to 25
  Location = sample(locations, 25, replace = FALSE),  # Randomly assign locations
  Enforcement_Start_Date = as.Date(
    paste0(sample(2007:2009, 25, replace = TRUE), "-", 
           sample(1:12, 25, replace = TRUE), "-", 
           sample(1:28, 25, replace = TRUE))
  )  # Random start dates
)

# Add yearly fines (randomly generated fines for each year)
for (year in years) {
  simulation_data[[paste0("Year_", year)]] <- sample(0:500, 25, replace = TRUE)
}



#### Simulate long format data ####

# Define Wards and Years
wards <- 1:25
years <- 2007:2024

# Simulate dataset
simulation_data_long <- data.frame(
  Ward_Number = rep(wards, each = length(years)), 
  Year = rep(years, times = length(wards)))

# Simulate Fines (random fines between 0 and 500)
simulation_data_long$Fines <- sample(0:500, nrow(simulation_data_long), replace = TRUE)

# Simulate Enforcement Start Date
# Randomly assign enforcement start dates (within a plausible range for each ward)
simulation_data_long$Enforcement_Start_Date <- as.Date(
  paste0(sample(2007:2010, nrow(simulation_data_long), replace = TRUE), "-", 
         sample(1:12, nrow(simulation_data_long), replace = TRUE), "-", 
         sample(1:28, nrow(simulation_data_long), replace = TRUE)),
  format = "%Y-%m-%d")

# Enforcement column (0 or 1 based on a threshold: after start date = 1)
simulation_data_long$Enforcement <- ifelse(
  as.Date(paste0(simulation_data_long$Year, "-01-01")) >= 
    simulation_data_long$Enforcement_Start_Date, 1, 0)

# Simulate Locations
locations <- c("Richmond St. and Parliament St.",
               "Lake Shore Blvd. and York St.",
               "Steeles Ave. and Hilda Ave.",
               "Bathurst St. and Sheppard Ave.",
               "Dixon Rd. and Kipling Ave.")
simulation_data_long$Location <- sample(locations, nrow(simulation_data_long), replace = TRUE)




#### Save data ####

write.csv(simulation_data, "../red-camera-charge/data/00-simulated_data/simulated_data.csv", 
          row.names = FALSE)

write.csv(simulation_data_long, "../red-camera-charge/data/00-simulated_data/simulated_data_long.csv", 
          row.names = FALSE)





