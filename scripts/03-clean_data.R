
#### Preamble ####
# Purpose: Cleans and processes the raw red-light camera fines dataset for analysis, 
# including transforming variables, handling missing data, and converting to long format.
# Author: Qisheng Yu
# Date: 27 November 2024
# Contact: qisheng.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `dplyr`, `tidyverse`, and `arrow` packages must be installed and loaded.
# - The raw dataset (`raw_data.csv`) must be available in the `01-raw_data` folder.
# Any other information needed? Ensure the `red-camera-charge` R project structure 
# is being used, and the cleaned data will be saved in the `02-analysis_data` folder.




#### Workspace setup ####

library(dplyr)
library(tidyverse)
library(arrow)


#### Clean data ####

data <- read.csv("../red-camera-charge/data/01-raw_data/raw_data.csv")
colnames(data) <- data[4, ]
data <- data[-c(1:4), ]
data <- data %>% slice(1:(n() - 3))

# Rename column names
colnames(data) <- c("Location_Code", "Ward_Number", "Location", "Enforcement_Start_Date",
                    "Enforcement_End_Date", "Year_2007", "Year_2008", "Year_2009", 
                    "Year_2010", "Year_2011", "Year_2012", "Year_2013", "Year_2014", 
                    "Year_2015", "Year_2016", "Year_2017", "Year_2018", "Year_2019", 
                    "Year_2020", "Year_2021", "Year_2022", "Year_2023", "Year_2024")

# Convert dates to standard R date format
data <- data %>%
  mutate(Enforcement_Start_Date = as.Date(as.numeric(Enforcement_Start_Date), origin = "1899-12-30"),
         Enforcement_End_Date = as.Date(as.numeric(Enforcement_End_Date), origin = "1899-12-30"))

data <- data %>% select(-Enforcement_End_Date)

# Convert all year columns to numeric values
data <- data %>%
  mutate(across(starts_with("Year"), as.numeric))

# Replace the NA of the Year column with 0
data <- data %>%
  mutate(across(starts_with("Year"), ~ replace_na(., 0)))

# Remove * from all entries
data <- data %>%
  mutate(across(everything(), ~ gsub("\\*", "", .)))

data <- data %>% drop_na()

# Convert to long format
data_long <- data %>%
  pivot_longer(cols = starts_with("Year"), names_to = "Year", values_to = "Fines") %>%
  mutate(Year = as.numeric(gsub("Year_", "", Year)))

# Convert Enforcement to a binary variable (0 = not started, 1 = started)
data_long <- data_long %>%
  mutate(Enforcement = ifelse(Enforcement_Start_Date <= as.Date(paste0(Year, "-12-31")), 1, 0))


#### Save data ####

write_parquet(data, "../red-camera-charge/data/02-analysis_data/analysis_data.parquet")
write_parquet(data_long, "../red-camera-charge/data/02-analysis_data/analysis_data_long.parquet")


