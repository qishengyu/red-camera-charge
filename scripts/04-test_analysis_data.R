
#### Preamble ####
# Purpose: Tests the structure, quality, and validity of the processed red-light 
# camera fines dataset in both wide and long formats.
# Author: Qisheng Yu
# Date: 27 November 2024
# Contact: qisheng.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse`, `testthat`, and `arrow` packages must be installed and loaded.
# - Processed datasets (`analysis_data.parquet` and `analysis_data_long.parquet`) 
#   must be available in the `02-analysis_data` folder.
# Any other information needed? Ensure the `red-camera-charge` R project structure 
# is being used, and the test scripts align with the analysis workflow.


#### Workspace setup ####

library(tidyverse)
library(testthat)
library(arrow)

data <- read_parquet("../red-camera-charge/data/02-analysis_data/analysis_data.parquet")
data_long <- read_parquet("../red-camera-charge/data/02-analysis_data/analysis_data_long.parquet")

data <- data %>%
  mutate_at(vars(contains("Year")), as.numeric)
data <- data %>%
  mutate(Enforcement_Start_Date = as.Date(Enforcement_Start_Date))
data <- data %>%
  mutate(Location_Code = as.numeric(Location_Code))
data$Ward_Number <- as.numeric(data$Ward_Number)

data_long <- data_long %>%
  mutate_at(vars(contains("Year")), as.numeric)
data_long <- data_long %>%
  mutate(Enforcement_Start_Date = as.Date(Enforcement_Start_Date))
data_long <- data_long %>%
  mutate(Fines = as.numeric(Fines))
data_long <- data_long %>%
  mutate(Location_Code = as.numeric(Location_Code))
data_long$Ward_Number <- as.numeric(data_long$Ward_Number)



#### Test data ####

test_that("Dataset Quality Tests", {
  
  # Test 1: Dataset is loaded successfully
  expect_true(exists("data"), info = "Dataset could not be loaded.")
  
  # Test 2: Check the number of rows and columns
  expect_true(nrow(data) > 0, info = "Dataset should have more than 0 rows.")
  expect_true(ncol(data) > 0, info = "Dataset should have more than 0 columns.")
  
  # Test 3: Validate column names
  expected_columns <- c("Location_Code", "Ward_Number", "Location", 
                        "Enforcement_Start_Date", paste0("Year_", 2007:2024))
  expect_true(all(expected_columns %in% colnames(data)), 
              info = "The dataset does not contain all expected columns.")
  
  # Test 4: Check if `Location_Code` is numeric and unique
  expect_true(is.numeric(data$Location_Code), 
              info = "Location_Code column should be numeric.")
  expect_equal(length(unique(data$Location_Code)), nrow(data), 
               info = "Location_Code values should be unique.")
  
  # Test 5: Validate `Ward_Number` is integer and within range
  expect_true(all(data$Ward_Number >= 1 & data$Ward_Number <= 25), 
              info = "Ward_Number values should be between 1 and 25.")
  
  # Test 6: Check `Enforcement_Start_Date` is a valid date
  enforcement_dates <- as.Date(data$Enforcement_Start_Date, format = "%Y-%m-%d")
  expect_true(all(!is.na(enforcement_dates)), 
              info = "Enforcement_Start_Date column contains invalid dates.")
  
  # Test 7: Validate all yearly fines columns
  year_columns <- grep("Year_", colnames(data), value = TRUE)
  fines_numeric_check <- sapply(data[year_columns], function(col) all(is.numeric(col) & col >= 0))
  expect_true(all(fines_numeric_check), 
              info = "All yearly fines columns should contain non-negative numeric values.")
  
  # Test 8: `Location` column should not contain NA values
  expect_true(all(!is.na(data$Location)), 
              info = "Location column contains NA values.")
  
  # Test 9: Dataset should not contain duplicate rows
  expect_equal(nrow(data), nrow(unique(data)), 
               info = "The dataset contains duplicate rows.")
  
  # Test 10: Validate `Year_2007` column is present and non-negative
  expect_true("Year_2007" %in% colnames(data), 
              info = "Year_2007 column is missing.")
  expect_true(all(data$Year_2007 >= 0), 
              info = "Year_2007 column contains negative values.")
})



#### Test long format data ####

# Define test suite for the long-format dataset
test_that("Long-Format Dataset Quality Tests", {
  
  # Test 1: Dataset is loaded successfully
  expect_true(exists("data_long"), info = "Dataset could not be loaded.")
  
  # Test 2: Dataset should not be empty
  expect_true(nrow(data_long) > 0, info = "Dataset should not be empty.")
  expect_true(ncol(data_long) > 0, info = "Dataset should have columns.")
  
  # Test 3: Validate column names
  expected_columns <- c("Ward_Number", "Year", "Fines", "Enforcement_Start_Date", "Enforcement")
  expect_true(all(expected_columns %in% colnames(data_long)), 
              info = "The dataset does not contain all expected columns.")
  
  # Test 4: `Ward_Number` should be numeric and within range 1-25
  data_long$Ward_Number <- as.numeric(data_long$Ward_Number)  # Ensure numeric
  expect_true(all(data_long$Ward_Number >= 1 & data_long$Ward_Number <= 25), 
              info = "Ward_Number values should be between 1 and 25.")
  
  # Test 5: `Year` should be between 2007 and 2024
  expect_true(all(data_long$Year >= 2007 & data_long$Year <= 2024), 
              info = "Year values are outside the valid range (2007-2024).")
  
  # Test 6: `Fines` should be numeric and >= 0
  expect_true(is.numeric(data_long$Fines), 
              info = "Fines column should be numeric.")
  expect_true(all(data_long$Fines >= 0), 
              info = "Fines column contains negative values.")
  
  # Test 7: `Enforcement_Start_Date` should be a valid date
  enforcement_dates <- as.Date(data_long$Enforcement_Start_Date, format = "%Y-%m-%d")
  expect_true(all(!is.na(enforcement_dates)), 
              info = "Enforcement_Start_Date contains invalid or missing dates.")
  
  # Test 8: `Enforcement` should only contain 0 or 1
  expect_true(all(data_long$Enforcement %in% c(0, 1)), 
              info = "Enforcement column contains values other than 0 or 1.")
  
  # Test 9: No duplicate rows in the dataset
  expect_equal(nrow(data_long), nrow(unique(data_long)), 
               info = "The dataset contains duplicate rows.")
  
  # Test 10: No missing values in critical columns
  required_columns <- c("Ward_Number", "Year", "Fines", "Enforcement_Start_Date", "Enforcement")
  expect_true(all(complete.cases(data_long[required_columns])), 
              info = "There are missing values in required columns.")
})



