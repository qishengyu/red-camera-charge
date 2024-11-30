
#### Preamble ####
# Purpose: Tests the structure and validity of the simulated red-light camera fines 
# dataset in both wide and long formats.
# Author: Qisheng Yu
# Date: 27 November 2024
# Contact: qisheng.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` and `testthat` packages must be installed and loaded.
# - Simulated datasets (`simulated_data.csv` and `simulated_data_long.csv`) must 
#   be available after running `00-simulate_data.R`.
# Any other information needed? Ensure you are in the `red-camera-charge` R 
# project structure.


#### Workspace setup ####

library(testthat)

simulated_data <- read.csv("../red-camera-charge/data/00-simulated_data/simulated_data.csv")
data_long <- read.csv("../red-camera-charge/data/00-simulated_data/simulated_data_long.csv")

#### Test data ####

# Define a test suite for the simulated dataset
test_that("Simulated dataset validation", {
  
  # Test 1: Dataset should load successfully
  expect_true(exists("simulated_data"), info = "Dataset could not be loaded.")
  
  # Test 2: Dataset should have the expected number of rows
  expect_equal(nrow(simulated_data), 25, info = "The dataset does not have 25 rows.")
  
  # Test 3: Dataset should have the expected number of columns
  expected_columns <- c("Location_Code", "Ward_Number", "Location", 
                        "Enforcement_Start_Date",
                        paste0("Year_", 2007:2024))
  expect_equal(colnames(simulated_data), expected_columns, 
               info = "The dataset does not have the expected columns.")
  
  # Test 4: `Location_Code` should be unique
  expect_equal(length(unique(simulated_data$Location_Code)), 
               nrow(simulated_data), 
               info = "Location_Code values are not unique.")
  
  # Test 5: `Ward_Number` should be within the range 1-25
  expect_true(all(simulated_data$Ward_Number >= 1 & 
                    simulated_data$Ward_Number <= 25), 
              info = "Ward_Number values are outside the range 1-25.")
  
  # Test 6: `Enforcement_Start_Date` should be a valid date
  enforcement_dates <- as.Date(simulated_data$Enforcement_Start_Date, 
                               format = "%Y-%m-%d")
  expect_true(all(!is.na(enforcement_dates)), 
              info = "Enforcement_Start_Date contains invalid dates.")
  
  # Test 7: Yearly fines columns should have numeric values >= 0
  year_columns <- grep("Year_", colnames(simulated_data), value = TRUE)
  fines_numeric_check <- sapply(simulated_data[year_columns], 
                                function(col) all(is.numeric(col) & col >= 0))
  expect_true(all(fines_numeric_check), 
              info = "Yearly fines contain non-numeric or negative values.")
  
  # Test 8: `Location` should contain valid values (non-NA)
  expect_true(all(!is.na(simulated_data$Location)), 
              info = "Location column contains NA values.")
  
  # Test 9: No duplicate rows in the dataset
  expect_equal(nrow(simulated_data), nrow(unique(simulated_data)), 
               info = "Dataset contains duplicate rows.")
  
})



#### Test long format data ####

# Define test suite for the long dataset
test_that("Dataset Quality Tests for Simulated Data (Long Format)", {
  
  # Test 1: Dataset is loaded successfully
  expect_true(exists("data_long"), info = "Dataset could not be loaded.")
  
  # Test 2: Check the number of rows
  expect_true(nrow(data_long) > 0, info = "The dataset should not be empty.")
  
  # Test 3: Check the presence of expected columns
  expected_columns <- c("Ward_Number", "Year", "Fines", "Enforcement_Start_Date", 
                        "Enforcement")
  expect_true(all(expected_columns %in% colnames(data_long)), 
              info = "The dataset does not have the expected columns.")
  
  # Test 4: `Ward_Number` should be an integer between 1 and 25
  expect_true(all(data_long$Ward_Number >= 1 & data_long$Ward_Number <= 25), 
              info = "Ward_Number values are out of range (1-25).")
  
  # Test 5: `Year` should be between 2007 and 2024
  expect_true(all(data_long$Year >= 2007 & data_long$Year <= 2024), 
              info = "Year values are outside the valid range (2007-2024).")
  
  # Test 6: `Fines` should be numeric and >= 0
  expect_true(all(is.numeric(data_long$Fines) & data_long$Fines >= 0), 
              info = "Fines should be numeric and non-negative.")
  
  # Test 7: `Enforcement_Start_Date` should be a valid date
  enforcement_dates <- as.Date(data_long$Enforcement_Start_Date, 
                               format = "%Y-%m-%d")
  expect_true(all(!is.na(enforcement_dates)), 
              info = "Enforcement_Start_Date contains invalid or missing values.")
  
  # Test 8: `Enforcement` should only contain 0 or 1
  expect_true(all(data_long$Enforcement %in% c(0, 1)), 
              info = "Enforcement column contains values other than 0 or 1.")
  
  # Test 9: No duplicate rows in the dataset
  expect_equal(nrow(data_long), nrow(unique(data_long)), 
               info = "The dataset contains duplicate rows.")
  
  # Test 10: Ensure no missing values in required columns
  required_columns <- c("Ward_Number", "Year", "Fines", "Enforcement_Start_Date", 
                        "Enforcement")
  expect_true(all(complete.cases(data_long[required_columns])), 
              info = "There are missing values in required columns.")
})




