
#### Preamble ####
# Purpose: Explores and analyzes the red-light camera fines dataset through 
# summary statistics, visualizations, and exploratory data analysis (EDA), 
# focusing on trends, regional differences, and predictors.
# Author: Qisheng Yu
# Date: 27 November 2024
# Contact: qisheng.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `arrow`, `ggplot2`, and `tidyr` packages must be installed and loaded.
# - Processed datasets (`analysis_data.parquet` and `analysis_data_long.parquet`) 
#   must be available in the `02-analysis_data` folder.
# Any other information needed? Ensure the `red-camera-charge` R project 
# structure is used for saving outputs and running analyses.



#### Workspace setup ####

library(arrow)
library(ggplot2)
library(tidyr)

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



#### Structure ####

str(data)
str(data_long)



#### Outcome Variable ####

# Summary statistics
summary_table <- data_long %>%
  summarise(
    Total_Fines = sum(Fines, na.rm = TRUE),
    Mean_Fines = mean(Fines, na.rm = TRUE),
    Median_Fines = median(Fines, na.rm = TRUE),
    Min_Fines = min(Fines, na.rm = TRUE),
    Max_Fines = max(Fines, na.rm = TRUE))
print(summary_table)

# Visualization: Distribution of Fines
ggplot(data_long, aes(x = log1p(Fines))) +
  geom_histogram(binwidth = 10, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution of Fines", x = "Fines", y = "Frequency") +
  theme_bw() +
  theme(panel.grid = element_blank())



#### Predictor Variables ####

ggplot(data_long, aes(x = as.factor(Enforcement), y = Fines)) +
  geom_boxplot(fill = c("lightblue", "orange"), alpha = 0.7) +
  labs(title = "Fines Before and After Enforcement", 
       x = "Enforcement (0 = Before, 1 = After)", y = "Fines") +
  theme_bw() +
  theme(panel.grid = element_blank())



#### EDA ####

# Descriptive Statistical Analysis: Total charges by region
region_totals <- data %>%
  group_by(Ward_Number) %>%
  summarise(Total_Fines = sum(Year_2007, Year_2008, Year_2009, Year_2010, Year_2011, 
                              Year_2012, Year_2013, Year_2014, Year_2015, Year_2016, 
                              Year_2017, Year_2018, Year_2019, Year_2020, Year_2021,
                              Year_2022, Year_2023, Year_2024, na.rm = TRUE))

ggplot(region_totals, aes(x = as.factor(Ward_Number), 
                          y = Total_Fines, fill = Total_Fines)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Charges by Region", 
       x = "Ward Number", y = "Total Charges", fill = "Total Fines") +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") 


# Time Trend Analysis

# Overall trend
ggplot(data_long, aes(x = Year, y = Fines)) +
  geom_line(stat = "summary", fun = "sum") +
  labs(title = "Trends in Annual Charges", x = "Year", y = "Total Charges") +
  theme_bw() +
  theme(panel.grid = element_blank())

# Trends by region
ggplot(data_long, aes(x = Year, y = Fines, color = as.factor(Ward_Number))) +
  geom_line(stat = "summary", fun = "mean") +
  labs(title = "Annual Trends in Charges by Region", 
       x = "Year", y = "Average Charges", 
       color = "Region (Ward)") +
  theme_bw() +
  theme(panel.grid = element_blank())



