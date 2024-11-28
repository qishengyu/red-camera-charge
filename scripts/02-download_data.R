
#### Preamble ####
# Purpose: Downloads and saves the data from [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####

library(opendatatoronto)
library(dplyr)


#### Download data ####

package <- show_package("6ae000e1-9899-4f6e-8e7d-96dca267db80")
package

resources <- list_package_resources(package)
resources

# Load the dataset
data <- get_resource("8dd4a83e-3284-4b16-9295-c256dcf62954")


#### Save data ####

write.csv(data, "../red-camera-charge/data/01-raw_data/raw_data.csv", row.names = FALSE)