
#### Preamble ####
# Purpose: Downloads and saves the red-light camera fines dataset from 
# Open Data Toronto.
# Author: Qisheng Yu
# Date: 27 November 2024
# Contact: qisheng.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `opendatatoronto` and `dplyr` packages must be installed and loaded.
# - Ensure the `red-camera-charge` R project structure is being used.
# Any other information needed? Ensure the working directory is set correctly   
# to save the downloaded data into the specified folder.


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