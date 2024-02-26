# Install the required package (uncomment the line below if you need to install the package)
# Install the required packages (uncomment the lines below if you need to install the packages)
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("readr")
# install.packages("tidyr")
# install.packages("visdat")
# install.packages("writexl")
# install.packages("readxl")


# Load the necessary libraries
library(dplyr)      # For data manipulation
library(ggplot2)    # For data visualization
library(readr)      # For reading data
library(tidyr)      # For tidying data
library(visdat)     # For visualizing data
library(writexl)    # For Write the datasets to .xlsx files

library(readxl)     # For reading Excel files

# Read the Excel file into a dataframe
Locally_Inspired_Order <- read_excel(file.choose())

# Uncomment the line below if you want to view the dataframe in a separate window
# View(Locally_Inspired_Order)

# Get a summary of the dataframe
summary(Locally_Inspired_Order)

# Display the structure of the dataframe
str(Locally_Inspired_Order)

# Remove columns where all values are NA
Locally_Inspired_Order <- Locally_Inspired_Order[, colSums(is.na(Locally_Inspired_Order)) != nrow(Locally_Inspired_Order)]

# Remove rows where Fulfillment Status is "unfulfilled"
Locally_Inspired_Order <- Locally_Inspired_Order %>% 
  filter(`Fulfillment Status` != "unfulfilled")

# Remove rows where Financial Status is not "paid"
Locally_Inspired_Order <- Locally_Inspired_Order %>% 
  filter(`Financial Status` == "paid")

# Remove columns that have only one unique value (excluding NAs)
Locally_Inspired_Order <- Locally_Inspired_Order %>%
  select(which(sapply(Locally_Inspired_Order, function(x) length(unique(na.omit(x))) > 1)))

# Find unique value amount for each column
unique_values <- lapply(Locally_Inspired_Order, function(x) length(unique(x)))

# Print out each unique value for every column that has less than 5 unique values
lapply(names(Locally_Inspired_Order)[unique_values < 7], function(x) {print(paste("Column Name:", x)); print(table(Locally_Inspired_Order[[x]]))})

# Remove rows where Subtotal is NA
Locally_Inspired_Order <- Locally_Inspired_Order %>% 
  filter(!is.na(Subtotal))

# Split the data based on whether Shipping Name is NA or not

# Dataset where Shipping Name is NA
inshop_order <- Locally_Inspired_Order %>% 
  filter(is.na(`Shipping Name`))

# Dataset where Shipping Name is not NA
online_order <- Locally_Inspired_Order %>% 
  filter(!is.na(`Shipping Name`))

# Check the dimensions of the split datasets
cat("Dimensions of dataset where Shipping Name is NA:", dim(inshop_order), "\n")
cat("Dimensions of dataset where Shipping Name is not NA:", dim(online_order), "\n")

# Get the current time and format it for the filename
current_time <- format(Sys.time(), "%Y-%m-%d_%H-%M-%S")

# Create filenames with the current time
filename_na <- paste0("inshop_order", current_time, ".xlsx")
filename_not_na <- paste0("online_order", current_time, ".xlsx")

# Write the datasets to .xlsx files
write_xlsx(inshop_order, filename_na)
write_xlsx(online_order, filename_not_na)

# Print out the filenames for verification
cat("Dataset with Shipping Name as NA saved to:", filename_na, "\n")
cat("Dataset with Shipping Name not NA saved to:", filename_not_na, "\n")




# For checking missing data information during process

# # Set a seed for reproducibility
# set.seed(123)
# 
# # Split the data into 30% and 70%
# train_data <- Locally_Inspired_Order_noNAs %>% sample_frac(0.30)
# test_data <- setdiff(Locally_Inspired_Order_noNAs, train_data)
# 
# # Check the dimensions of the split datasets
# cat("Dimensions of train_data (30%):", dim(train_data), "\n")
# cat("Dimensions of test_data (70%):", dim(test_data), "\n")
# 
# # Use vis_miss() to visualize missing data
# vis_miss(change_to_var_name, cluster = TRUE, large = TRUE, warn_large_data = FALSE)
# 
# # Use vis_miss() to visualize missing data
# vis_miss(change_to_var_name, cluster = TRUE, large = TRUE, warn_large_data = FALSE)
















