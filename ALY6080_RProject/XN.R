#install.packages("visdat")
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(visdat)

library(readxl)
Locally_Inspired_monthly_inventory <- read_excel("DataSets/Locally Inspired monthly_inventory_2023-05-01.xlsx")
View(Locally_Inspired_monthly_inventory)

Locally_Inspired_Order <- read_excel("DataSets/Locally Inspired Order_1'1'23-5'31'23.xlsx")
View(Locally_Inspired_Order)

summary(Locally_Inspired_monthly_inventory)
str(Locally_Inspired_monthly_inventory)

summary(Locally_Inspired_Order)
str(Locally_Inspired_Order)

# Use vis_miss() to visualize missing data

vis_miss(Locally_Inspired_Order, cluster = TRUE, large = TRUE, warn_large_data = FALSE)

# Remove columns where all values are NA
Locally_Inspired_monthly_inventory <- Locally_Inspired_monthly_inventory[, colSums(is.na(Locally_Inspired_monthly_inventory)) != nrow(Locally_Inspired_monthly_inventory)]
Locally_Inspired_Order <- Locally_Inspired_Order[, colSums(is.na(Locally_Inspired_Order)) != nrow(Locally_Inspired_Order)]

vis_miss(Locally_Inspired_Order, cluster = TRUE, large = TRUE, warn_large_data = FALSE)

summary(Locally_Inspired_monthly_inventory)
str(Locally_Inspired_monthly_inventory)

summary(Locally_Inspired_Order)
str(Locally_Inspired_Order)

--------------------------------

# Step 1: Find unique value amount for each column
lapply(Locally_Inspired_monthly_inventory, function(x) length(unique(x)))

----------------------------

# Step 2: Find number distribution of the columns cost, ending_quantity and sum_last_total_inventory_value using plot
# cost distribution
par(mfrow=c(1,3)) # Use multiple plot layout
hist(Locally_Inspired_monthly_inventory$cost, main = "Distribution of Cost", xlab = "Cost", col = "blue")

# ending_quantity distribution
hist(Locally_Inspired_monthly_inventory$ending_quantity, main = "Distribution of Ending Quantity", xlab = "Ending Quantity", col = "green")

# sum_last_total_inventory_value distribution
hist(Locally_Inspired_monthly_inventory$sum_last_total_inventory_value, main = "Distribution of Sum Last Total Inventory Value", xlab = "Sum Last Total Inventory Value", col = "red")

# Create new data from original data and remove outliers
Locally_Inspired_monthly_inventory_new <- Locally_Inspired_monthly_inventory %>% select(cost, ending_quantity, sum_last_total_inventory_value)
Locally_Inspired_monthly_inventory_new <- Locally_Inspired_monthly_inventory_new %>% 
  mutate(across(everything(), ~ifelse(. > quantile(., 0.75) + 1.5 * IQR(.) | . < quantile(., 0.25) - 1.5 * IQR(.), NA, .)))

par(mfrow=c(1,3)) # Use multiple plot layout
hist(Locally_Inspired_monthly_inventory_new$cost, main = "Distribution of Cost", xlab = "Cost", col = "blue")

# ending_quantity distribution
hist(Locally_Inspired_monthly_inventory_new$ending_quantity, main = "Distribution of Ending Quantity", xlab = "Ending Quantity", col = "green")

# sum_last_total_inventory_value distribution
hist(Locally_Inspired_monthly_inventory_new$sum_last_total_inventory_value, main = "Distribution of Sum Last Total Inventory Value", xlab = "Sum Last Total Inventory Value", col = "red")

----------------------------
  
# Step 3: Find amount of rows with missing values and print them out
Locally_Inspired_monthly_inventory_with_na <- Locally_Inspired_monthly_inventory %>% filter(rowSums(is.na(.)) > 0)
print(Locally_Inspired_monthly_inventory_with_na)



----------------------------
  
# Step 1: Find unique value amount for each column
unique_values <- lapply(Locally_Inspired_Order, function(x) length(unique(x)))

# Print out each unique value for every column that has less than 5 unique values
lapply(names(Locally_Inspired_Order)[unique_values < 7], function(x) {print(paste("Column Name:", x)); print(table(Locally_Inspired_Order[[x]]))})

----------------------------
# Step 2: Find number distribution of the columns cost, ending_quantity and sum_last_total_inventory_value using plot
# cost distribution  
  
# Visualize numeric data
par(mfrow=c(2,2)) # Use multiple plot layout

hist(Locally_Inspired_Order$Subtotal, main="Subtotal Distribution", xlab="Subtotal", col="lightgreen")
hist(Locally_Inspired_Order$Shipping, main="Shipping Distribution", xlab="Shipping", col="lightgreen")
hist(Locally_Inspired_Order$Taxes, main="Taxes Distribution", xlab="Taxes", col="lightgreen")
hist(Locally_Inspired_Order$Total, main="Total Distribution", xlab="Total", col="lightgreen")

----------------------------
  
# Step 3: Find amount of rows with missing values and print them out
missing_data <- Locally_Inspired_Order %>% summarize_all(function(x) sum(is.na(x)))
print(missing_data)











































