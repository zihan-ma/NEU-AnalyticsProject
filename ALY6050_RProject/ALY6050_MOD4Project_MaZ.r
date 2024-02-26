# ALY6050_MOD4Project_MaZ.r
# 06.20.2023
# Week 4
# Student Name: Zihan Ma
# Class Name: ALY6050 80478 
#             Intro to Enterprise Analytics SEC 09 Spring 2023 CPS
# Instructor:  Prof. Richard He

# Un-comment to install package as needed
# install.packages("tidyverse")
# install.packages("knitr")

# Libraries used in this script
library(tidyverse)    # A collection of basic R packages such as ggplot2, tibble, tidyr, readr, dplyr, stringr, purr, and forcats.


# --------------------------q1----------------------------

# Set a specific random seed for reproducibility
set.seed(114514)


# Generate 1000 random values between 1 and 1000 for order_quantity
order_quantity <- sample(1:10000, 10000, replace = TRUE)

# Define the constant values
annual_demand <- 12000
order_cost <- 108
holding_cost_percentage <- 0.15
holding_cost_per_unit <- 90 * 0.15

# Initialize empty vectors for storing results
annual_ordering_cost <- numeric(length(order_quantity))
average_inventory <- numeric(length(order_quantity))
annual_holding_cost <- numeric(length(order_quantity))
total_inventory_cost <- numeric(length(order_quantity))

# Perform calculations for each order quantity
for (i in 1:length(order_quantity)) {
  # Calculate annual ordering cost
  annual_ordering_cost[i] <- (annual_demand / order_quantity[i]) * order_cost
  
  # Calculate average inventory
  average_inventory[i] <- order_quantity[i] / 2
  
  # Calculate annual holding cost
  annual_holding_cost[i] <- average_inventory[i] * holding_cost_percentage
  
  # Calculate total inventory cost
  total_inventory_cost[i] <- annual_ordering_cost[i] + annual_holding_cost[i]
}

# Create a data frame to store the results
inventory_costs <- data.frame(Order_Quantity = order_quantity,
                              Annual_Ordering_Cost = annual_ordering_cost,
                              Average_Inventory = average_inventory,
                              Annual_Holding_Cost = annual_holding_cost,
                              Total_Inventory_Cost = total_inventory_cost)

# Find the order quantity with the minimum total inventory cost
min_cost <- inventory_costs$Total_Inventory_Cost[which.min(inventory_costs$Total_Inventory_Cost)]
optimal_order_quantity <- inventory_costs$Order_Quantity[which.min(inventory_costs$Total_Inventory_Cost)]

# Print the inventory cost data frame
head(inventory_costs)

# Print the approximate order quantity with the smallest total cost
cat("\nApproximate order quantity with the smallest total cost: ", optimal_order_quantity, "\n")
cat("Corresponding total cost: ", min_cost, "\n")

# Plot Total Cost versus Order Quantity
plot(inventory_costs$Order_Quantity, inventory_costs$Total_Inventory_Cost, type = "l",
     xlab = "Order Quantity", ylab = "Total Cost", main = "Total Cost versus Order Quantity")

# Add a marker for the order quantity with the smallest total cost
points(optimal_order_quantity, min_cost, col = "red", pch = 16)

# --------------------------q1----------------------------
  
# # Install necessary libraries
# install.packages("triangle")

# Load necessary libraries
library(triangle)

# Set up triangular distribution parameters
lower <- 8000
mode <- 13000
upper <- 15000

# Define number of simulation occurrences
num_occurrences <- 5000

# Define potential order quantities
order_quantities <- seq(500, 10000, by = 100)

# Initialize vectors for minimum total cost, optimal order quantity, and annual number of orders
min_total_costs <- numeric(num_occurrences)
optimal_order_quantities <- numeric(num_occurrences)
annual_number_of_orders <- numeric(num_occurrences)

# Function to generate random numbers from a triangular distribution
rtriangle_base <- function(n, a, b, c) {
  u <- runif(n)
  q <- (c - a) / (b - a)
  
  return(ifelse(u < q, a + sqrt(u * (b - a) * (c - a)), b - sqrt((1 - u) * (b - a) * (b - c))))
}

# Check if rtriangle_base is working as expected
print(rtriangle_base(1, lower, upper, mode))

# redefine total_inventory_cost function
total_inventory_cost <- function(order_quantity, demand) {
  # Calculate the annual ordering cost
  annual_ordering_cost <- (demand / order_quantity) * order_cost
  
  # Calculate the annual holding cost
  annual_holding_cost <- (order_quantity / 2) * holding_cost_per_unit
  
  # The total cost is the sum of the ordering and holding costs
  total_cost <- annual_ordering_cost + annual_holding_cost
  
  return(total_cost)
}

# Perform simulation
for(i in 1:num_occurrences){
  demand <- rtriangle_base(1, lower, mode, upper)
  total_costs <- sapply(order_quantities, function(x) total_inventory_cost(x, demand))
  
  print(paste("Demand:", demand))
  print(paste("Total costs:", total_costs))
  
  min_total_costs[i] <- min(total_costs)
  optimal_order_quantities[i] <- order_quantities[which.min(total_costs)]
  annual_number_of_orders[i] <- demand / optimal_order_quantities[i]
}

# Function to calculate confidence interval
calc_CI <- function(data, conf_level = 0.95) {
  mu <- mean(data)
  sigma <- sd(data)
  se <- sigma / sqrt(length(data))
  
  z <- qnorm((1 - conf_level) / 2, lower.tail = FALSE)
  
  lower <- mu - z * se
  upper <- mu + z * se
  
  return(c(lower, upper))
}

# Estimate the expected minimum total cost and its 95% confidence interval
expected_min_cost <- mean(min_total_costs)
CI_min_cost <- calc_CI(min_total_costs)

# Estimate the expected order quantity and its 95% confidence interval
expected_order_quantity <- mean(optimal_order_quantities)
CI_order_quantity <- calc_CI(optimal_order_quantities)

# Estimate the expected annual number of orders and its 95% confidence interval
expected_annual_orders <- mean(annual_number_of_orders)
CI_annual_orders <- calc_CI(annual_number_of_orders)

# Print out the expected minimum total cost and its 95% confidence interval
print(paste("Expected minimum total cost:", expected_min_cost))
print(paste("95% confidence interval for minimum total cost: (", CI_min_cost[1], ", ", CI_min_cost[2], ")"))

# Print out the expected order quantity and its 95% confidence interval
print(paste("Expected order quantity:", expected_order_quantity))
print(paste("95% confidence interval for order quantity: (", CI_order_quantity[1], ", ", CI_order_quantity[2], ")"))

# Print out the expected annual number of orders and its 95% confidence interval
print(paste("Expected annual number of orders:", expected_annual_orders))
print(paste("95% confidence interval for annual number of orders: (", CI_annual_orders[1], ", ", CI_annual_orders[2], ")"))





















