
# Set a specific random seed for reproducibility
set.seed(114514)


# Define the constant values
annual_demand <- 12000
order_cost <- 108
holding_cost_percentage <- 0.15

# Define the objective function to minimize the total cost
total_cost <- function(order_quantity) {
  annual_ordering_cost <- (annual_demand / order_quantity) * order_cost
  average_inventory <- order_quantity / 2
  annual_holding_cost <- average_inventory * holding_cost_percentage
  total_inventory_cost <- annual_ordering_cost + annual_holding_cost
  return(sum(total_inventory_cost))
}

# Use optim() to find the order quantity that minimizes the total cost
result <- optim(par = 500, fn = total_cost, method = "Brent", lower = 1, upper = 1000)

# Print the approximate order quantity and the corresponding total cost
approx_order_quantity <- result$par
min_total_cost <- result$value

cat("Approximate Order Quantity:", approx_order_quantity, "\n")
cat("Minimum Total Cost:", min_total_cost, "\n")