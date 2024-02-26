# ALY6050_MOD5Project_MaZ.r
# 06.25.2023
# Week 5
# Student Name: Zihan Ma
# Class Name: ALY6050 80478 
#             Intro to Enterprise Analytics SEC 09 Spring 2023 CPS
# Instructor:  Prof. Richard He

# Un-comment to install package as needed
# install.packages("lpSolve")


# Libraries used in this script
library(lpSolve)    


# Set up the linear programming formulation in an Excel workbook or R

# Coefficients of the objective function
f.obj <- c(519.99 - 349.99, 719.99 - 374.99, 699.99 - 399.99, (249.99*5 - 655))

# Matrix for the left-hand side of the constraints
f.con <- matrix(c(349.99, 374.99, 399.99, 655,     # Budget constraint
                  5*5, 8*5, 5*5, 5*5,             # Space constraint
                  -0.36, -0.36, 1-0.36, 1-0.36,   # Marketing decision 1
                  0, 0, -1.8, 1),                  # Marketing decision 2
                nrow = 4, byrow = TRUE)

# Vector for the right-hand side of the constraints
f.dir <- c("<=", "<=", ">=", ">=")

# Vector for the right-hand side of the constraints
f.rhs <- c(150000, 81*30*5, 0, 0)

# Solve the linear programming problem
solution <- lp("max", f.obj, f.con, f.dir, f.rhs)

# Print the solution
print(solution)

# Calculate the sensitivity of the constraints
sensitivity <- solution$solution[1:length(f.rhs)]
names(sensitivity) <- paste("Constraint", 1:length(f.rhs), sep = " ")
print(sensitivity)

# Increase budget by $10,000 and re-solve
f.rhs[1] <- f.rhs[1] + 10000
solution_increase_budget <- lp("max", f.obj, f.con, f.dir, f.rhs)
increase_in_profit_per_dollar_invested <- (solution_increase_budget$objval - solution$objval) / 10000

# Increase space by 10% and re-solve
f.rhs[2] <- f.rhs[2] * 1.1
solution_increase_space <- lp("max", f.obj, f.con, f.dir, f.rhs)
increase_in_profit_per_sqft_added <- (solution_increase_space$objval - solution$objval) / (0.1 * f.rhs[2])

# Decrease space by 10% and re-solve
f.rhs[2] <- f.rhs[2] * 0.9
solution_decrease_space <- lp("max", f.obj, f.con, f.dir, f.rhs)
decrease_in_profit_per_sqft_removed <- (solution$objval - solution_decrease_space$objval) / (0.1 * f.rhs[2])

# Print results
print(increase_in_profit_per_dollar_invested)
print(increase_in_profit_per_sqft_added)
print(decrease_in_profit_per_sqft_removed)

# Print the decrease in profit per square foot removed
print(decrease_in_profit_per_sqft_removed)







