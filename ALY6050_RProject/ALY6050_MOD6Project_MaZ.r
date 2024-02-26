# ALY6050_MOD6Project_MaZ.r
# 06.25.2023
# Week 6
# Student Name: Zihan Ma
# Class Name: ALY6050 80478 
#             Intro to Enterprise Analytics SEC 09 Spring 2023 CPS
# Instructor:  Prof. Richard He

# Un-comment to install package as needed
# install.packages("lpSolve")


# Libraries used in this script
library(lpSolve)    

# ------------------------------------------------------------------------------
# Define the cost matrices for direct shipment and transshipments

# Direct shipment costs
plant_to_site_costs <- matrix(c(8, 14, 13,
                                10, 8, 9,
                                13, 15, 10,
                                12, 16, 14,
                                12, 11, 13,
                                19, 14, 13), nrow = 6, ncol = 3, byrow = TRUE)

# Transshipment costs between plants
inter_plant_costs <- matrix(c(0, 2, 5, 3, 3, 4,
                              2, 0, 4, 6, 5, 3,
                              5, 4, 0, 3, 4, 5,
                              3, 6, 3, 0, 4, 7,
                              3, 5, 4, 4, 0, 4,
                              4, 3, 5, 7, 4, 0), nrow = 6, ncol = 6, byrow = TRUE)

# Transshipment costs between disposal sites
inter_site_costs <- matrix(c(0, 3, 4,
                             3, 0, 5,
                             4, 5, 0), nrow = 3, ncol = 3, byrow = TRUE)

# ------------------------------------------------------------------------------
# Define the supply from each plant and the demand at each disposal site

# Supply from each plant
supply <- c(65, 35, 60, 50, 40, 50)

# Maximum capacity for each waste disposal site
capacity <- c(90, 120, 110)


# ------------------------------------------------------------------------------
# Formulate the LP problem and solve it

# # Convert NA values to large numbers
# direct_costs[is.na(direct_costs)] <- 1e7
# plant_costs[is.na(plant_costs)] <- 1e7
# disposal_costs[is.na(disposal_costs)] <- 1e7

# Define the objective function
cost_matrix <- rbind(cbind(inter_plant_costs, plant_to_site_costs), cbind(t(plant_to_site_costs), inter_site_costs))
cost_vector <- as.vector(t(cost_matrix))


# Inter-plant constraints
inter_plant_supply <- diag(6)
inter_plant_capacity <- matrix(0, nrow = 6, ncol = 36)

# Plant to site constraints
plant_to_site_supply <- matrix(0, nrow = 6, ncol = 36)
plant_to_site_capacity <- diag(3)

# Inter-site constraints
inter_site_supply <- matrix(0, nrow = 3, ncol = 36)
inter_site_capacity <- diag(3)

# Combine the constraint matrices
constraints <- cbind(inter_plant_supply, plant_to_site_supply, inter_site_supply,
                     inter_plant_capacity, plant_to_site_capacity, inter_site_capacity)

rhs <- c(supply, -supply, capacity, -capacity)

# Run the linear program
lp_solution <- lp("min", cost_vector, constraints, direction, rhs, all.bin = FALSE)

# Check the solution status
if(lp_solution$status == 0) {
  print("Optimal solution found")
  shipments <- matrix(lp_solution$solution, nrow = 9, ncol = 9)
  print(shipments)
} else {
  print("No optimal solution found")
}





# ------------------------------------------------------------------------------





# ------------------------------------------------------------------------------



# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------













