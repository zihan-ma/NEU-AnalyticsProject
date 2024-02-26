# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readxl)
library(hexbin)

# Load the dataset
Locally_Inspired_Order <- read_excel(file.choose())

# Calculate LTV
customer_LTV <- Locally_Inspired_Order %>%
  group_by(Name) %>%
  summarise(LTV = sum(Total) - sum(`Discount Amount`))

# Histogram
ggplot(customer_LTV, aes(x=LTV)) +
  geom_histogram(binwidth=10, fill="blue", color="black", alpha=0.7) +
  labs(title="Distribution of Customer LTV", x="LTV", y="Number of Customers")

# Create hexbin plot
ggplot(Locally_Inspired_Order, aes(x=`Discount Amount`, y=Total)) +
  geom_hex(bins = 40) + 
  labs(title="LTV vs Discount Amount", x="Discount Amount", y="LTV") +
  theme_minimal() + 
  theme(legend.position="right")







