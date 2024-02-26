# Module5RScript_ZihanMa_03.26.2023
# Week 5
# R Script.R
# Student Name: Zihan Ma
# Class Name: ALY6010: Probability Theory and Introductory Statistics
# Instructor: Tom Breur

# Libraries used in this script
library(rmarkdown)
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(magrittr)
library(RColorBrewer)
library(FSA)
library(ggplot2)
library(corrplot)
library(stargazer)
library(ggcorrplot)

# Data imported from "Adidas US Sales Datasets.xlsx"
library(readxl)
Adidas_US_Sales_Datasets = read_excel("DataSets/Adidas US Sales Datasets.xlsx", skip = 2, col_names = TRUE)

# Create a new dataframe with only numerical columns
Sales = Adidas_US_Sales_Datasets %>% dplyr::select("Price per Unit", "Units Sold", "Total Sales", "Operating Profit")

# View the first few rows of data
kable(head(Sales), align = "c", booktabs = TRUE, caption = "Table 1: Summary Statistics for Numeric Variables") %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  column_spec(1, width = "10em") %>%
  column_spec(2, width = "10em") %>%
  column_spec(3, width = "10em") %>%
  column_spec(4, width = "10em") 

# Create a correlation matrix
Sales_correlations = cor(Sales)

# Create a correlation plot
Sales_correlations_output = ggcorrplot(Sales_correlations, hc.order = TRUE, type = "lower", lab = TRUE, lab_size = 3,
           method = "circle", colors = c("#6D9EC1", "white", "#E46726"), title = "Table 2: Correlation Matrix for Adidas US Sales") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

Sales_correlations_output

# Fit a linear regression model
Sales_model <- lm(Sales$`Total Sales` ~ Sales$`Price per Unit` + Sales$`Units Sold`)

Sales_model_table = stargazer(Sales_model, type = "html")

Sales_model_table <- kable(Sales_model_table, format = "html", escape = FALSE)

Sales_model_table %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE) %>%
  add_header_above(c("Table 3.1: Total Sales vs Units Sold for Adidas US Sales (Color: Price per Unit)" = 1))

# Create a regression plot with ggplot2
Sales_model_regression_plot <- ggplot(Sales, aes(x = Sales$`Units Sold`, y = Sales$`Total Sales`, color = Sales$`Price per Unit`)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", color = "black") +
  scale_color_gradient2(low = "#6D9EC1", mid = "white", high = "#E46726", midpoint = median(Sales$`Price per Unit`)) +
  labs(title = "Table 3.2: Total Sales vs Units Sold for Adidas US Sales(Color: Price per Unit)", x = "Units Sold", y = "Total Sales") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

Sales_model_regression_plot





