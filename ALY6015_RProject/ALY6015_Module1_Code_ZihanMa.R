# ALY6015_Module1_Code_ZihanMa.r
# 05.30.2023
# Week 1
# Student Name: Zihan Ma
# Class Name: ALY6015 80472 
#             Intermediate Analytics SEC 04 Spring 2023 CPS [BOS-B-HY]
# Instructor:  Prof. Valeriy Shevchenko

# Un-comment to install package as needed
# install.packages("tidyverse")
# install.packages("corrplot")
# install.packages("car")

# Libraries used in this script
library(tidyverse)    # A collection of basic R packages such as ggplot2, tibble, tidyr, readr, dplyr, stringr, purr, and forcats.
library(corrplot)     # A visual exploratory tool on correlation matrix
library(car)          # For use vif()

# The libraries actually used in this script.
# library(dplyr)    # for data manipulation
# library(readr)    # for inputing data 
# library(ggplot2)  # for data visualization
# library(corrplot) # a visual exploratory tool on correlation matrix
# library(car)      # for use vif()

# --------------------------q1----------------------------

# Read AmesHousing-2 data set. When prompted navigate and open AmesHousing-2.csv file
AmesHousing <- read.table(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)
View(AmesHousing)

# Add custom column names for this data set is not necessary
# names(AmesHousing) <- c("Order", "PID", "MSSubClass", "MSZoning",
#                     "SqFt", "Income", "IncomePerSqFt", "Expense",
#                     "ExpensePerSqFt", "NetIncome", "Value",
#                     "ValuePerSqFt", "Boro")

# --------------------------q2----------------------------

# Show the variable names, data types, and an overview of the dataset.
str(AmesHousing)

# provides summary statistics for each variable
summary(AmesHousing)

# --------------------------q3----------------------------

# Check how many missing value do we have
colSums(is.na(AmesHousing))

# Tool function for filling NA values
Mode <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Identify variables with missing values
variables_with_missing <- colnames(AmesHousing)[apply(is.na(AmesHousing), 2, any)]

# Impute missing values with mean (numerical variables) or mode (categorical variables)
for (var in variables_with_missing) {
  if (is.numeric(AmesHousing[[var]])) {
    imputed_value <- mean(AmesHousing[[var]], na.rm = TRUE)
    AmesHousing[[var]][is.na(AmesHousing[[var]])] <- imputed_value
  } else {
    imputed_value <- Mode(AmesHousing[[var]], na.rm = TRUE)  # Mode function is used for finding mode
    AmesHousing[[var]][is.na(AmesHousing[[var]])] <- imputed_value
  }
}

# Check if missing values are imputed
colSums(is.na(AmesHousing))

# --------------------------q4----------------------------

# for testing 
# Select only the columns from the data set which could meaningful to compute the correlation
# picked_data <- select(AmesHousing, Lot.Area, Overall.Qual, 
#                       Overall.Cond, Year.Built, SalePrice)
#
# Compute the correlation matrix
# cor_matrix <- cor(picked_data)

# Select only the numeric columns from the dataset
numeric_columns <- sapply(AmesHousing, is.numeric)
numeric_data <- AmesHousing[, numeric_columns]

# Compute the correlation matrix
cor_matrix <- cor(numeric_data)

# Print the correlation matrix
print(cor_matrix)

# --------------------------q5----------------------------

# Create a correlation matrix plot
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         tl.col = "black", tl.srt = 45, tl.cex = 0.8, col.lab = "black")

# Add a title to the graph
title(main = "Correlation Matrix of Numeric Variables")

# --------------------------q6----------------------------

# Identify the variable with the highest correlation with SalePrice
highest_corr_var <- names(which.max(cor_matrix[,"SalePrice"]))

# Identify the variable with the lowest correlation with SalePrice
lowest_corr_var <- names(which.min(cor_matrix[,"SalePrice"]))

# Identify the variable with correlation closest to 0.5 with SalePrice
closest_corr_var <- names(which(abs(cor_matrix[,"SalePrice"] - 0.5) == min(abs(cor_matrix[,"SalePrice"] - 0.5))))

# Function to add a trend line to a scatter plot
add_trendline <- function(x, y) {
  lm_model <- lm(y ~ x)
  abline(lm_model, col = "blue")
}

# Customize plot settings
plot_settings <- list(
  xlab = list("X Variable (Units)", cex = 1.2),
  ylab = list("Sale Price ($)", cex = 1.2),
  cex.axis = 1.1,
  cex.lab = 1.2,
  cex.main = 1.4
)

# Create scatter plots with custom styling
par(mfrow = c(1, 3))

# Scatter plot for the variable with the highest correlation
plot(AmesHousing[, highest_corr_var], AmesHousing$SalePrice,
     xlim = c(min(AmesHousing[, highest_corr_var]), max(AmesHousing[, highest_corr_var])),
     ylim = c(min(AmesHousing$SalePrice), max(AmesHousing$SalePrice)),
     xlab = plot_settings$xlab, ylab = plot_settings$ylab, main = "SalePrice vs. Highest Correlated Variable(SalePrice)")
add_trendline(AmesHousing[, highest_corr_var], AmesHousing$SalePrice)

# Scatter plot for the variable with the lowest correlation
plot(AmesHousing[, lowest_corr_var], AmesHousing$SalePrice,
     xlim = c(min(AmesHousing[, lowest_corr_var]), max(AmesHousing[, lowest_corr_var])),
     ylim = c(min(AmesHousing$SalePrice), max(AmesHousing$SalePrice)),
     xlab = plot_settings$xlab, ylab = plot_settings$ylab, main = "SalePrice vs. Lowest Correlated Variable(PID)")
add_trendline(AmesHousing[, lowest_corr_var], AmesHousing$SalePrice)

# Scatter plot for the variable with correlation closest to 0.5
plot(AmesHousing[, closest_corr_var], AmesHousing$SalePrice,
     xlim = c(min(AmesHousing[, closest_corr_var]), max(AmesHousing[, closest_corr_var])),
     ylim = c(min(AmesHousing$SalePrice), max(AmesHousing$SalePrice)),
     xlab = plot_settings$xlab, ylab = plot_settings$ylab, main = "Sale vs. Closest Correlation to 0.5(TotRms.AbvGrd)")
add_trendline(AmesHousing[, closest_corr_var], AmesHousing$SalePrice)

# Reset the plot layout
par(mfrow = c(1, 1))

# --------------------------q7----------------------------

# Fit a regression model using three continuous variables
model <- lm(SalePrice ~ Gr.Liv.Area + Total.Bsmt.SF + Year.Built, data = AmesHousing)

# Print the summary of the model
summary(model)

# --------------------------q8----------------------------

# no code for q8

# --------------------------q9----------------------------

# produces the plots for the model: the residuals vs. fitted values plot, the normal Q-Q plot, the scale-location plot, and the residuals vs. leverage plot
plot(model)

# --------------------------q10----------------------------

# Calculate VIF values
vif(model)

# --------------------------q11----------------------------

# Use outlierTest() to detect outliers
outlier_test <- outlierTest(model)

# Print the results
print(outlier_test)


# --------------------------q12----------------------------

# Remove observations 2181 and 1499
new_AmesHousing <- AmesHousing[-c(2181, 1499), ]

# Fit a regression model using three continuous variables
new_model <- lm(SalePrice ~ Gr.Liv.Area + Total.Bsmt.SF + Year.Built, data = new_AmesHousing)

# Compaire the summary of the model
summary(model)
summary(new_model)


# --------------------------q13----------------------------

# the lowest model is the null model, basically the straight average
nullModel <- lm(SalePrice ~ 1, data = AmesHousing)

# the largest model we will accept
fullModel <- lm(SalePrice ~ Gr.Liv.Area + Total.Bsmt.SF + Year.Built, data = AmesHousing)

# try different models
# start with nullModel, do not go above fullModel, work in both directions
houseStep <- step(nullModel, scope = list(lower = nullModel, upper = fullModel), direction = "both")

# --------------------------q14----------------------------

# no code for q14



