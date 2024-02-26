# ALY6015_Module4_Code_ZihanMa.r
# 06.19.2023
# Week 3
# Student Name: Zihan Ma
# Class Name: ALY6015 80472 
#             Intermediate Analytics SEC 04 Spring 2023 CPS [BOS-B-HY]
# Instructor:  Prof. Valeriy Shevchenko



# Un-comment to install package as needed
# install.packages("dplyr")
# install.packages("readr")
# install.packages("glmnet")
# install.packages("ISLR")
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("knitr")
# install.packages("corrplot")

# The libraries actually used in this script.
library(dplyr)    # for data manipulation
library(readr)    # for inputing data 
library(glmnet)   # for fits linear, logistic and multinomial, poisson, and Cox regression models
library(ISLR)     # for an Introduction to Statistical Learning with Applications in R
library(caret)    # for use the createDataPartition function
library(ggplot2)  # for data visualization
library(knitr)    # for output tables
library(corrplot) # A visual exploratory tool on correlation matrix
library(kableExtra)     # for extra fomat for tables

# --------------------------p1----------------------------
# Dataset and Preprocessing

# Read College data set. When prompted navigate and open College.csv file
College_provide <- read.table(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)
View(College_provide)

# Show the variable names, data types, and an overview of the data set.
str(College_provide)

# provides summary statistics for each variable
summary(College_provide)

# Set the seed for reproducibility
set.seed(3456)

# Specify the proportion of data to allocate to the training set
trainIndex <- createDataPartition(College_provide$Private, p = .8, list = FALSE, times = 1)

# Create the training and testing datasets
train_unscale <- College_provide[trainIndex,]
test_unscale <- College_provide[-trainIndex,]

# check proportion of train and test data set
nrow(train_unscale)/(nrow(train_unscale)+nrow(test_unscale))
nrow(test_unscale)/(nrow(train_unscale)+nrow(test_unscale))

# Keep only numerical variables
train_unscale <- select_if(train_unscale, is.numeric)
test_unscale <- select_if(test_unscale, is.numeric)

# Scale the Numeric Features
train <- scale(train_unscale)
test <- scale(test_unscale)

train <- as.data.frame(train)  # Convert train to a data frame
test <- as.data.frame(test)  # Convert test to a data frame

# Show the variable names, data types, and an overview of the data set.
str(train)

# Show the variable names, data types, and an overview of the data set.
str(test)

# ------------------Ridge Regression----------------------
# --------------------------p2----------------------------
# Lambda Estimation
# [Write about the lambda.min and lambda.1se values estimated using the 
# cv.glmnet function and their significance.]

# Define the X and Y matrices
x_train <- model.matrix(Grad.Rate~., data = train)[, -1]
y_train <- train$Grad.Rate
x_test <- model.matrix(Grad.Rate~., data = test)[, -1]
y_test <- test$Grad.Rate

# Use cv.glmnet() to estimate lambda.min and lambda.1se values
cv.ridge <- cv.glmnet(x_train, y_train, alpha = 0)
lambda.min.ridge <- cv.ridge$lambda.min
lambda.1se.ridge <- cv.ridge$lambda.1se

# Print the result
summary(cv.ridge)
print(lambda.min.ridge)
print(lambda.1se.ridge)

# --------------------------p3----------------------------
# Interpretation of cv.glmnet Function Plot
# [A graphical representation is provided for better understanding of cv.glmnet
# results. Discuss the implications of the plot.]

# Plot the results
plot(cv.ridge)

# Add a title to the plot and move it up by 3mm
title(main = "Ridge Regression: CV GLMNET Results", line = 2.5)

# Plot the best λ position
abline( h = cv.ridge$cvup[cv.ridge$index[1]], lty = 4 )

# --------------------------p4----------------------------
# Model Coefficients
# [Report on the coefficients of the Ridge regression model against the
# training set. Highlight if there are any notable findings.]

# Fit a Ridge model against the training set
fit.ridge <- glmnet(x_train, y_train, alpha = 0, lambda = lambda.min.ridge)

# Report on the coefficients
coef.ridge <- coef(fit.ridge)

# Extract the coefficients and their names
coefficients <- as.numeric(coef(fit.ridge))
names <- colnames(train_unscale)

# Create a data frame for plotting
plot_data <- data.frame(names = names, coefficients = coefficients)

# Sort the coefficients in descending order
plot_data <- plot_data[order(-abs(plot_data$coefficients)), ]

# Plot the coefficients
bar_colors <- ifelse(plot_data$coefficients >= 0, "blue", "red")  # Use different colors for positive and negative coefficients
barplot(plot_data$coefficients, names.arg = plot_data$names, col = bar_colors,
        xlab = "Coefficient", ylab = "Value", main = "Ridge Regression Coefficients",
        las = 2)  # Rotate x-axis labels by 90 degrees

# Plot a path diagram
plot(cv.ridge$glmnet.fit, "lambda", label=FALSE)

# Add a title to the plot and move it up by 3mm
title(main = "Ridge Regression: Path Diagram Results", line = 2.5)


# --------------------------p5----------------------------
# Performance on Training Set
# [Calculate and report the RMSE of the Ridge regression model on the training 
# set.]

# Calculate RMSE for the training set
pred.train.ridge <- predict(fit.ridge, s = lambda.min.ridge, newx = x_train)
rmse.train.ridge <- sqrt(mean((y_train - pred.train.ridge)^2))

# Report the RMSE result
print(rmse.train.ridge)

# --------------------------p6----------------------------
# Performance on Test Set
# [Calculate and report the RMSE of the Ridge regression model on the test set.
# Discuss if the model is overfit.]

colnames(x_test) = paste0("column",1:ncol(x_test))

# Calculate RMSE for the test set
pred.test.ridge <- predict(fit.ridge, s = lambda.min.ridge, newx = x_test)
rmse.test.ridge <- sqrt(mean((y_test - pred.test.ridge)^2))



# Create a data frame for Ridge Regression results
ridge_results <- data.frame(
  Metric = c("Lambda.Min", "Lambda.1se", "Training RMSE", "Testing RMSE"),
  Value = c(lambda.min.ridge, lambda.1se.ridge, rmse.train.ridge, rmse.test.ridge)
)

# Create and display a table
knitr::kable(ridge_results, caption = "Ridge Regression Results", "pipe")


# -------------------------LASSO--------------------------
# --------------------------p7----------------------------
# Lambda Estimation
# [Discuss the lambda.min and lambda.1se values estimated using the cv.glmnet
# function in the context of LASSO regression.]

# Use cv.glmnet() to estimate lambda.min and lambda.1se values
cv.lasso <- cv.glmnet(x_train, y_train, alpha = 1)
lambda.min.lasso <- cv.lasso$lambda.min
lambda.1se.lasso <- cv.lasso$lambda.1se

# Print the result
summary(cv.lasso)
print(lambda.min.lasso)
print(lambda.1se.lasso)

# --------------------------p8----------------------------
# Interpretation of cv.glmnet Function Plot
# [Present a graphical representation of the cv.glmnet function results. Discuss
# the implications of the plot.]

# Plot the results
plot(cv.lasso)

# Add a title to the plot and move it up 
title(main = "Lasso Regression: CV GLMNET Results", line = 2.5)

# Plot the best λ position
abline(h = cv.lasso$cvup[cv.lasso$index[1]], lty = 4 )

# --------------------------p9----------------------------
# Model Coefficients
# [Report on the coefficients of the LASSO regression model against the training
# set. Highlight if any coefficients reduce to zero and identify which ones.]

# Fit a LASSO model against the training set
fit.lasso <- glmnet(x_train, y_train, alpha = 1, lambda = lambda.min.lasso)

# Report on the coefficients
coef.lasso <- coef(fit.lasso)
# Extract the coefficients and their names
coefficients_lasso <- as.numeric(coef(fit.lasso))
names <- colnames(train_unscale)

# Create a data frame for plotting
plot_data_lasso <- data.frame(names = names, coefficients = coefficients_lasso)

# Sort the coefficients in descending order
plot_data_lasso <- plot_data_lasso[order(-abs(plot_data_lasso$coefficients)), ]

# Plot the coefficients
bar_colors_lasso <- ifelse(plot_data_lasso$coefficients >= 0, "blue", "red")  # Use different colors for positive and negative coefficients
barplot(plot_data_lasso$coefficients, names.arg = plot_data_lasso$names, col = bar_colors_lasso,
        xlab = "Coefficient", ylab = "Value", main = "LASSO Regression Coefficients",
        las = 2)  # Rotate x-axis labels by 90 degrees

# Plot a path diagram
plot(cv.lasso$glmnet.fit, "lambda", label=FALSE)

# Add a title to the plot and move it up 
title(main = "Lasso Regression: Path Diagram Results", line = 2.5)



# --------------------------p10----------------------------
# Performance on Training Set
# [Calculate and report the RMSE of the LASSO regression model on the training
# set.]

# Calculate RMSE for the training set
pred.train.lasso <- predict(fit.lasso, s = lambda.min.lasso, newx = x_train)
rmse.train.lasso <- sqrt(mean((y_train - pred.train.lasso)^2))

# print the RMSE result
print(rmse.train.lasso)

# --------------------------p11----------------------------
# Performance on Test Set
# [Calculate and report the RMSE of the LASSO regression model on the test set.
# Discuss if the model is overfit.]

# Calculate RMSE for the test set
pred.test.lasso <- predict(fit.lasso, s = lambda.min.lasso, newx = x_test)
rmse.test.lasso <- sqrt(mean((y_test - pred.test.lasso)^2))

# Create a data frame for LASSO results
lasso_results <- data.frame(
  Metric = c("Lambda.Min", "Lambda.1se", "Training RMSE", "Testing RMSE"),
  Value = c(lambda.min.lasso, lambda.1se.lasso, rmse.train.lasso, rmse.test.lasso)
)

# Create and display a table
knitr::kable(lasso_results, caption = "LASSO Regression Results", "pipe")


# --------------------------p12----------------------------
# Performance Comparison
# [Compare the performance of the Ridge and LASSO regression models. Discuss
# which model performed better and why. Share if this was as expected.]

# Combine the results into a single data frame
combined_results <- cbind(ridge_results, lasso_results[,2])

# Set row names for identification
colnames (combined_results) <- c("Metric", "Ridge Regression", "LASSO Regression")

# Create and display a table
knitr::kable(combined_results, caption = "Combined Ridge/LASSO Regression Results", "pipe")


# -----------------Stepwise Regression---------------------
# --------------------------p13----------------------------
# Stepwise Selection and Model Fitting
# [Perform stepwise selection and fit a model. Discuss the results.]

# Stepwise Regression

# Fit a model using stepwise selection
stepwise.model <- step(lm(Grad.Rate ~., data = train), direction = "both")

# Use cv.glmnet() to estimate lambda.min and lambda.1se values
cv.stepwise <- cv.glmnet(x_train, y_train, alpha = 0)
lambda.min.stepwise <- cv.stepwise$lambda.min
lambda.1se.stepwise <- cv.stepwise$lambda.1se


# Calculate RMSE for the training set
pred.train.stepwise <- predict(stepwise.model, newdata = train)
rmse.train.stepwise <- sqrt(mean((train$Grad.Rate - pred.train.stepwise)^2))

# Calculate RMSE for the test set
pred.test.stepwise <- predict(stepwise.model, newdata = test)
rmse.test.stepwise <- sqrt(mean((test$Grad.Rate - pred.test.stepwise)^2))

# Create a data frame for Stepwise Regression results
stepwise_results <- data.frame(
  Metric = c("Lambda.Min", "Lambda.1se", "Training RMSE", "Testing RMSE"),
  Value = c(lambda.min.stepwise, lambda.1se.stepwise, rmse.train.stepwise, rmse.test.stepwise)
)

# Print the results
knitr::kable(stepwise_results, caption = "Stepwise Regression Results", "pipe")
summary(stepwise.model)

# --------------------------p14----------------------------
# Comparing with Ridge and LASSO
# [Discuss if the model from the stepwise selection performed better than or as
# well as the Ridge regression or LASSO.]

# Combine the results into a single data frame
combined_results_3 <- cbind(combined_results, stepwise_results[,2])

# Set row names for identification
colnames (combined_results_3) <- c("Metric", "Ridge Regression", "LASSO Regression", "Stepwise Regression")

# Create and display a table
knitr::kable(combined_results_3, caption = "Combined Ridge/LASSO/Stepwise Regression Results", "pipe")



# --------------------------p15----------------------------
# Preferred Method
# [Discuss which method among Ridge, LASSO, or stepwise selection is preferred
# and explain why.]

# No code in this part
