# Load necessary libraries
library(tidyverse)
library(readr)
library(caret)
library(e1071)
library(cluster)
library(factoextra)
library(ggplot2)
library(reshape2)
library(randomForest)
library(GGally)
library(glmnet)

# Load the dataset
banking_data <- read_csv(file.choose())
summary(banking_data)
#------EDA------

# # Data Preparation and Cleaning
# ## Check for missing values
# colSums(is.na(banking_data))
# 
# ## Convert categorical variables to factors
# categorical_columns <- c("job", "marital", "education", "default", "housing", "loan", "contact", "month", "poutcome", "Target")
# banking_data[categorical_columns] <- lapply(banking_data[categorical_columns], factor)
# 
# # Exploratory Data Analysis
# ## Summary statistics
# summary(banking_data)
# 
# ## Visualizations
# ### Histogram of Age
# hist(banking_data$age, main="Histogram of Age", xlab="Age")
# 
# ### Boxplot for Balance by Education
# boxplot(balance ~ education, data = banking_data, main="Balance by Education", xlab="Education", ylab="Balance")
# 
# ### Scatter plot for Age vs Balance
# plot(banking_data$age, banking_data$balance, main="Age vs Balance", xlab="Age", ylab="Balance", pch=19, col=rgb(0.2,0.4,0.6,0.7))
# 
# # Statistical Analysis
# ## Correlation Analysis
# numeric_columns <- sapply(banking_data, is.numeric)
# correlation_matrix <- cor(banking_data[, numeric_columns])
# print(correlation_matrix)
# 
# # Compute the correlation matrix
# corr_matrix <- cor(banking_data[, numeric_columns])
# 
# # Melt the correlation matrix for visualization
# melted_corr_matrix <- melt(corr_matrix)
# 
# # Heatmap plot with correlation coefficients and title
# ggplot(melted_corr_matrix, aes(Var1, Var2, fill = value)) +
#   geom_tile() +
#   geom_text(aes(label = round(value, 2)), vjust = 1) +  # Add text labels
#   scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
#                        midpoint = 0, limit = c(-1,1), space = "Lab", 
#                        name="Pearson\nCorrelation") +
#   theme_minimal() +
#   coord_fixed() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels for readability
#   ggtitle("Correlation Matrix Heatmap")  # Add title
# 
# ## Chi-Square Test for Categorical Variables
# categorical_columns <- setdiff(categorical_columns, "Target") # Remove 'Target' from categorical columns
# chi_square_results <- lapply(categorical_columns, function(x) {
#   chisq.test(table(banking_data[, x], banking_data$Target))
# })
# names(chi_square_results) <- categorical_columns
# print(chi_square_results)
# 
# # Examining the effect of the number of contacts on subscription rate
# ggplot(banking_data, aes(x = campaign, fill = Target)) +
#   geom_histogram(position = "fill", bins = 30) +
#   labs(title = "Effect of Number of Contacts on Subscription Rate", x = "Number of Contacts", y = "Proportion")
# 
# # Analyzing the impact of contact type on subscription rate
# ggplot(banking_data, aes(x = contact, fill = Target)) +
#   geom_bar(position = "fill") +
#   labs(title = "Impact of Contact Type on Subscription Rate", x = "Contact Type", y = "Proportion")


#------Modeling------

#------Processing The Data------

# Splitting the data into training and testing sets
set.seed(123) # For reproducibility
splitIndex <- createDataPartition(banking_data$Target, p = 0.80, list = FALSE)
train_data <- banking_data[splitIndex, ]
test_data <- banking_data[-splitIndex, ]

# Converting categorical variables to factors
categorical_columns <- c("job", "marital", "education", "default", "housing", "loan", "contact", "poutcome") # add or remove column names as necessary

train_data[categorical_columns] <- lapply(train_data[categorical_columns], factor)
test_data[categorical_columns] <- lapply(test_data[categorical_columns], factor)

# Convert Target to a factor for classification
train_data$Target <- as.factor(train_data$Target)
test_data$Target <- as.factor(test_data$Target)

#------Building Random Forest------


# # Tuning ntree: Try different numbers of trees and observe the error rate
# ntree_range <- seq(100, 500, by = 50)
# oob_error <- sapply(ntree_range, function(ntree) {
#   model <- randomForest(Target ~ ., data = train_data, ntree = ntree, mtry = 2)
#   model$err.rate[ntree, "OOB"]
# })

# Plotting OOB Error vs. Number of Trees
plot(ntree_range, oob_error, type = "b", col = "blue", 
     xlab = "Number of Trees", ylab = "OOB Error Rate", 
     main = "Out-of-Bag Error vs. Number of Trees in Random Forest")

# Choose ntree where the error stabilizes
# Suppose it stabilizes at ntree_opt
ntree_opt <- 450 # replace with the number you find suitable

# Building the model with optimal ntree
rf_model <- randomForest(Target ~ ., data = train_data, ntree = ntree_opt)

# Model summary
print(rf_model)

#------Random Forest Performance------

# Predicting on the test set
rf_predictions <- predict(rf_model, test_data)

# Evaluating model performance
confusionMatrix(rf_predictions, test_data$Target)

#------Building Logistic Regression Model------

# Standardizing numeric variables
numeric_columns <- c("age", "balance", "duration", "campaign", "previous") # Add more as needed
train_data[numeric_columns] <- scale(train_data[numeric_columns])
test_data[numeric_columns] <- scale(test_data[numeric_columns])

#------Finding Best Lambda------

# Preparing the data for glmnet (convert to matrix)
x_train <- model.matrix(Target ~ . - 1, data = train_data) # -1 to exclude the intercept
y_train <- train_data$Target

x_test <- model.matrix(Target ~ . - 1, data = test_data)

# Fit the Logistic Regression model with L1 regularization (Lasso)
set.seed(123) # For reproducibility
cv_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)

# Best lambda value
best_lambda <- cv_model$lambda.min

# Refitting the model on the selected lambda
log_model_reg <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = best_lambda)

# Predictions
predictions <- predict(log_model_reg, s = best_lambda, newx = x_test, type = "response")
predictions <- ifelse(predictions > 0.5, 1, 0)

# Evaluating model performance
confusionMatrix(as.factor(predictions), as.factor(test_data$Target))

#------Cross-Validation------

# Prepare data for glmnet
x_train <- model.matrix(Target ~ . - 1, data = train_data)
y_train <- train_data$Target
x_test <- model.matrix(Target ~ . - 1, data = test_data)

# Cross-Validation to find optimal lambda
cv_log_model <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1)
plot(cv_log_model)
title("Optimal Lambda Selection in Logistic Regression", line = 2.5)
optimal_lambda <- cv_log_model$lambda.min

# Refitting the model with the optimal lambda
log_model_cv <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = optimal_lambda)



#------Logistic Regression Model Performance------
predictions_cv <- predict(log_model_cv, s = optimal_lambda, newx = x_test, type = "response")
predictions_cv <- ifelse(predictions_cv > 0.5, 1, 0)

# Evaluating model performance
confusionMatrix(as.factor(predictions_cv), as.factor(test_data$Target))


#------Model Comparison------

# Calculating Confusion Matrices for each model
rf_conf_matrix <- confusionMatrix(rf_predictions, test_data$Target)
lr_conf_matrix <- confusionMatrix(as.factor(predictions_cv), as.factor(test_data$Target))

# Compute performance metrics for Random Forest
rf_accuracy <- rf_conf_matrix$overall['Accuracy']
rf_sensitivity <- rf_conf_matrix$byClass['Sensitivity']
rf_specificity <- rf_conf_matrix$byClass['Specificity']

# Compute performance metrics for Logistic Regression
lr_accuracy <- lr_conf_matrix$overall['Accuracy']
lr_sensitivity <- lr_conf_matrix$byClass['Sensitivity']
lr_specificity <- lr_conf_matrix$byClass['Specificity']

# Combine the metrics into a data frame
performance_data <- data.frame(
  Model = c("Random Forest", "Logistic Regression"),
  Accuracy = c(rf_accuracy, lr_accuracy),
  Sensitivity = c(rf_sensitivity, lr_sensitivity),
  Specificity = c(rf_specificity, lr_specificity)
)

# Melt the data for plotting
library(reshape2)
melted_data <- melt(performance_data, id.vars = "Model")

# Plotting
library(ggplot2)
ggplot(melted_data, aes(x = Model, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  labs(y = "Performance Metrics", title = "Model Comparison: Random Forest vs Logistic Regression")

#------Model Coefficients Result------

# Extracting feature importance
importance_rf <- importance(rf_model)

# Creating a Feature Importance Plot
varImpPlot(rf_model, main = "Feature Importance in Random Forest Model")

# Extracting coefficients at the optimal lambda
coefficients_opt_lr <- as.matrix(coef(optimal_log_model, s = optimal_lambda))

# Converting to a data frame for ggplot
coef_df <- data.frame(Coefficient = coefficients_opt_lr[,1])
coef_df$Feature <- rownames(coefficients_opt_lr)

# Removing the intercept
coef_df <- coef_df[-1, ]

# Creating a Coefficients Plot
ggplot(coef_df, aes(x = reorder(Feature, Coefficient), y = Coefficient)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Feature") +
  ylab("Coefficient") +
  ggtitle("Coefficients in Optimized Logistic Regression Model")

#------clustering------

# Select columns for clustering
segmentation_columns <- c("age", "job", "education", "balance", "housing", "loan")
segmentation_data <- banking_data[segmentation_columns]

# Convert to dataframe if not already
segmentation_data <- as.data.frame(segmentation_data)

# Remove rows with NA values
segmentation_data <- na.omit(segmentation_data)

# Remove rows with infinite values
for(column in names(segmentation_data)) {
  segmentation_data <- segmentation_data[!is.infinite(segmentation_data[[column]]), ]
}

# Scale the data
segmentation_data_scaled <- scale(segmentation_data)

# K-means clustering
set.seed(123) # for reproducibility
k <- 3 # number of clusters
kmeans_result <- kmeans(segmentation_data_scaled, centers = k, nstart = 25)

# Add cluster assignment to the data for plotting
segmentation_data$cluster <- as.factor(kmeans_result$cluster)

# Plotting clusters
fviz_cluster(kmeans_result, data = segmentation_data_scaled, geom = "point", stand = FALSE, ellipse.type = "norm")