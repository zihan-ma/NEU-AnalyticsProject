# ALY6040_Module6_FinalProject_Code_Group2
# Name: Zihan Ma
# Instructor: Kasun Samarasinghe 
# Date: 08.16.2023

# Un-comment to install package as needed
# install.packages('rpart')
# install.packages('caret')
# install.packages('rpart.plot')
# install.packages('rattle')
# install.packages('readxl')
# install.packages('xgboost')
# install.packages('gbm')
# install.packages('Metrics')


#---------Loading libraries----------

library(tidyverse) 
library(visdat) 
library(xgboost)
library(glmnet)
library(gbm)
library(caret)
library(Metrics)
library(car)

#-----------Data Import-------------

# Read listings data set. When prompted navigate and open listings.csv file
library(readr)
listings <- read_csv(file.choose())
# View(listings)

# Setting a seed for reproducibility
set.seed(114514)

# Basic status
summary(listings)
str(listings)

#-----------Data Cleaning-------------

# Checking missing data
missing_data <- sapply(listings, function(y) sum(is.na(y)))
print(missing_data)
vis_miss(listings, cluster = TRUE, large = TRUE, warn_large_data = FALSE)

# Fill missing value in reviews_per_month using 0
listings <- listings %>%
  mutate(reviews_per_month = replace_na(reviews_per_month, 0))

# Checking for repeating data
duplicates <- listings[duplicated(listings),]
print(duplicates)

# Checking for outliers (only for price column)
boxplot(listings$price, main="Price Boxplot", boxwex=0.1)
outliers <- boxplot.stats(listings$price)$out
print(outliers)

# create a dataset withouts the outliers  
listings <- listings %>%
  filter(!(price %in% outliers))

# Find the Earliest and Latest Time in the last_review Column
earliest_review <- min(listings$last_review, na.rm = TRUE)
latest_review <- max(listings$last_review, na.rm = TRUE)

print(paste("Earliest Review:", earliest_review))
print(paste("Latest Review:", latest_review))

# Fill NA Values in the last_review Column with the Earliest Time
listings$last_review[is.na(listings$last_review)] <- earliest_review

# Transform the Recorded Time into Days Using the Latest Time
listings$days_since_last_review <- as.numeric(difftime(latest_review, listings$last_review, units = "days"))

# delete the last_review Column
listings <- listings %>% select(-last_review)


# ---------------Data Engineering-----------------

# Find the unique value amount for each column
unique_counts <- sapply(listings, function(x) length(unique(x)))
print(unique_counts)


# Print out each unique value for every column that has less than 6 unique values
columns_less_than_6 <- names(unique_counts[unique_counts < 6])

for(col in columns_less_than_6) {
  print(paste("Column:", col))
  value_counts <- table(listings[[col]])
  value_percentages <- prop.table(value_counts) * 100
  for(i in 1:length(value_counts)) {
    print(paste0(names(value_counts)[i], ": ", round(value_percentages[i], 2), "%"))
  }
  print("--------------")
}

# One-Hot Encoding for neighbourhood_group
listings <- listings %>%
  mutate(
    Central_Region = as.integer(neighbourhood_group == "Central Region"),
    East_Region = as.integer(neighbourhood_group == "East Region"),
    North_East_Region = as.integer(neighbourhood_group == "North-East Region"),
    North_Region = as.integer(neighbourhood_group == "North Region"),
    West_Region = as.integer(neighbourhood_group == "West Region")
  ) %>%
  select(-neighbourhood_group)

# One-Hot Encoding for room_type
listings <- listings %>%
  mutate(
    Entire_home_apt = as.integer(room_type == "Entire home/apt"),
    Private_room = as.integer(room_type == "Private room"),
    Shared_room = as.integer(room_type == "Shared room")
  ) %>%
  select(-room_type)

# Extract latitude and longitude data
cluster_data <- listings %>% select(latitude, longitude)

# K-means Clustering
# kmeans_result <- kmeans(cluster_data, centers = 28)  # Assuming 28 clusters
kmeans_result <- kmeans(cluster_data, centers = 5)  # Assuming 5 clusters
# Add Cluster Assignment to Data
listings$cluster <- kmeans_result$cluster

# Visualize the Clusters
ggplot(listings, aes(x = latitude, y = longitude, color = as.factor(cluster))) +
  geom_point() +
  labs(color = "Cluster") +
  theme_minimal() +
  ggtitle("K-means Clustering on Latitude and Longitude")

# perform one-hot encoding
listings <- listings %>%
  mutate(cluster_value = 1) %>%
  spread(key = cluster, value = cluster_value, fill = 0)

# Remove the original cluster column if still present
listings$cluster <- NULL

# delete the latitude and longitude Column
listings <- listings %>% select(-latitude)
listings <- listings %>% select(-longitude)

#-----------------Finalize Data-------------------

# delete columns that are hare to utilized
listings <- listings %>% select(-name)
listings <- listings %>% select(-host_name)
listings <- listings %>% select(-neighbourhood)
listings <- listings %>% select(-host_id)

# Standardize numerical columns
listings$price <- scale(listings$price)
listings$minimum_nights <- scale(listings$minimum_nights)
listings$number_of_reviews <- scale(listings$number_of_reviews)
listings$reviews_per_month <- scale(listings$reviews_per_month)
listings$calculated_host_listings_count <- scale(listings$calculated_host_listings_count)
listings$availability_365 <- scale(listings$availability_365)
listings$days_since_last_review <- scale(listings$days_since_last_review)

# Function to normalize a column
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# Normalize numerical columns
listings$price <- normalize(listings$price)
listings$minimum_nights <- normalize(listings$minimum_nights)
listings$number_of_reviews <- normalize(listings$number_of_reviews)
listings$reviews_per_month <- normalize(listings$reviews_per_month)
listings$calculated_host_listings_count <- normalize(listings$calculated_host_listings_count)
listings$availability_365 <- normalize(listings$availability_365)
listings$days_since_last_review <- normalize(listings$days_since_last_review)




# Splitting data into training (80%) and testing (20%) sets
sample_size <- floor(0.8 * nrow(listings))
train_indices <- sample(seq_len(nrow(listings)), size=sample_size)

train_data <- listings[train_indices, ]
test_data <- listings[-train_indices, ]

# Setting up train control for k-fold cross-validation
train_control <- trainControl(method="cv", number=10) # 10-fold cross-validation



#--------------------------MLR----------------------------

# linear regression
mlr_model <- train(price ~ ., data=train_data, method="lm", trControl=train_control)
summary(mlr_model)

# Converting training data to model matrix format
x_train <- model.matrix(price ~ ., train_data)[,-1]
y_train <- train_data$price




# Lasso Regression (L1 Regularization)
lasso_model <- cv.glmnet(x_train, y_train, alpha=1)

lambda.min.lasso <- lasso_model$lambda.min
lambda.1se.lasso <- lasso_model$lambda.1se

# Interpretation of cv.glmnet Function Plot

summary(lasso_model)

# Plot the results
plot(lasso_model)

# Add a title to the plot and move it up by 3mm
title(main = "Lasso Regression: CV GLMNET Results", line = 2.5)

# Plot the best λ position
abline( h = lasso_model$cvup[lasso_model$index[1]], lty = 4 )




# Model Coefficients

# Fit a Ridge model against the training set
fit_lasso <- glmnet(x_train, y_train, alpha = 0, lambda = lambda.min.lasso)

# Report on the coefficients
coef_lasso <- coef(fit_lasso)
coef_lasso

# Extract the coefficients and their names
coefficients <- as.numeric(coef(fit_lasso))
names <- colnames(train_data)

# Create a data frame for plotting
plot_data <- data.frame(names = names, coefficients = coefficients)

# Sort the coefficients in descending order
plot_data <- plot_data[order(-abs(plot_data$coefficients)), ]

# Plot the coefficients
bar_colors <- ifelse(plot_data$coefficients >= 0, "blue", "red")  # Use different colors for positive and negative coefficients
barplot(plot_data$coefficients, names.arg = plot_data$names, col = bar_colors,
        xlab = "Coefficient", ylab = "Value", main = "Lasso Regression Coefficients",
        las = 2, horiz=TRUE)  # Rotate x-axis labels by 90 degrees

# Plot a path diagram
plot(lasso_model$glmnet.fit, "lambda", label=FALSE)
lasso_model$glmnet.fit

# Add a title to the plot and move it up by 3mm
title(main = "Lasso Regression: Path Diagram Results", line = 2.5)








# Ridge Regression (L2 Regularization)
ridge_model <- cv.glmnet(x_train, y_train, alpha=0)

lambda.min.ridge <- ridge_model$lambda.min
lambda.1se.ridge <- ridge_model$lambda.1se



# Interpretation of cv.glmnet Function Plot

summary(ridge_model)

# Plot the results
plot(ridge_model)

# Add a title to the plot and move it up by 3mm
title(main = "Ridge Regression: CV GLMNET Results", line = 2.5)

# Plot the best λ position
abline( h = ridge_model$cvup[ridge_model$index[1]], lty = 4 )



# Model Coefficients

# Fit a Ridge model against the training set
fit_ridge <- glmnet(x_train, y_train, alpha = 0, lambda = lambda.min.ridge)

# Report on the coefficients
coef_ridge <- coef(fit_ridge)
coef_ridge

# Extract the coefficients and their names
coefficients <- as.numeric(coef(fit_ridge))
names <- colnames(train_data)

# Create a data frame for plotting
plot_data <- data.frame(names = names, coefficients = coefficients)

# Sort the coefficients in descending order
plot_data <- plot_data[order(-abs(plot_data$coefficients)), ]

# Plot the coefficients
bar_colors <- ifelse(plot_data$coefficients >= 0, "blue", "red")  # Use different colors for positive and negative coefficients
barplot(plot_data$coefficients, names.arg = plot_data$names, col = bar_colors,
        xlab = "Coefficient", ylab = "Value", main = "Ridge Regression Coefficients",
        las = 2, horiz=TRUE)  # Rotate x-axis labels by 90 degrees


# Plot a path diagram
plot(ridge_model$glmnet.fit, "lambda", label=FALSE)
ridge_model$glmnet.fit

# Add a title to the plot and move it up by 3mm
title(main = "Ridge Regression: Path Diagram Results", line = 2.5)










#--------------------------GBM--------------------------

# Training GBM
gbm_model <- train(price ~ ., data=train_data, method="gbm", trControl=train_control, verbose=FALSE)
summary(gbm_model)

# Extract feature importance from GBM model 
gbm_importance <- summary(gbm_model, plot=FALSE)

par(mar=c(5, 12, 4, 2))

# Create a barplot with rotated y-axis labels
barplot(gbm_importance$rel.inf, names.arg=rownames(gbm_importance), las=2, horiz=TRUE, main="GBM Feature Importance", xlab="Relative Importance")

# Print feature importance
print("GBM Feature Importance")
print(gbm_importance)


#--------------------------Test Model--------------------------

# Define the true values
true_values <- test_data$price

# Predictions
lasso_predictions <- predict(lasso_model, s=lasso_model$lambda.min, newx=model.matrix(price ~ ., test_data)[,-1])
ridge_predictions <- predict(ridge_model, s=ridge_model$lambda.min, newx=model.matrix(price ~ ., test_data)[,-1])
gbm_predictions <- predict(gbm_model, newdata=test_data)

# Compute MAE for each model
mae_lasso <- mean(abs(true_values - lasso_predictions))
mae_ridge <- mean(abs(true_values - ridge_predictions))
mae_gbm <- mean(abs(true_values - gbm_predictions))

# Compute R-squared for each model
rss_lasso <- sum((true_values - lasso_predictions)^2)
tss_lasso <- sum((true_values - mean(true_values))^2)
r2_lasso <- 1 - (rss_lasso/tss_lasso)

rss_ridge <- sum((true_values - ridge_predictions)^2)
tss_ridge <- sum((true_values - mean(true_values))^2)
r2_ridge <- 1 - (rss_ridge/tss_ridge)

rss_gbm <- sum((true_values - gbm_predictions)^2)
tss_gbm <- sum((true_values - mean(true_values))^2)
r2_gbm <- 1 - (rss_gbm/tss_gbm)


# Compute RMSE for each model
rmse_lasso <- rmse(test_data$price, lasso_predictions)
rmse_ridge <- rmse(test_data$price, ridge_predictions)
rmse_gbm <- rmse(test_data$price, gbm_predictions)

# Print the results
print(paste("Lasso MAE:", mae_lasso))
print(paste("Ridge MAE:", mae_ridge))
print(paste("GBM MAE:", mae_gbm))

print(paste("Lasso R^2:", r2_lasso))
print(paste("Ridge R^2:", r2_ridge))
print(paste("GBM R^2:", r2_gbm))

print(paste("Lasso RMSE:", rmse_lasso))
print(paste("Ridge RMSE:", rmse_ridge))
print(paste("GBM RMSE:", rmse_gbm))









# view(head(listings))













