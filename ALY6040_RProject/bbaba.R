# Install necessary packages
install.packages(c("tidyverse", "dplyr"))
if(!require(heatmaply)) {
  install.packages("heatmaply")
}
if(!require(pheatmap)) {
  install.packages("pheatmap")
}
if(!require(plotly)) {
  install.packages("plotly")
}


# Load the libraries
library(dplyr)
library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
library(ISLR)         # contains example data set "Khan"
library(RColorBrewer) # customized coloring of plots
library(caret)
library(pROC)
library(mlr)
library(car)
library(corrplot)
library(reshape2)  # for melt
library(ggplot2)   # for ggplot
library(heatmaply)
library(pheatmap)
library(plotly)
library(readr)
listings <- read_csv("DataSets/listings.csv")



# quantiles for price
quantile(listings$price, probs = c(1/3, 2/3))

## 33.33333% 66.66667% 
##        85       164

# classify the price into 3 groups
# low: price <= 85
# middle: 85 < price <= 164
# high: price > 164
listings$group <- factor(ifelse(listings$price <= 85, "low", 
                          ifelse(listings$price > 85 & listings$price <= 164, "middle", "high")),
                   levels = c("low", "middle", "high"))


# fill missing value in 'reviews_per_month' column with 0
listings$reviews_per_month <- ifelse(is.na(listings$reviews_per_month), 0, listings$reviews_per_month)

# One-hot encode the neighbourhood_group and room_type columns
listings_encoded <- model.matrix(~ neighbourhood_group + room_type - 1, data = listings)

# Convert the matrix to a data frame
listings_encoded <- as.data.frame(listings_encoded)

# Merge with the original dataset
listings_final <- cbind(listings, listings_encoded)

# Remove the original neighbourhood_group and room_type columns
listings_final$neighbourhood_group <- NULL
listings_final$room_type <- NULL

# Check and fix column names
names(listings_final) <- make.names(names(listings_final))

# Check the column names again
str(listings_final)

# Select only numerical columns
listings_numeric <- select_if(listings_final, is.numeric)

# Scale the data
listings_scaled <- scale(listings_numeric)

# Convert listings_scaled back into a data frame
listings_scaled <- as.data.frame(listings_scaled)

# Correlation analysis
correlations <- cor(listings_scaled[, -8]) # assuming the group is the 8th column
corrplot(correlations, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, tl.cex = 0.6)

# Subset correlations for 'price'
price_correlations <- correlations['price', ]

# Sort price correlations in descending order
sorted_price_correlations <- price_correlations[order(abs(price_correlations), decreasing = TRUE)]
sorted_price_correlations <- sorted_price_correlations[-1]

# Print correlations with 'price'
print(sorted_price_correlations)

# Convert the correlations to a dataframe
correlation_df <- data.frame(
  Variable = names(sorted_price_correlations[-which(names(price_correlations) == "price")]),
  Correlation = as.vector(sorted_price_correlations[-which(names(price_correlations) == "price")])
)

# Create a barplot
ggplot(correlation_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Correlations with Price", 
       x = "Variables", 
       y = "Correlation") +
  coord_flip()  # Flipping the axes for better visibility

# Use the select function to exclude some useless column 
listings_scaled <- select(listings_scaled, -price)
listings_scaled <- select(listings_scaled, -id)
listings_scaled <- select(listings_scaled, -host_id)

# listings_scaled <- select(listings_scaled, -longitude)
# listings_scaled <- select(listings_scaled, -latitude)

# Add the target variable back into the data frame
listings_processed <- cbind(listings_scaled, group = listings$group)


summary(listings_processed)
str(listings_processed)

# Get the total number of rows in the data frame
total_rows <- nrow(listings_processed)

# Create the indices for the training data
trainIndex <- sample(seq_len(total_rows), size = floor(0.7 * total_rows), replace = FALSE)


# Create the train and test datasets
train_data <- listings_processed[trainIndex,]
test_data <- listings_processed[-trainIndex,]


# Train the SVM
svm_model <- svm(group ~ ., data = train_data, method = "C-classification", kernel = "radial")

# Predict on the test set
svm_predictions <- predict(svm_model, newdata = test_data)

# Calculate accuracy
acc <- sum(svm_predictions == test_data$group) / nrow(test_data)
print(paste("Accuracy: ", round(acc*100, 2), "%", sep=""))

# Create a task for the classification problem
task <- makeClassifTask(data = train_data, target = "group")

# Create an SVM learner
learner <- makeLearner("classif.ksvm", predict.type = "response")

# Define the parameter space
params <- makeParamSet(
  makeNumericParam("C", lower = 2^2, upper = 2^9),
  makeNumericParam("sigma", lower = 2^-9, upper = 2^2)
)

# Define a random search
random_search <- makeTuneControlRandom(maxit = 100L) # for example, perform 100 iterations

# Tune the hyperparameters
res <- tuneParams(learner, task, hout, par.set = params, control = random_search)

# Print the results
print(res)

# Extract the best parameters
best_params <- res$x

# train SVM model with the optimized parameters
svm_model_tuned <- svm(group ~ ., data = train_data, cost = best_params$C, gamma = best_params$sigma)

# Print the best model
print(svm_model_tuned)

# Predict on the test set using the tuned model
tuned_predictions <- predict(svm_model_tuned, newdata = test_data)

# Calculate accuracy
tuned_acc <- sum(tuned_predictions == test_data$group) / nrow(test_data)
print(paste("Tuned Accuracy: ", round(tuned_acc*100, 2), "%", sep=""))

# Generate confusion matrix
conf_mat <- confusionMatrix(tuned_predictions, test_data$group)

# Print confusion matrix
print(conf_mat)

# Convert confusion matrix to numeric for compatibility with heatmap
conf_mat_numeric <- as.numeric(conf_mat$table)

# Convert numeric matrix back to matrix with correct dimensions
conf_mat_matrix <- matrix(conf_mat_numeric, nrow = nrow(conf_mat$table), byrow=TRUE)

# Set row and column names to the names in the confusion matrix
rownames(conf_mat_matrix) <- rownames(conf_mat$table)
colnames(conf_mat_matrix) <- colnames(conf_mat$table)

# Create heatmap of confusion matrix
heatmaply(conf_mat_matrix, 
          xlab = "Predicted", 
          ylab = "Actual",
          colors = heat.colors(256),
          notecol = "black",  # change font color of cell annotations to black
          fontsize_row = 12, # size of row names
          fontsize_col = 12, # size of column names
          title = "Confusion Matrix Heatmap",
          showticklabels = c(T,T),  # show axis tick labels (x and y respectively)
          showtext = T) # show text in cells




















# # Function to Check for Missing Values--------------
# 
# missing_data <- function(df) {
#   missing <- sapply(df, function(x) sum(is.na(x)))
#   missing_df <- data.frame(Feature = names(missing), MissingValues = missing)
#   missing_df <- missing_df[missing_df$MissingValues > 0,]
#   return(missing_df)
# }
# 
# # Call the function
# missing_data(listings)
# 
# # Function to Check for Duplicate Values--------------
# 
# check_duplicates <- function(df) {
#   return(anyDuplicated(df))
# }
# 
# # Call the function
# check_duplicates(listings)
# 
# # Function to Identify and Count Outliers------------
# count_outliers <- function(df, column_name) {
#   IQR = IQR(df[[column_name]], na.rm = TRUE)
#   lower_bound = quantile(df[[column_name]], 0.25, na.rm = TRUE) - 1.5 * IQR
#   upper_bound = quantile(df[[column_name]], 0.75, na.rm = TRUE) + 1.5 * IQR
#   outliers = df[df[[column_name]] <= lower_bound | df[[column_name]] >= upper_bound, ]
#   return(nrow(outliers))
# }
# 
# # Call the function for 'price' column
# count_outliers(listings, 'price')
# 
# # Function to Identify and Remove Outliers--------------
# 
# remove_outliers <- function(df, column_name) {
#   IQR = IQR(df[[column_name]], na.rm = TRUE)
#   lower_bound = quantile(df[[column_name]], 0.25, na.rm = TRUE) - 1.5 * IQR
#   upper_bound = quantile(df[[column_name]], 0.75, na.rm = TRUE) + 1.5 * IQR
#   df <- df[df[[column_name]] > lower_bound & df[[column_name]] < upper_bound, ]
#   return(df)
# }
# 
# # Call the function for 'price' column
# listings <- remove_outliers(listings, 'price')
# 
# # Function to Regularize Columns--------------
# 
# encode_room_type <- function(df) {
#   df$room_type <- ifelse(df$room_type == "Private room", 1, 0)
#   return(df)
# }
# 
# normalize_cols <- function(df, cols) {
#   for (col in cols) {
#     df[[col]] <- (df[[col]] - min(df[[col]], na.rm = TRUE)) / (max(df[[col]], na.rm = TRUE) - min(df[[col]], na.rm = TRUE))
#   }
#   return(df)
# }
# 
# # Call the functions
# listings <- encode_room_type(listings)
# listings <- normalize_cols(listings, c("latitude", "longitude"))
# 
# # Calculate correlations--------------
# 
# correlations <- cor(listings %>% select_if(is.numeric), use="complete.obs")
# 
# # Show correlations as heatmap
# heatmap(correlations)