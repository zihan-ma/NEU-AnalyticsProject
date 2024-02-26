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
# install.packages("viridis")


#REAL NEEDED LIBRARIES
# library(glmnet)
# library(caret)
# library(pROC)
# library(reshape2)
# library(viridis)  # Required for the "viridis" color palette

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
library(tidyverse)
library(broom)
library(pROC)
library(readr)
library(reshape2)
library(viridis)  # Required for the "viridis" color palette



# using read.csv
listings <- read.csv(file.choose(), stringsAsFactors = FALSE)


# 
# 
# View(listings)
# 
# str(listings)
# summary(listings)
# 
# 
# 
# #----------
# 
# # Step 1: Data Preprocessing
# filtered_data <- listings %>% 
#   select(neighbourhood_group, latitude, longitude, room_type, price, minimum_nights, number_of_reviews, calculated_host_listings_count, availability_365)
# 
# # Recoding the 'room_type' variable
# filtered_data$room_type <- ifelse(filtered_data$room_type %in% c("Private room", "Shared room"), "not full house", "full house")
# 
# # Creating binary variables for Logistic Regression
# filtered_data$room_type_binary <- ifelse(filtered_data$room_type == "full house", 1, 0)
# 
# # Recode the elements in 'neighbourhood_group' column
# filtered_data$neighbourhood_group <- recode(filtered_data$neighbourhood_group,
#                                             "North Region" = 1,
#                                             "Central Region" = 2,
#                                             "East Region" = 3,
#                                             "North-East Region" = 4,
#                                             "West Region" = 5)
# 
# # remove `room_type` from dataframe
# filtered_data <- subset(filtered_data, select = -room_type)

#--------------------------filtered dataset----------------------------------

# # View the modified dataset
# View(filtered_data)
# 
# # Convert necessary columns to numeric if needed
# filtered_data_cor$price <- as.numeric(filtered_data_cor$price)
# filtered_data_cor$minimum_nights <- as.numeric(filtered_data_cor$minimum_nights)
# filtered_data_cor$number_of_reviews <- as.numeric(filtered_data_cor$number_of_reviews)
# filtered_data_cor$calculated_host_listings_count <- as.numeric(filtered_data_cor$calculated_host_listings_count)
# filtered_data_cor$availability_365 <- as.numeric(filtered_data_cor$availability_365)
# 

#---------------------# Step 1: Data correlation matrix-------------------------


# Calculate the correlation matrix
correlation_matrix <- cor(cleaned_data)

# Increase the size of the plot window
windows(width = 10, height = 8)

# Plot the correlation matrix with adjusted clarity, increased font size, and correlation values
corrplot(correlation_matrix, method = "circle", type = "upper", order = "hclust",
         tl.cex = 0.8, cl.cex = 0.8, addCoef.col = "black")

# Add a title with increased font size
title(main = "Correlation Matrix", cex.main = 1.2)

# Create a custom color legend with increased font size
legend("bottomleft", legend = c("Strong Negative", "Weak Negative", "No Correlation", "Weak Positive", "Strong Positive"),
       fill = c("blue", "lightblue", "white", "pink", "red"), title = "Correlation",
       cex = 0.9, bty = "n", y.intersp = 0.8)

#------------------correlation values related to 'price'--------------------------

# Exclude 'price' from the correlation values related to 'price'
price_correlation <- correlation_matrix["price", ]
price_correlation <- price_correlation[-which(names(price_correlation) == "price")]

# Increase the size of the plot
options(repr.plot.width = 8, repr.plot.height = 6)

# Plot the correlation values related to 'price' with rotated y-axis labels, adjusted margin, and reduced font size
par(mar = c(5, 9, 4, 2) + 0.1)
barplot(price_correlation, horiz = TRUE, col = "steelblue",
        main = "Correlation with Price", xlab = "Correlation", ylab = "", las = 1,
        names.arg = , cex.axis = 0.8, cex.names = 0.7)




# # Step 2: Standardizing numeric variables
# # Apply standardization for all columns except the binary 'room_type_binary'
# cols_to_normalize <- setdiff(names(clened_data), "room_type_binary")
# clened_data[cols_to_normalize] <- scale(clened_data[cols_to_normalize])
# 
# # Step 3: Splitting the data into training and testing sets
# set.seed(123) # for reproducibility
# train_index <- sample(1:nrow(clened_data), nrow(clened_data)*0.7)
# train_data <- clened_data[train_index, ]
# test_data <- clened_data[-train_index, ]

# Applying regularization and Building the logistic regression model

LR_X_train <- model.matrix(room_type_binary ~ .-1, train_data)
LR_X_train <- train_data$room_type_binary

# 对自变量进行标准化或缩放（如果需要）
LR_X_train <- scale(RLR_X_train)
LR_X_test <- scale(RLR_X_test)

# alpha=1 for lasso regression. 
LR_cv_model <- cv.glmnet(LR_X_train, LR_Y_train, family="binomial", alpha=1)
LR_cv_model

# Testing accuracy on training and testing data
# Predict on training data
LR_train_pred <- predict(lr_cv_model, s = lr_cv_model$lambda.min, newx = X_train, type = "response")
LR_train_pred <- ifelse(LR_train_pred > 0.5, 1, 0) # binary conversion

# Predict on testing data
LR_X_test <- model.matrix(room_type_binary ~ .-1, test_data)
LR_Y_test <- test_data$room_type_binary
LR_test_pred <- predict(LR_cv_model, s = LR_cv_model$lambda.min, newx = X_test, type = "response")
LR_test_pred <- ifelse(LR_test_pred > 0.5, 1, 0) # binary conversion

#-----------------------------Accuracy------------------------------

# Accuracy
LR_train_acc <- sum(LR_train_pred == LR_Y_train) / nrow(train_data)
LR_test_acc <- sum(LR_test_pred == LR_Y_test) / nrow(test_data)

cat("Training Accuracy: ", LR_train_acc, "\n")
cat("Testing Accuracy: ", LR_test_acc)

#-----------------------------confusion matrix------------------------------

# Confusion matrix and heat map
LR_conf_matrix <- confusionMatrix(as.factor(LR_test_pred), as.factor(LR_Y_test))

# Melt the confusion matrix to be suitable for ggplot
LR_melted_cm <- melt(LR_conf_matrix$table, varnames = c("Predicted", "Actual"))

# Create a heat map of the confusion matrix with improved color scheme
LR_heatmap <- ggplot(data = LR_melted_cm, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_viridis(option = "plasma", direction = -1) +  # Using "viridis" color palette
  geom_text(aes(label = value), color = "green", size = 4) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 14)) +
  labs(fill = "Count", x = "Actual Value", y = "Predicted Value", title = "Confusion Matrix Heatmap")

# Print the heat map
print(LR_heatmap)
















