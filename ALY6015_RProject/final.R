install.packages("FSAdata")
install.packages("FSA")
install.packages("dplyr")
install.packages("plotrix")
install.packages("ggplot2")
install.packages("vcd")
install.packages("corrplot")
install.packages("grid")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("psych")
install.packages("zoo")
install.packages("glmnet")
install.packages("caret")
install.packages("pROC")
install.packages("reshape2")
install.packages("viridis")

library(FSAdata)
library(FSA)
library(dplyr)
library(plotrix)
library(ggplot2)
library(vcd)
library(corrplot)
library(grid)
library(magrittr)
library(tidyverse)
library(psych)
library(zoo)
library(glmnet)
library(caret)
library(pROC)
library(reshape2)
library(viridis)


#import dataset choose a file.
listings= read.csv(file.choose(), stringsAsFactors = FALSE)
#view dataset
head(listings)
missing_values=sapply(listings, function(x) sum(is.na(x)))
print(missing_values)

# Select the variables we need(without private information and  variables which has lots of missing values)
selected_vars <- listings %>%
  select(neighbourhood_group, latitude, longitude, room_type, price, minimum_nights,
         number_of_reviews, calculated_host_listings_count, availability_365)
# 检查是否还存在缺失值
sum(is.na(selected_vars))

#name cleaned_data=selected_vars
cleaned_data=selected_vars

#Check dataset
View(cleaned_data)
str(cleaned_data)
summary(cleaned_data)

#Visual Analysis
# Plot histogram of Price Distribution with customized x-axis
price_range= range(cleaned_data$price)
hist(cleaned_data$price, breaks = seq(price_range[1], price_range[2], by = 50),
     xlim = c(0, 2000), xlab = "Price", ylab = "Frequency", main = "Price Distribution")


#Room type pie-chart
# Set bigger margin values
par(mar = c(6, 6, 6, 6))
# Calculate the frequency of room types
# Create pie chart with percentage labels
room_type_counts=table(cleaned_data$room_type)
room_type_percent= prop.table(room_type_counts) * 100
pie(room_type_counts, labels = paste0(names(room_type_counts), " (", round(room_type_percent, 1), "%)"), main = "Room Type Pie Chart")

#Average price box plot for different room_types
boxplot(price ~ room_type, data = cleaned_data, main = "Price Distribution by Room Type")

#We put private and shared rooms into one category
# Recording the 'room_type' variable
cleaned_data$room_type =ifelse(cleaned_data$room_type %in% c("Private room", "Shared room"), "not full house", "full house")
# Creating binary variables for Logistic Regression
cleaned_data$room_type_binary= ifelse(cleaned_data$room_type == "full house", 1, 0)

# Record the elements in 'neighbourhood_group' column
cleaned_data$neighbourhood_group= recode(cleaned_data$neighbourhood_group,
                                         "North Region" = 1,
                                         "Central Region" = 2,
                                         "East Region" = 3,
                                         "North-East Region" = 4,
                                         "West Region" = 5)


# View the modified dataset
View(cleaned_data)
#numerical all variables 
cleaned_data = subset(cleaned_data, select = -room_type)
cleaned_data = cleaned_data %>%
  mutate(across(everything(), as.numeric))
#---------------------------------------Splitting the Dataset---------------------------------------

# Split the dataset into training and test sets
set.seed(123)  # Set the random seed for reproducibility
train_indices <- sample(1:nrow(cleaned_data), nrow(cleaned_data)*0.7)  # 70% of the data as the training set
train_data <- cleaned_data[train_indices, ]
test_data <- cleaned_data[-train_indices, ]
#-------------------------------------MA--------------------------------
#---------------------# Step 1: Data correlation matrix-------------------------


# Calculate the correlation matrix
correlation_matrix <- cor(cleaned_data)

# Plot the correlation matrix with adjusted clarity, increased font size, and correlation values
corrplot(correlation_matrix, method = "circle", type = "upper", order = "hclust",
         tl.cex = 0.8, cl.cex = 0.8, addCoef.col = "black")

# Add a title with increased font size
title(main = "Correlation Matrix", cex.main = 1.2)

#------------------correlation values related to 'price'--------------------------

# Exclude 'price' from the correlation values related to 'price'
price_correlation <- correlation_matrix["price", ]
price_correlation <- price_correlation[-which(names(price_correlation) == "price")]
price_correlation
# Increase the size of the plot
options(repr.plot.width = 8, repr.plot.height = 6)

# Plot the correlation values related to 'price' with rotated y-axis labels, adjusted margin, and reduced font size
par(mar = c(5, 9, 4, 2) + 0.1)
barplot(price_correlation, horiz = TRUE, col = "steelblue",
        main = "Correlation with Price", xlab = "Correlation", ylab = "", las = 1,
        names.arg = , cex.axis = 0.8, cex.names = 0.7)

#------------------Predict the room_type--------------------------

# Applying regularization and Building the logistic regression model

LR_X_train <- model.matrix(room_type_binary ~ .-1, train_data)
LR_Y_train <- train_data$room_type_binary
LR_X_test <- model.matrix(room_type_binary ~ .-1, test_data)
LR_Y_test <- test_data$room_type_binary

# Applying scaling to dataset
LR_X_train <- scale(LR_X_train)
LR_X_test <- scale(LR_X_test)

# alpha=1 for lasso regression. 
LR_cv_model <- cv.glmnet(LR_X_train, LR_Y_train, family="binomial", alpha=1)
LR_cv_model

# Testing accuracy on training and testing data
# Predict on training data
LR_train_pred <- predict(LR_cv_model, s = LR_cv_model$lambda.min, newx = LR_X_train, type = "response")
LR_train_pred <- ifelse(LR_train_pred > 0.5, 1, 0) # binary conversion

# Predict on testing data

LR_test_pred <- predict(LR_cv_model, s = LR_cv_model$lambda.min, newx = LR_X_test, type = "response")
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
  geom_text(aes(label = value), color = "green", size = 14) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 14)) +
  labs(fill = "Count", x = "Actual Value", y = "Predicted Value", title = "Confusion Matrix Heatmap")

# Print the heat map
print(LR_heatmap)

# Plot a path diagram
plot(LR_cv_model$glmnet.fit, "lambda", label=TRUE, main = "Lambda path diagram")


#----------------------------------------DOU_岭回归套索回归--------------------------------------


#Ridge Regression
#2.Estimate the value of lambda.min and lambda.1se
ridge_fit <- cv.glmnet(as.matrix(train_data[,-4]), train_data$price, alpha=0)#Constructing a ridge regression model，Parameter -18 is the elimination of the effect of the predictor variables themselves, same below.
ridge_lambda_min <- ridge_fit$lambda.min
ridge_lambda_1se <- ridge_fit$lambda.1se# Calculate these two values
ridge_lambda_min
ridge_lambda_1se#Print these two values
#3.Graphing the results of the cv.glmnet function
plot(ridge_fit)

#4.Fitting a ridge regression model to the training set
ridge_model <- glmnet(as.matrix(train_data[,-4]), train_data$price, alpha=0, lambda=ridge_lambda_min)
coef(ridge_model)

#5.Calculate the RMSE on the training set
ridge_train_predictions <- predict(ridge_model, newx=as.matrix(train_data[,-4]))
ridge_train_rmse <- sqrt(mean((train_data$price - ridge_train_predictions)^2))
ridge_train_rmse

#6.Calculating the RMSE on the test set
ridge_test_predictions <- predict(ridge_model, newx=as.matrix(test_data[,-4]))
ridge_test_rmse <- sqrt(mean((test_data$price - ridge_test_predictions)^2))
ridge_test_rmse

# lasso
#7.Use the cv.glmnet function to estimate the values of lambda.min and lambda.1se, and compare and discuss
lasso_fit <- cv.glmnet(as.matrix(train_data[,-4]), train_data$price, alpha=1)
lasso_lambda_min <- lasso_fit$lambda.min
lasso_lambda_1se <- lasso_fit$lambda.1se
lasso_lambda_min
lasso_lambda_1se

#8.Graphing the results of the cv.glmnet function
plot(lasso_fit)

#9.Fit the LASSO regression model on the training set and report the coefficients
lasso_model <- glmnet(as.matrix(train_data[,-4]), train_data$price, alpha=1, lambda=lasso_lambda_min)
coef(lasso_model)

#10Compute the RMSE on the training set
lasso_train_predictions <- predict(lasso_model, newx=as.matrix(train_data[,-4]))
lasso_train_rmse <- sqrt(mean((train_data$price - lasso_train_predictions)^2))
lasso_train_rmse

#11.Calculating the RMSE on the test set
lasso_test_predictions <- predict(lasso_model, newx=as.matrix(test_data[,-4]))
lasso_test_rmse <- sqrt(mean((test_data$price - lasso_test_predictions)^2))
lasso_test_rmse

#---------------------------------YA--------------------------------------
# full model
fit.train <- lm(price~., train_data)
summary(fit.train)
fit.step <- step(fit.train, direction = "backward")
summary(fit.step)
# AIC
AIC(fit.step)
BIC(fit.step)
# RMSE
sqrt(mean(fit.step$residuals^2))
# Checking the fit to the test set
sw_pred_test <- predict(fit.step, newx = test_data)
sw_rmse_test <- sqrt(mean((test_data$price - sw_pred_test)^2))
print(sw_rmse_test)



# non-parametric method
# H0: the median price is the same across all the neighbourhood group.

# Ha: the median price is not the same across all the neighbourhood group.
result2 <- kruskal.test(price~neighbourhood_group, cleaned_data)
result2
# since the p-value is less than 0.05, we can reject the null hypothesis.