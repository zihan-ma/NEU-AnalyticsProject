# ALY6015_Module1_Code_ZihanMa.r
# 06.12.2023
# Week 3
# Student Name: Zihan Ma
# Class Name: ALY6015 80472 
#             Intermediate Analytics SEC 04 Spring 2023 CPS [BOS-B-HY]
# Instructor:  Prof. Valeriy Shevchenko



# Un-comment to install package as needed
# install.packages("tidyverse")
# install.packages("gridExtra")
# install.packages("rlang")
# install.packages("caret")
# install.packages("stats")
# install.packages("reshape2")
# install.packages("pROC")

# The libraries actually used in this script.
library(dplyr)    # for data manipulation
library(readr)    # for inputing data 
library(ggplot2)  # for data visualization
library(gridExtra)    # arrange multiple plots in a grid layout
library(caret)        # use the createDataPartition function
library(stats)    # for using the glm() function
library(reshape2) # for melt()
library(pROC)     # for melt()

# --------------------------p1----------------------------

# Read College data set. When prompted navigate and open College.csv file
College <- read.table(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)
View(College)

# Show the variable names, data types, and an overview of the data set.
str(College)

# provides summary statistics for each variable
summary(College)

# Define plots for Apps, Top10perc, and Private
plot1 <- ggplot(College, aes(x=Apps)) +
  geom_histogram(binwidth=500, fill="steelblue", color="black", alpha=0.7) +
  labs(title="Distribution of Number of Applications", 
       x="Number of Applications", 
       y="Frequency") +
  theme_minimal()

plot2 <- ggplot(College, aes(x=Private, y=Apps, fill=Private)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("blue", "green")) +
  labs(title="Number of Applications by Type of College", 
       x="Type of College", 
       y="Number of Applications") +
  theme_minimal()

plot3 <- ggplot(College, aes(x=Top10perc)) +
  geom_histogram(binwidth=5, fill="steelblue", color="black", alpha=0.7) +
  labs(title="Distribution of Top10perc", 
       x="% of Students from Top 10% of High School Class", 
       y="Frequency") +
  theme_minimal()

plot4 <- ggplot(College, aes(x=Private, y=Top10perc, fill=Private)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values=c("blue", "green")) +
  labs(title="Top10perc by Type of College", 
       x="Type of College", 
       y="% of Students from Top 10% of High School Class") +
  theme_minimal()

# Use grid.arrange to organize your plots
grid.arrange(plot1, plot2, plot3, plot4, nrow=2)

# --------------------------q2----------------------------

# Make a copy for Private convert process
College_numeric = College



# Convert Private into a binary numeric variable
College_numeric$Private <- ifelse(College_numeric$Private == "Yes", 1, 0)

# Set the seed for reproducibility
set.seed(3456)

# Specify the proportion of data to allocate to the training set
trainIndex <- createDataPartition(College_numeric$Private, p = .8, list = FALSE, times = 1)

# Create the training and testing datasets
train <- College_numeric[trainIndex,]
test <- College_numeric[-trainIndex,]

# check proportion of train and test data set
nrow(train)/(nrow(train)+nrow(test))
nrow(test)/(nrow(train)+nrow(test))

# --------------------------q3----------------------------

# Fit a logistic regression model
logistic_model <- glm(Private ~ Apps + Top10perc, data = train, family = binomial)

# Output the summary of the model
summary(logistic_model)

# --------------------------q4----------------------------

# Generate predicted probabilities
predicted_probs <- predict(logistic_model, newdata = train, type = "response")

# Convert probabilities to predictions using a threshold of 0.5
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

# Create a confusion matrix
cm <- confusionMatrix(table(predicted_classes, train$Private))
print(cm)

# Calculate metrics
TP <- cm$table[2,2]
FP <- cm$table[2,1]
TN <- cm$table[1,1]
FN <- cm$table[1,2]

Accuracy  <- (TP + TN) / (TP + FP + TN + FN)
Precision <- TP / (TP + FP)
Recall <- TP / (TP + FN) # also known as Sensitivity
Specificity <- TN / (TN + FP)

# Print metrics
print(paste("Accuracy: ", Accuracy))
print(paste("Precision: ", Precision))
print(paste("Recall (Sensitivity): ", Recall))
print(paste("Specificity: ", Specificity))

# melt cm to be suitable for ggplot
melted_cm <- melt(cm$table, varnames = c("Predicted", "Actual"))

# Create a heat map of the confusion matrix
heatmap <- ggplot(data = melted_cm, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = value), color = "black", size = 4) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 14)) +
  labs(fill = "Count", x = "Actual Value", y = "Predicted Value", title = "Confusion Matrix Heatmap")

# Print the heat map
print(heatmap)

# --------------------------q5----------------------------

# No code for this part

# --------------------------q6----------------------------

# Make predictions on the test set
predicted_classes_test <- ifelse(predict(logistic_model, newdata = test, type = "response") > 0.5, 1, 0)

# Create a confusion matrix
cm_test <- confusionMatrix(table(predicted_classes_test, test$Private))
print(cm_test)

# Calculate metrics
TP_test <- cm_test$table[2,2]
FP_test <- cm_test$table[2,1]
TN_test <- cm_test$table[1,1]
FN_test <- cm_test$table[1,2]

Accuracy_test  <- (TP_test + TN_test) / (TP_test + FP_test + TN_test + FN_test)
Precision_test <- TP_test / (TP_test + FP_test)
Recall_test <- TP_test / (TP_test + FN_test) # also known as Sensitivity
Specificity_test <- TN_test / (TN_test + FP_test)

# Print metrics
print(paste("Accuracy: ", Accuracy_test))
print(paste("Precision: ", Precision_test))
print(paste("Recall (Sensitivity): ", Recall_test))
print(paste("Specificity: ", Specificity_test))

# Melt cm_test to be suitable for ggplot
melted_cm_test <- melt(cm_test$table, varnames = c("Predicted", "Actual"))

# Create a heatmap of the confusion matrix for the test set
heatmap_test <- ggplot(data = melted_cm_test, aes(x = Actual, y = Predicted, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  geom_text(aes(label = value), color = "black", size = 4) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12)) +
  theme(axis.title = element_text(size = 14)) +
  labs(fill = "Count", x = "Actual Value", y = "Predicted Value", title = "Confusion Matrix Heatmap - Test Data")

# Print the heatmap for the test set
print(heatmap_test)

# --------------------------q7----------------------------

# Predict probabilities on the test set
probs <- predict(logistic_model, newdata = test, type = "response")

# Compute the ROC curve
roc_obj <- roc(test$Private, probs)

# Calculate AUC
auc <- round(auc(roc_obj), 4)

# Create ROC plot
roc_plot <- ggroc(roc_obj, colour = 'steelblue', size = 2) +
  ggtitle(paste0('ROC Curve ', '(AUC = ', auc, ')')) +
  theme_minimal() 

# Print ROC plot
print(roc_plot)
