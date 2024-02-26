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
listings<- read.csv(file.choose(), stringsAsFactors = FALSE)

# Select the variables we need
selected_vars <- listings %>%
  select(neighbourhood_group, latitude, longitude, room_type, price, minimum_nights,
         number_of_reviews, calculated_host_listings_count, availability_365)

# # 根据neighbourhood_group填充缺失值
# # 根据 neighbourhood_group 分组计算每个分组的平均价格
# selected_vars <- selected_vars %>%
#   group_by(neighbourhood_group) %>%
#   mutate(price = ifelse(is.na(price), mean(price, na.rm = TRUE), price)) %>%
#   ungroup()
# 
# # 根据 neighbourhood_group 分组计算每个分组的平均 latitude
# selected_vars <- selected_vars %>%
#   group_by(neighbourhood_group) %>%
#   mutate(latitude = ifelse(is.na(latitude), mean(latitude, na.rm = TRUE), latitude)) %>%
#   ungroup()
# 
# # 根据 neighbourhood_group 分组计算每个分组的平均 longitude
# selected_vars <- selected_vars %>%
#   group_by(neighbourhood_group) %>%
#   mutate(longitude = ifelse(is.na(longitude), mean(longitude, na.rm = TRUE), longitude)) %>%
#   ungroup()
# 
# # 根据 neighbourhood_group 分组计算每个分组的平均 number_of_reviews
# selected_vars <- selected_vars %>%
#   group_by(neighbourhood_group) %>%
#   mutate(number_of_reviews = ifelse(is.na(number_of_reviews), mean(number_of_reviews, na.rm = TRUE), number_of_reviews)) %>%
#   ungroup()

# 检查是否还存在缺失值
sum(is.na(selected_vars$price))
sum(is.na(selected_vars$latitude))
sum(is.na(selected_vars$longitude))
sum(is.na(selected_vars$number_of_reviews))
#name cleaned_data=selected_vars
cleaned_data=selected_vars

#Check dataset
head(cleaned_data)
View(cleaned_data)
str(cleaned_data)
summary(cleaned_data)

#可视化分析
# 数值变量的直方图
# Plot histogram with customized x-axis
hist(cleaned_data$price, xlim = c(0, 2000),
     xlab = "Price", ylab = "Frequency", main = "Price Distribution")
hist(cleaned_data$minimum_nights,xlim = c(0, 500))
# Set bigger margin values
par(mar = c(6, 6, 6, 6))

#创建饼状图 关于room type
# Calculate the frequency of room types
room_type_counts=table(cleaned_data$room_type)
room_type_percent= prop.table(room_type_counts) * 100
pie(room_type_counts, labels = paste0(names(room_type_counts), " (", round(room_type_percent, 1), "%)"), main = "Room Type Distribution")
# Create pie chart with percentage labels
# Calculate frequency of each neighbourhood group
neighbourhood_group_counts = table(cleaned_data$neighbourhood_group)
# Calculate percentages
neighbourhood_group_percent = prop.table(neighbourhood_group_counts) * 100
pie(neighbourhood_group_counts, labels = paste0(names(neighbourhood_group_counts), " (", format(neighbourhood_group_percent, digits = 1), "%)"), main = "Neighbourhood Group Distribution")

# 不同room_type的平均价格箱线图
boxplot(price ~ room_type, data = cleaned_data)

#上面饼状图看到，只有3类，并且shared room占比很小，所以我们把private和shared room归为一类
# Recording the 'room_type' variable
cleaned_data$room_type =ifelse(cleaned_data$room_type %in% c("Private room", "Shared room"), "not full house", "full house")
# Creating binary variables for Logistic Regression
cleaned_data$room_type_binary <- ifelse(cleaned_data$room_type == "full house", 1, 0)

# Record the elements in 'neighbourhood_group' column
cleaned_data$neighbourhood_group <- recode(cleaned_data$neighbourhood_group,
                                           "North Region" = 1,
                                           "Central Region" = 2,
                                           "East Region" = 3,
                                           "North-East Region" = 4,
                                           "West Region" = 5)
# ???
cleaned_data <- subset(cleaned_data, select = -room_type)
cleaned_data <- cleaned_data %>%
  mutate(across(everything(), as.numeric))


# View the modified dataset
View(cleaned_data)
str(cleaned_data)
#---------------------------------------公用分集---------------------------------------

# 划分数据集为训练集和测试集
set.seed(123)  # 设置随机种子，以确保可重现性
train_indices <- sample(1:nrow(cleaned_data), nrow(cleaned_data)*0.7)  # 70% 数据作为训练集
train_data <- cleaned_data[train_indices, ]
test_data <- cleaned_data[-train_indices, ]

#-------------------------------------MA--------------------------------
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
price_correlation
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
LR_Y_train <- train_data$room_type_binary
LR_X_test <- model.matrix(room_type_binary ~ .-1, test_data)
LR_Y_test <- test_data$room_type_binary

# 对自变量进行标准化或缩放（如果需要）
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



#----------------------------------------DOU_岭回归套索回归--------------------------------------

# 创建训练集和测试集的自变量和因变量
RLR_X_train <- train_data[, !(names(train_data) %in% c("price"))]
RLR_y_train <- train_data$price
RLR_X_test <- test_data[, !(names(test_data) %in% c("price"))]
RLR_y_test <- test_data$price
# 对自变量进行标准化或缩放（如果需要）
RLR_X_train <- scale(RLR_X_train)
RLR_X_test <- scale(RLR_X_test)

# 拟合岭回归模型
ridge_model <- glmnet(RLR_X_train, RLR_y_train, alpha = 0)
# 使用岭回归模型进行预测
ridge_pred <- predict(ridge_model, newx = RLR_X_test)
# 绘制岭回归模型的系数图
plot(ridge_model, xvar = "lambda", label = TRUE)


# 拟合套索回归模型
lasso_model <- glmnet(RLR_X_train, RLR_y_train, alpha = 1)
# 使用套索回归模型进行预测
lasso_pred <- predict(lasso_model, newx = RLR_X_test)
# 绘制套索回归模型的系数图
plot(lasso_model, xvar = "lambda", label = TRUE)
summary(lasso_model)

# 通过计算RMSE确定拟合模型相对于训练集和测试集的性能,在ride上
ridge_pred_train <- predict(ridge_model, newx = RLR_X_train)
ridge_rmse_train <- sqrt(mean((ridge_pred_train - RLR_y_train)^2))
print(ridge_rmse_train)

ridge_pred_test <- predict(ridge_model, newx = RLR_X_test)
ridge_rmse_test <- sqrt(mean((ridge_pred_test - RLR_y_test)^2))
print(ridge_rmse_test)

# 通过计算RMSE确定拟合模型相对于训练集和测试集的性能,在lasso上
lasso_pred_train <- predict(lasso_model, newx = RLR_X_train)
lasso_rmse_train <- sqrt(mean((lasso_pred_train - RLR_y_train)^2))
print(lasso_rmse_train)

lasso_pred_test <- predict(lasso_model, newx = RLR_X_test)
lasso_rmse_test <- sqrt(mean((lasso_pred_test - RLR_y_test)^2))
print(lasso_rmse_test)

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