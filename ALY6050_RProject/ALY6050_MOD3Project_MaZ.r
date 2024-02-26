# ALY6050_MOD3Project_MaZ.r
# 06.13.2023
# Week 3
# Student Name: Zihan Ma
# Class Name: ALY6050 80478 
#             Intro to Enterprise Analytics SEC 09 Spring 2023 CPS
# Instructor:  Prof. Richard He

# Un-comment to install package as needed
# install.packages("tidyverse")
# install.packages("knitr")
# install.packages("forecast")

# Libraries used in this script
library(tidyverse)    # A collection of basic R packages such as ggplot2, tibble, tidyr, readr, dplyr, stringr, purr, and forcats.
library(knitr)        # For print result in table
library(forecast)     # Provides methods and tools for displaying and analysing univariate time series forecasts

# --------------------------BASE DATA----------------------------

# Read stock data set. When prompted navigate and open ALY6050_MOD3Project_Data_2023SpringB.csv file
stock <- read.table(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)
View(stock)

# Show the variable names, data types, and an overview of the data set.
str(stock)

# provides summary statistics for each variable
summary(stock)



# --------------------------q1.1----------------------------

# Create a subset of data excluding rows with NA values
stock_subset <- stock[!is.na(stock$NFLX),]

# Reshape the data to a longer format
stock_long <- stock_subset %>% 
  pivot_longer(cols = c(AMZN, NFLX), 
               names_to = "Company", 
               values_to = "Stock_Price")

# Plot
ggplot(data = stock_long, aes(x = Date, y = Stock_Price, color = Company)) +
  geom_line() +
  scale_color_manual(values = c("blue", "red")) +
  labs(title = 'Stock Prices of Amazon and Netflix',
       x = 'Date',
       y = 'Stock Price',
       color = 'Company') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8, 0.9),
        legend.background = element_rect(fill = "white", size = 0.5, linetype = "solid", colour = "black"))

  # --------------------------q1.2----------------------------
  
# Function to compute forecasts using exponential smoothing
exponential_smoothing <- function(stock_subset, alpha){
  # Initialize forecast with the first data point
  forecast <- rep(stock_subset[1], length(stock_subset))
  
  # Apply exponential smoothing
  for(t in 2:length(stock_subset)){
    forecast[t] <- alpha * stock_subset[t-1] + (1 - alpha) * forecast[t-1]
  }
  
  return(forecast)
}

alpha_values <- c(0.2, 0.4, 0.6, 0.8)

result <- data.frame(
  Alpha = alpha_values,
  MAPD_AMZN = rep(0, length(alpha_values)),
  MAPD_NFLX = rep(0, length(alpha_values))
)

for(i in 1:length(alpha_values)){
  alpha <- alpha_values[i]
  
  # Compute forecasts
  forecastA <- exponential_smoothing(stock_subset$AMZN, alpha)
  forecastB <- exponential_smoothing(stock_subset$NFLX, alpha)
  
  # Compute MAPD
  MAPD_A <- mean(abs((stock_subset$AMZN - forecastA) / stock_subset$AMZN)) * 100
  MAPD_N <- mean(abs((stock_subset$NFLX - forecastB) / stock_subset$NFLX)) * 100
  
  # Store results
  result$MAPD_AMZN[i] <- MAPD_A
  result$MAPD_NFLX[i] <- MAPD_N
}

print(kable(result, 
            col.names = c("Alpha", "MAPD_AMZN", "MAPD_NFLX"),
            caption = "MAPD for different Alpha values for AMZN and NFLX",
            align = 'c'))



# --------------------------q1.3----------------------------

# Function to compute forecasts using double exponential smoothing
double_exponential_smoothing <- function(stock_subset, alpha, beta){
  # Initialize forecast and trend
  forecast <- rep(0, length(stock_subset))
  trend <- rep(0, length(stock_subset))
  
  # Initialize the first forecast and trend
  forecast[1] <- stock_subset[1]
  trend[1] <- stock_subset[2] - stock_subset[1]
  
  # Apply double exponential smoothing
  for(t in 2:length(stock_subset)){
    forecast[t] <- alpha * stock_subset[t-1] + (1 - alpha) * (forecast[t-1] + trend[t-1])
    trend[t] <- beta * (forecast[t] - forecast[t-1]) + (1 - beta) * trend[t-1]
  }
  
  # Final forecast is the sum of the forecasted level and trend
  return(forecast + trend)
}

alpha <- 0.6
beta_values <- c(0.2, 0.4, 0.6, 0.8)

result <- data.frame(
  Beta = beta_values,
  MAPD_AMZN = rep(0, length(beta_values)),
  MAPD_NFLX = rep(0, length(beta_values))
)

for(i in 1:length(beta_values)){
  beta <- beta_values[i]
  
  # Compute forecasts
  forecastA <- double_exponential_smoothing(stock_subset$AMZN, alpha, beta)
  forecastB <- double_exponential_smoothing(stock_subset$NFLX, alpha, beta)
  
  # Compute MAPD
  MAPD_A <- mean(abs((stock_subset$AMZN - forecastA) / stock_subset$AMZN)) * 100
  MAPD_N <- mean(abs((stock_subset$NFLX - forecastB) / stock_subset$NFLX)) * 100
  
  # Store results
  result$MAPD_AMZN[i] <- MAPD_A
  result$MAPD_NFLX[i] <- MAPD_N
}

print(kable(result, 
            col.names = c("Beta", "MAPD_AMZN", "MAPD_NFLX"),
            caption = "MAPD for different Beta values for AMZN and NFLX",
            align = 'c'))

# --------------------------q2.1----------------------------

# Apply 3-period weighted moving average on AMZN and NFLX
weights <- c(0.5, 0.3, 0.2)

# Use rollapply from zoo package to apply moving average with custom weights
library(zoo)
stock_subset$AMZN_ma <- rollapply(stock_subset$AMZN, width = 3, FUN = function(x) sum(x * weights), align = "right", fill = NA)
stock_subset$NFLX_ma <- rollapply(stock_subset$NFLX, width = 3, FUN = function(x) sum(x * weights), align = "right", fill = NA)

# Use linear model (lm) to predict trend
stock_subset$period <- 1:nrow(stock_subset)
model_AMZN <- lm(AMZN ~ period, data = stock_subset[51:252,])
model_NFLX <- lm(NFLX ~ period, data = stock_subset[51:252,])

# Creating a new dataframe for prediction
pred_data <- data.frame(period = 51:262)

# Use the linear models to predict future values
pred_data$AMZN_predicted <- predict(model_AMZN, newdata = pred_data)
pred_data$NFLX_predicted <- predict(model_NFLX, newdata = pred_data)

# Merging the predicted values back into the original dataframe
stock_subset <- merge(stock_subset, pred_data, by = "period", all.x = TRUE)

# Compute MAPDs for periods 4 to 252
stock_subset$AMZN_mapd <- abs((stock_subset$AMZN - stock_subset$AMZN_ma) / stock_subset$AMZN) * 100
stock_subset$NFLX_mapd <- abs((stock_subset$NFLX - stock_subset$NFLX_ma) / stock_subset$NFLX) * 100

mean_mapd_AMZN <- mean(stock_subset$AMZN_mapd[4:252], na.rm = TRUE)
mean_mapd_NFLX <- mean(stock_subset$NFLX_mapd[4:252], na.rm = TRUE)

AMZN_actual <- c(162.85,159.33,160.93,156.92,158.11,159.13,155.71,156.04,158.29,156.37) # actual values
NFLX_actual <- c(540.73,522.86,520.80,500.49,508.89,510.40,499.10,494.25,507.79,500.86) # actual values

# Create a subset for periods 253-262
forecast_accuracy <- stock_subset[253:262, c("period", "AMZN_predicted", "NFLX_predicted")]

# Add actual prices to the dataframe
forecast_accuracy$AMZN_actual <- AMZN_actual
forecast_accuracy$NFLX_actual <- NFLX_actual

# Add predicted prices to the dataframe
forecast_accuracy$AMZN_predicted <- pred_data$AMZN_predicted[203:212]
forecast_accuracy$NFLX_predicted <- pred_data$NFLX_predicted[203:212]

# Add period to the dataframe
forecast_accuracy$period <- c(253:262)


# Calculate forecast errors (actual - predicted)
forecast_accuracy$AMZN_error <- forecast_accuracy$AMZN_actual - forecast_accuracy$AMZN_predicted
forecast_accuracy$NFLX_error <- forecast_accuracy$NFLX_actual - forecast_accuracy$NFLX_predicted

# Calculate MAPE (Mean Absolute Percentage Error)
forecast_accuracy$AMZN_MAPE <- mean(abs(forecast_accuracy$AMZN_error / forecast_accuracy$AMZN_actual)) * 100
forecast_accuracy$NFLX_MAPE <- mean(abs(forecast_accuracy$NFLX_error / forecast_accuracy$NFLX_actual)) * 100

# Print the summary table
print(kable(forecast_accuracy, 
            col.names = c("Period", "AMZN_Predicted", "NFLX_Predicted", "AMZN_Actual", "NFLX_Actual", 
                          "AMZN_Error", "NFLX_Error", "AMZN_MAPE", "NFLX_MAPE"), 
            caption = "Forecast Accuracy for Periods 253-262 for AMZN and NFLX", 
            align = 'c'))

# Reshape the data to a longer format
forecast_accuracy_long <- forecast_accuracy %>%
  pivot_longer(cols = c(AMZN_predicted, NFLX_predicted, AMZN_actual, NFLX_actual), 
               names_to = "Variable", 
               values_to = "Value")

# Plot
ggplot(data = forecast_accuracy_long, aes(x = period, y = Value, color = Variable)) +
  geom_line() +
  scale_color_manual(values = c("blue", "red", "green", "purple")) +
  labs(title = 'Comparison of Predicted and Actual Values for AMZN and NFLX',
       x = 'Period',
       y = 'Value',
       color = 'Variable') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.8, 0.9),
        legend.background = element_rect(fill = "white", size = 0.5, linetype = "solid", colour = "black"))

# --------------------------q3.1----------------------------

# Fit the linear model
model_AMZN <- lm(AMZN ~ Period, data = stock_subset[1:252,])

# Predict the values for periods 1 through 262
AMZN_predicted <- predict(model_AMZN, newdata = data.frame(Period = 1:262))

# Fit the linear model
model_NFLX <- lm(NFLX ~ Period, data = stock_subset[1:252,])

# Predict the values for periods 1 through 262
NFLX_predicted <- predict(model_NFLX, newdata = data.frame(Period = 1:262))

# Calculate the MAPD for AMZN
AMZN_actual <- stock_subset$AMZN[1:262]
AMZN_error <- abs(AMZN_predicted - AMZN_actual) / AMZN_actual
AMZN_MAPE <- mean(AMZN_error, na.rm = TRUE) * 100

# Calculate the MAPD for NFLX
NFLX_actual <- stock_subset$NFLX[1:262]
NFLX_error <- abs(NFLX_predicted - NFLX_actual) / NFLX_actual
NFLX_MAPE <- mean(NFLX_error, na.rm = TRUE) * 100

# Create a data frame to hold the MAPDs
mapd_df <- data.frame(
  Part = c("Part3", "Part1", "Part2"),
  MAPD_AMZN = c(AMZN_MAPE, 1.793166, 12.40852), 
  MAPD_NFLX = c(NFLX_MAPE, 2.024692, 6.715306) 
)

# Print the data frame in a pretty table format
print(kable(mapd_df, format = "pipe", col.names = c("Part", "MAPD_AMZN", "MAPD_NFLX"), align = c('c','c','c')))

# --------------------------q3.2----------------------------

# Calculate residuals for both models
residuals_AMZN <- model_AMZN$residuals
residuals_NFLX <- model_NFLX$residuals

# Change the layout to have 2 rows and 1 column
par(mfrow=c(2,1))

# For Checking Independence of Residuals
# For AMZN
plot(residuals_AMZN, main = "Residuals of AMZN", xlab = "Index", ylab = "Residuals")
# For NFLX
plot(residuals_NFLX, main = "Residuals of NFLX", xlab = "Index", ylab = "Residuals")

# Reset to default layout
par(mfrow=c(1,1))

# For Checking Homoscedasticity of residuals
par(mfrow=c(2,1))

# For AMZN
plot(fitted(model_AMZN), residuals_AMZN, main = "AMZN Residuals vs Fitted", 
     xlab = "Fitted values", ylab = "Residuals")

# For NFLX
plot(fitted(model_NFLX), residuals_NFLX, main = "NFLX Residuals vs Fitted", 
     xlab = "Fitted values", ylab = "Residuals")

par(mfrow=c(1,1)) 

# For Checking Normality of residuals
par(mfrow=c(2,1))

# For AMZN
qqnorm(residuals_AMZN, main = "AMZN QQ plot")
qqline(residuals_AMZN)

# For NFLX
qqnorm(residuals_NFLX, main = "NFLX QQ plot")
qqline(residuals_NFLX)

par(mfrow=c(1,1))

# For AMZN
shapiro.test(residuals_AMZN)

# For NFLX
shapiro.test(residuals_NFLX)

# --------------------------q4.1----------------------------

# Shift the AMZN and NFLX prices by 1 to create the predicted prices
stock_subset$AMZN_predicted <- c(NA, stock_subset$AMZN[-length(stock_subset$AMZN)])
stock_subset$NFLX_predicted <- c(NA, stock_subset$NFLX[-length(stock_subset$NFLX)])

# Calculate the absolute percentage deviations for AMZN and NFLX
stock_subset$AMZN_apd <- abs(stock_subset$AMZN - stock_subset$AMZN_predicted) / stock_subset$AMZN * 100
stock_subset$NFLX_apd <- abs(stock_subset$NFLX - stock_subset$NFLX_predicted) / stock_subset$NFLX * 100

# Calculate the mean absolute percentage deviations for AMZN and NFLX
AMZN_MAPD <- mean(stock_subset$AMZN_apd, na.rm = TRUE)
NFLX_MAPD <- mean(stock_subset$NFLX_apd, na.rm = TRUE)

# Print the results
print(paste("AMZN MAPD: ", AMZN_MAPD))
print(paste("NFLX MAPD: ", NFLX_MAPD))

# --------------------------q4.2----------------------------

# Baseline model MAPD data
baseline_data <- data.frame(Part = "Baseline",
                            MAPD_AMZN = 1.79196556080498, 
                            MAPD_NFLX = 2.10756199248139)

# Append the baseline data to the existing data frame
mapd_df <- rbind(mapd_df, baseline_data)

# Print the updated data frame
print(kable(mapd_df, format = "pipe", col.names = c("Part", "MAPD_AMZN", "MAPD_NFLX"), align = c('c','c','c')))



# --------------------------q4----------------------------


# --------------------------q4----------------------------


# --------------------------q4----------------------------


# --------------------------q4----------------------------


# --------------------------q4----------------------------


# --------------------------q4----------------------------


# --------------------------q4----------------------------


# --------------------------q4----------------------------


# --------------------------q4----------------------------


