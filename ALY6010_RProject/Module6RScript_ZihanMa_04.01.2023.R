# Module6RScript_ZihanMa_04.1.2023
# Week 6
# R Script.R
# Student Name: Zihan Ma
# Class Name: ALY6010: Probability Theory and Introductory Statistics
# Instructor: Tom Breur

# Libraries used in this script
library(rmarkdown)
library(readxl)
library(readr)
library(tidyverse)
library(dplyr)
library(knitr)
library(kableExtra)
library(magrittr)
library(RColorBrewer)
library(FSA)
library(ggplot2)
library(corrplot)
library(stargazer)
library(ggcorrplot)

# Data imported from "Fruit_Vegetable_Market.csv"
library(readr)
Fruit_Vegetable_Market <- read_csv("DataSets/Fruit_Vegetable_Market.csv")
summary(Fruit_Vegetable_Market)

# Convert character columns to factors
Fruit_Vegetable_Market$ORGANIC <- as.factor(Fruit_Vegetable_Market$ORGANIC)

# Remove rows with missing prices or missing organic status
clean_data <- Fruit_Vegetable_Market %>% filter(!is.na(WEIGHTED_AVERAGE_PRICE) & !is.na(ORGANIC))

# Filter the data to only include observations with valid organic status
clean_data <- clean_data %>% filter(!is.na(ORGANIC))

# Create a new factor variable for organic status with nicer labels
clean_data <- clean_data %>% mutate(ORGANIC_FAC = recode_factor(ORGANIC, "Yes" = "Organic", "No" = "Conventional"))

# boxplot visualization 
ggplot(clean_data, aes(x = ORGANIC_FAC, y = WEIGHTED_AVERAGE_PRICE, fill = ORGANIC_FAC)) +
  geom_boxplot() +
  labs(title = "Table 1.1 Price vs. Organic Status", x = "", y = "Weighted Average Price (USD)") +
  scale_fill_manual(values = c("NO" = "#56B4E9", "YES" = "#E69F00")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.position = "none")

# Create a new factor variable for organic status with nicer labels
clean_data <- clean_data %>% mutate(ORGANIC_FAC = recode_factor(ORGANIC, "YES" = "Organic", "NO" = "Conventional"))

# Create a dummy variable for organic status using ORGANIC_FAC
clean_data <- clean_data %>% mutate(ORGANIC_dummy = ifelse(ORGANIC_FAC == "Organic", 1, 0))

# Regression model
organic_price_model <- lm(WEIGHTED_AVERAGE_PRICE ~ ORGANIC_dummy, data = clean_data)
summary(organic_price_model)

ggplot(clean_data, aes(x = ORGANIC_dummy, y = WEIGHTED_AVERAGE_PRICE)) +
  geom_jitter(width = 0.1, alpha = 0.2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "solid") +
  labs(title = "Weighted Average Price vs. Organic Status",
       x = "Organic Status (0 = Conventional, 1 = Organic)",
       y = "Weighted Average Price (USD)") +
  scale_x_continuous(breaks = c(0, 1), labels = c("Conventional", "Organic")) +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10))

ggplot(price_difference_by_LEVEL_1, aes(x = reorder(LEVEL_1, price_difference), y = price_difference, fill = price_difference)) +
  geom_col() +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(face = "bold", size = 12),
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5)) +
  labs(title = "Price Difference Between Organic and Non-Organic Products",
       y = "Price Difference (USD)") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = median(price_difference_by_LEVEL_1$price_difference), guide = "none")

apple_banana_asparagus_data <- Fruit_Vegetable_Market %>% 
  filter(LEVEL_1 %in% c("APPLES", "BANANAS", "ASPARAGUS"))

# Create a new factor variable for organic status with nicer labels
apple_banana_asparagus_data <- apple_banana_asparagus_data %>% 
  mutate(ORGANIC_FAC = recode_factor(ORGANIC, "YES" = "Organic", "NO" = "Conventional"))

# Create a dummy variable for organic status using ORGANIC_FAC
apple_banana_asparagus_data <- apple_banana_asparagus_data %>% 
  mutate(ORGANIC_dummy = ifelse(ORGANIC_FAC == "Organic", 1, 0))

# Regression model for each product category
apple_model <- lm(WEIGHTED_AVERAGE_PRICE ~ ORGANIC_dummy, data = apple_banana_asparagus_data %>% filter(LEVEL_1 == "APPLES"))
banana_model <- lm(WEIGHTED_AVERAGE_PRICE ~ ORGANIC_dummy, data = apple_banana_asparagus_data %>% filter(LEVEL_1 == "BANANAS"))
asparagus_model <- lm(WEIGHTED_AVERAGE_PRICE ~ ORGANIC_dummy, data = apple_banana_asparagus_data %>% filter(LEVEL_1 == "ASPARAGUS"))

# Summary of regression results for each product category
cat("Regression results for Apples:\n")
summary(apple_model)

cat("Regression results for Bananas:\n")
summary(banana_model)

cat("Regression results for Asparagus:\n")
summary(asparagus_model)


# Create a scatterplot with regression lines for each product category
ggplot(data = apple_banana_asparagus_data, aes(x = ORGANIC_dummy, y = WEIGHTED_AVERAGE_PRICE, color = LEVEL_1)) + 
  geom_point(alpha = 0.5, position = position_jitter(width = 0.05, height = 0.05)) +  # Adding noise
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
  facet_wrap(~LEVEL_1) +
  scale_color_manual(values = c("darkred", "darkblue", "darkgreen")) +
  labs(title = "Relationship between Organic Status and Weighted Average Price",
       x = "Organic Status", y = "Weighted Average Price", color = "Product Category")