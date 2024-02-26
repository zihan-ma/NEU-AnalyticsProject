# Module6FinalPriject_ZihanMa_04.1.2023
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

# Data imported from "beer_reviews.csv"
library(readr)
beer_reviews <- read_csv("DataSets/beer_reviews.csv")

# Summary statistics
summary(beer_reviews)

# Distribution of overall review ratings
ggplot(beer_reviews, aes(x = review_overall)) +
  geom_histogram(binwidth = 0.5, fill = "dodgerblue", color = "black") +
  labs(title = "Table 1.1: Distribution of Overall Review Ratings",
       x = "Overall Review Rating", y = "Frequency") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Calculate the mean of the finite beer_abv values
mean_beer_abv <- mean(beer_reviews$beer_abv, na.rm = TRUE)

# Replace non-finite values with the mean value
beer_reviews$beer_abv_imputed <- ifelse(is.na(beer_reviews$beer_abv) | !is.finite(beer_reviews$beer_abv),
                                        mean_beer_abv, beer_reviews$beer_abv)

# Distribution of beer ABV using imputed values
ggplot(beer_reviews, aes(x = beer_abv_imputed)) +
  geom_histogram(binwidth = 1, fill = "firebrick", color = "black") +
  labs(title = "Table 1.2: Distribution of Beer ABV (Imputed)",
       x = "Beer ABV (%)", y = "Frequency") +
  scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, by = 1)) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal()

# Calculate the total number of reviews
total_reviews <- nrow(beer_reviews)

# Number of reviews by beer style (top 10) with percentage
beer_style_counts <- beer_reviews %>%
  group_by(beer_style) %>%
  summarise(n = n()) %>%
  mutate(percentage = n / total_reviews * 100) %>%
  arrange(desc(n)) %>%
  head(10)

# Plot with percentages
ggplot(beer_style_counts, aes(x = reorder(beer_style, n), y = n)) +
  geom_bar(stat = "identity", fill = "forestgreen") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), 
            hjust = 1.2, vjust = -0.2, size = 3.5) +
  labs(title = "Table 1.3: Top 10 Beer Styles by Number of Reviews",
       x = "Beer Style", y = "Number of Reviews") +
  coord_flip() +
  theme_minimal()

# Calculate the average beer ABV for the top 10 beer styles
top_beer_styles_abv <- beer_reviews %>%
  filter(beer_style %in% beer_style_counts$beer_style) %>%
  group_by(beer_style) %>%
  summarise(avg_abv = mean(beer_abv, na.rm = TRUE)) %>%
  arrange(desc(avg_abv))

# Create a table with the kable and kable_styling functions
abv_table <- top_beer_styles_abv %>%
  kable(
    col.names = c("Beer Style", "Average ABV (%)"),
    digits = 2,
    caption = "Table 1.4: Average Beer ABV of Top 10 Beer Styles by Number of Reviews"
  ) %>%
  kable_styling(bootstrap_options = "striped", full_width = FALSE)

# Display the table
abv_table

# Prepare the design matrix and response vector
X <- cbind(1, beer_reviews$beer_abv_imputed)
y <- beer_reviews$review_overall

# Question 1: Is there a relationship between beer ABV and overall review ratings?

# Perform linear regression using lm.fit()
lm1_fit <- lm.fit(X, y)

# Display the coefficients
summary(lm1_fit)
lm1_fit$coefficients

# Get the coefficients from lm1_fit
intercept <- lm1_fit$coefficients[1]
slope <- lm1_fit$coefficients[2]

# Create a custom linear regression line using the coefficients
lm_line <- data.frame(beer_abv_imputed = range(beer_reviews$beer_abv_imputed))
lm_line$review_overall <- intercept + slope * lm_line$beer_abv_imputed

# Plot the scatterplot with the custom linear regression line
lm1_plot = ggplot(beer_reviews, aes(x = beer_abv_imputed, y = review_overall)) +
  geom_point(alpha = 0.1) +
  geom_line(data = lm_line, aes(x = beer_abv_imputed, y = review_overall), color = "red") +
  labs(title = "Relationship between Beer ABV and Overall Review Ratings",
       x = "Beer ABV (%)", y = "Overall Review Rating") +
  theme_minimal()

# Question 2: Does beer aroma rating influence the overall review rating?

# Perform linear regression
lm2_fit <- lm.fit(cbind(1, beer_reviews$review_aroma), beer_reviews$review_overall)

# Display the summary
summary(lm2_fit)
lm2_fit$coefficients

# Create a custom linear regression line using the coefficients
intercept2 <- lm2_fit$coefficients[1]
slope2 <- lm2_fit$coefficients[2]
lm_line2 <- data.frame(review_aroma = range(beer_reviews$review_aroma))
lm_line2$review_overall <- intercept2 + slope2 * lm_line2$review_aroma

# Plot the scatterplot with the custom linear regression line
lm2_plot = ggplot(beer_reviews, aes(x = review_aroma, y = review_overall)) +
  geom_point(alpha = 0.1) +
  geom_line(data = lm_line2, aes(x = review_aroma, y = review_overall), color = "blue") +
  labs(title = "Relationship between Beer Aroma Rating and Overall Review Rating",
       x = "Beer Aroma Rating", y = "Overall Review Rating") +
  theme_minimal()

lm2_plot

# Question 3: Do beers with a higher appearance rating also have a higher taste rating?

# Perform linear regression
lm3_fit <- lm.fit(cbind(1, beer_reviews$review_appearance), beer_reviews$review_taste)

# Display the summary
summary(lm3_fit)
lm3_fit$coefficients

# Create a custom linear regression line using the coefficients
intercept3 <- lm3_fit$coefficients[1]
slope3 <- lm3_fit$coefficients[2]
lm_line3 <- data.frame(review_appearance = range(beer_reviews$review_appearance))
lm_line3$review_taste <- intercept3 + slope3 * lm_line3$review_appearance

# Plot the scatterplot with the custom linear regression line
lm3_plot = ggplot(beer_reviews, aes(x = review_appearance, y = review_taste)) +
  geom_point(alpha = 0.1) +
  geom_line(data = lm_line3, aes(x = review_appearance, y = review_taste), color = "green") +
  labs(title = "Relationship between Beer Appearance Rating and Taste Rating",
       x = "Beer Appearance Rating", y = "Taste Rating") +
  theme_minimal()

lm3_plot

# Perform linear regression and obtain summary for Question 1
lm1 <- lm(review_overall ~ beer_abv_imputed, data = beer_reviews)
lm1_summary <- summary(lm1)
lm1_summary

# Perform linear regression and obtain summary for Question 2
lm2 <- lm(review_overall ~ review_aroma, data = beer_reviews)
lm2_summary <- summary(lm2)
lm2_summary

# Perform linear regression and obtain summary for Question 3
lm3 <- lm(review_taste ~ review_appearance, data = beer_reviews)
lm3_summary <- summary(lm3)
lm3_summary

# Create the scatterplot with custom aesthetics
fancy_scatterplot <- ggplot(beer_reviews, aes(x = beer_abv_imputed, y = review_overall)) +
  geom_point(alpha = 0.4, color = "steelblue", size = 0.8) +
  theme_minimal() +
  labs(title = "Fancy Scatterplot: Beer ABV vs. Overall Review Ratings",
       x = "Beer ABV (%)", y = "Overall Review Rating") +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

# Display the fancy scatterplot
fancy_scatterplot











