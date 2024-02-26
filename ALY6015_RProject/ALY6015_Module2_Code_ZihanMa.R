# ALY6015_Module1_Code_ZihanMa.r
# 06.5.2023
# Week 2
# Student Name: Zihan Ma
# Class Name: ALY6015 80472 
#             Intermediate Analytics SEC 04 Spring 2023 CPS [BOS-B-HY]
# Instructor:  Prof. Valeriy Shevchenko



# Un-comment to install package as needed
# install.packages("tidyverse")


# Libraries used in this script
library(tidyverse)    # comprehensive data science toolkit


# The libraries actually used in this script.
# library(dplyr)    # for data manipulation
# library(readr)    # for inputing data 
# library(ggplot2)  # for data visualization

# --------------------------q1----------------------------

# Input observed and expected frequencies
observed <- c(12, 8, 24, 6)    # (12/50, 8/50, 24/50, 6/50)
expected <- c(0.2, 0.28, 0.36, 0.16)  # Total sample size is 50

# Run Chi-Square test
result <- chisq.test(observed, p=expected)

# Find critical value
alpha <- 0.10
critical_value <- qchisq(1 - alpha, df = length(observed) - 1)
print(critical_value)

# Compute the test value
test_value <- result$statistic
print(test_value)

# Make the decision
if (test_value > critical_value) {
  print("Reject the null hypothesis")
} else {
  print("Do not reject the null hypothesis")
}

# Summarize the results
p_value <- result$p.value
print(paste("The test statistic is", test_value, 
            "and the p-value is", p_value))

# --------------------------q2----------------------------

# Input observed and expected frequencies
observed <- c(125, 10, 25, 40)    # 200 - 125 (on time) - 10 (NAS delay) - 40 (weather delay) = 25 (arriving late)
expected <- c(0.708, 0.082, 0.09, 0.12)  # Expected proportions from Bureau of Transportation Statistics

# Run Chi-Square test
result <- chisq.test(observed, p=expected)

# Find critical value
alpha <- 0.05
critical_value <- qchisq(1 - alpha, df = length(observed) - 1)
print(critical_value)

# Compute the test value
test_value <- result$statistic
print(test_value)

# Make the decision
if (test_value > critical_value) {
  print("Reject the null hypothesis")
} else {
  print("Do not reject the null hypothesis")
}

# Summarize the results
p_value <- result$p.value
print(paste("The test statistic is", test_value, 
            "and the p-value is", p_value))

# --------------------------q3----------------------------

# Input the observed data
observed <- matrix(c(724, 370, 335, 292, 174, 152, 107, 140), nrow = 2, byrow = TRUE)

# Assign column and row names
colnames(observed) <- c("Caucasian", "Hispanic", "African American", "Other")
rownames(observed) <- c("2013", "2014")

# Run Chi-Square test of independence
result <- chisq.test(observed)

# Find critical value
alpha <- 0.05
df <- (nrow(observed) - 1) * (ncol(observed) - 1) # Degrees of freedom
critical_value <- qchisq(1 - alpha, df)
print(critical_value)

# Compute the test value
test_value <- result$statistic
print(test_value)

# Make the decision
if (test_value > critical_value) {
  print("Reject the null hypothesis")
} else {
  print("Do not reject the null hypothesis")
}

# Summarize the results
p_value <- result$p.value
print(paste("The test statistic is", test_value, 
            "and the p-value is", p_value))

# --------------------------q4----------------------------

# Input the observed data
observed <- matrix(c(10791, 62491, 7816, 42750, 932, 9525, 11819, 54344), nrow = 4, byrow = TRUE)

# Assign column and row names
colnames(observed) <- c("Officers", "Enlisted")
rownames(observed) <- c("Army", "Navy", "Marine Corps", "Air Force")

# Run Chi-Square test of independence
result <- chisq.test(observed)

# Find critical value
alpha <- 0.05
df <- (nrow(observed) - 1) * (ncol(observed) - 1) # Correct degrees of freedom
critical_value <- qchisq(1 - alpha, df)
print(critical_value)

# Compute the test value
test_value <- result$statistic
print(test_value)

# Make the decision
if (test_value > critical_value) {
  print("Reject the null hypothesis")
} else {
  print("Do not reject the null hypothesis")
}

# Summarize the results
p_value <- result$p.value
print(paste("The test statistic is", test_value,
            "and the p-value is", p_value))

# --------------------------q5----------------------------

# Set the significance level
alpha <- 0.05

# Input the data
condiments <- data.frame('sodium' = c(270, 130, 230, 180, 80, 70, 200), 'food' = rep('condiments', 7), stringsAsFactors = FALSE)
cereals <- data.frame('sodium' = c(260, 220, 290, 290, 200, 320, 140), 'food' = rep('cererals', 7), stringsAsFactors = FALSE)
desserts <- data.frame('sodium' = c(100, 180, 250, 250, 300, 360, 300, 160), 'food' = rep('desserts', 8), stringsAsFactors = FALSE)

# Combine the data.frame into one
sodium <- rbind(condiments, cereals, desserts)
sodium$food <- as.factor(sodium$food)

# Run the ANOVA test
anova <- aov(sodium ~ food, data = sodium)

# View the model summary
summary(anova)

# Save the summary to an object
a.summary <- summary(anova)

# DOF
# k-1: between group variance - numerator
df.numerator <- a.summary[[1]][1, "Df"]
df.numerator

# N-k: within group variance - denominator
df.denominator <- a.summary[[1]][2, "Df"]
df.denominator

# Extract the F test Value from the summary
F.value <- a.summary[[1]][[1, "F value"]]
F.value

# Extract the F test Value from the summary
p.value <- a.summary[[1]][[1, "Pr(>F)"]]
p.value

# Make the decision
if (p.value > alpha) {
  decision <- "Do not reject the null hypothesis"
} else {
  decision <- "Reject the null hypothesis"
}
print(decision)

# See difference
TukeyHSD(anova)

# --------------------------q6----------------------------

# Set the significance level
alpha <- 0.01

# Input the data
cereal <- data.frame('sales' = c(578, 320, 264, 249, 237), 'food' = rep('cereal', 5), stringsAsFactors = FALSE)
chocolate_candy <- data.frame('sales' = c(311, 106, 109, 125, 173), 'food' = rep('chocolate_candy', 5), stringsAsFactors = FALSE)
coffee <- data.frame('sales' = c(261, 185, 302, 689), 'food' = rep('coffee', 4), stringsAsFactors = FALSE)

# Combine the data.frame into one
sales <- rbind(cereal, chocolate_candy, coffee)
sales$food <- as.factor(sales$food)


# Perform one-way ANOVA
result <- aov(sales ~ food, data = sales)

# View the model summary
summary(result)

# Save the summary to an object
a_summary <- summary(result)

# Degrees of freedom
df_numerator <- a_summary[[1]][1, "Df"]
df_denominator <- a_summary[[1]][2, "Df"]

# Extract the F test value from the summary
f_value <- a_summary[[1]][[1, "F value"]]

# Extract the p-value from the summary
p_value <- a_summary[[1]][[1, "Pr(>F)"]]

# Make the decision
if (p_value < alpha) {
  decision <- "Reject the null hypothesis"
} else {
  decision <- "Do not reject the null hypothesis"
}

# Print the results
print("ANOVA Summary:")
print(a_summary)
cat("\n")

print("Degrees of Freedom:")
print(paste("Numerator (k - 1):", df_numerator))
print(paste("Denominator (N - k):", df_denominator))
cat("\n")

print("F test value:")
print(f_value)
cat("\n")

print("P-value:")
print(p_value)
cat("\n")

print("Decision:")
print(decision)
cat("\n")

# Perform the post hoc Tukey's test
tukey_result <- TukeyHSD(result)
print("Tukey's Test Results:")
print(tukey_result)

# --------------------------q7----------------------------

# Set the significance level
alpha <- 0.05

# Input the data
eastern_third <- data.frame('expenditures' = c(4946, 5953, 6202, 7243), 'dollars' = rep('eastern_third', 4), stringsAsFactors = FALSE)
middle_third <- data.frame('expenditures' = c(6149, 7451, 6000, 6479), 'dollars' = rep('middle_third', 4), stringsAsFactors = FALSE)
western_third <- data.frame('expenditures' = c(5282, 8605, 6528, 6911, 6113), 'dollars' = rep('western_third', 5), stringsAsFactors = FALSE)

# Combine the data.frame into one
expenditures <- rbind(eastern_third, middle_third, western_third)
expenditures$dollars <- as.factor(expenditures$dollars)


# Perform one-way ANOVA
result <- aov(expenditures ~ dollars, data = expenditures)

# View the model summary
summary(result)

# Save the summary to an object
a_summary <- summary(result)

# Degrees of freedom
df_numerator <- a_summary[[1]][1, "Df"]
df_denominator <- a_summary[[1]][2, "Df"]

# Extract the F test value from the summary
f_value <- a_summary[[1]][[1, "F value"]]

# Extract the p-value from the summary
p_value <- a_summary[[1]][[1, "Pr(>F)"]]

# Make the decision
if (p_value < alpha) {
  decision <- "Reject the null hypothesis"
} else {
  decision <- "Do not reject the null hypothesis"
}

# Print the results
print("ANOVA Summary:")
print(a_summary)
cat("\n")

print("Degrees of Freedom:")
print(paste("Numerator (k - 1):", df_numerator))
print(paste("Denominator (N - k):", df_denominator))
cat("\n")

print("F test value:")
print(f_value)
cat("\n")

print("P-value:")
print(p_value)
cat("\n")

print("Decision:")
print(decision)
cat("\n")

# --------------------------q8----------------------------

# Set the significance level
alpha <- 0.05

# Input the data
light1_foodA <- data.frame('growth' = c(9.2, 9.4, 8.9), 'light' = rep('light1', 3), 'food' = rep('foodA', 3), stringsAsFactors = FALSE)
light2_foodA <- data.frame('growth' = c(8.5, 9.2, 8.9), 'light' = rep('light2', 3), 'food' = rep('foodA', 3), stringsAsFactors = FALSE)
light1_foodB <- data.frame('growth' = c(7.1, 7.2, 8.5), 'light' = rep('light1', 3), 'food' = rep('foodB', 3), stringsAsFactors = FALSE)
light2_foodB <- data.frame('growth' = c(5.5, 5.8, 7.6), 'light' = rep('light2', 3), 'food' = rep('foodB', 3), stringsAsFactors = FALSE)

# Combine the data.frame into one
data <- rbind(light1_foodA, light2_foodA, light1_foodB, light2_foodB)
data$light <- as.factor(data$light)
data$food <- as.factor(data$food)

# Run the Two-way ANOVA test
anova <- aov(growth ~ light*food, data = data)

# View the model summary
summary(anova)

# Save the summary to an object
a.summary <- summary(anova)

# Extract the DOF
df_light <- a.summary[[1]][1, "Df"]
df_food <- a.summary[[1]][2, "Df"]
df_light_food <- a.summary[[1]][3, "Df"]
df_residuals <- a.summary[[1]][4, "Df"]

# Extract the F test Value from the summary
F_value_light <- a.summary[[1]][[1, "F value"]]
F_value_food <- a.summary[[1]][[2, "F value"]]
F_value_light_food <- a.summary[[1]][[3, "F value"]]

# Extract the p-values from the summary
p_value_light <- a.summary[[1]][[1, "Pr(>F)"]]
p_value_food <- a.summary[[1]][[2, "Pr(>F)"]]
p_value_light_food <- a.summary[[1]][[3, "Pr(>F)"]]

# Make the decisions
if (p_value_light > alpha) {
  decision_light <- "Do not reject the null hypothesis for light"
} else {
  decision_light <- "Reject the null hypothesis for light"
}

if (p_value_food > alpha) {
  decision_food <- "Do not reject the null hypothesis for food"
} else {
  decision_food <- "Reject the null hypothesis for food"
}

if (p_value_light_food > alpha) {
  decision_light_food <- "Do not reject the null hypothesis for interaction"
} else {
  decision_light_food <- "Reject the null hypothesis for interaction"
}

# Print the decisions
print(decision_light)
print(decision_food)
print(decision_light_food)

# For any significant factors, conduct post-hoc analysis
if (p_value_light <= alpha | p_value_food <= alpha | p_value_light_food <= alpha) {
  print(TukeyHSD(anova))
}

# --------------------------q9----------------------------

# Import the dataset
bb <- read_csv(file.choose()) 

# Perform EDA 
summary(bb)
str(bb)
head(bb)

# Plot the distribution of wins
ggplot(bb, aes(x=W)) +
  geom_histogram(binwidth=5, fill="blue", color="white") +
  labs(title="Distribution of Wins", x="Number of Wins", y="Count")

# Extract decade from year 
bb$Decade <- bb$Year - (bb$Year %% 10)

# Create a wins table by summing the wins by decade
wins <- bb %>% group_by(Decade) %>% summarise(wins = sum(W)) %>% as_tibble()

# Display the wins table
print(wins)

# Perform Chi-Square Goodness-of-Fit test
chisq_test <- chisq.test(wins$wins)

# Display the test results
print(chisq_test)

# State the hypotheses and identify the claim
# H0: The number of wins is equally distributed across decades.
# H1: The number of wins is not equally distributed across decades.

# Find the critical value
# From the Chi-Square distribution table, for df=9 (10 decades - 1) and alpha=0.05, the critical value is approximately 16.919.

# Compute the test value
test_value <- chisq_test$statistic
print(test_value)

# Make the decision
if (chisq_test$p.value > 0.05) {
  decision <- "Do not reject the null hypothesis"
} else {
  decision <- "Reject the null hypothesis"
}

# Print the decision
print(decision)

# Provide conclusion
# If "Reject the null hypothesis" is printed, then the conclusion is: 
# There is a significant difference in the number of wins by decade. 
# If "Do not reject the null hypothesis" is printed, then the conclusion is: 
# There is no significant difference in the number of wins by decade.

# Compare the critical value with the test value and the p-value from R with the significance level
same_result <- (test_value > 16.919) == (chisq_test$p.value < 0.05)
print(same_result)

# --------------------------q10----------------------------

# Import the data
crop_data <- read_csv(file.choose())

# Convert the variables to factors
crop_data$density <- as.factor(crop_data$density)
crop_data$fertilizer <- as.factor(crop_data$fertilizer)
crop_data$block <- as.factor(crop_data$block)

# Display the structure of the dataset
str(crop_data)

# State the hypotheses
# Null Hypothesis (H0): Fertilizer and density have no impact on yield.
# Alternative Hypothesis (H1): Fertilizer and/or density have an impact on yield.

# Perform a Two-way ANOVA test
anova_results <- aov(yield ~ fertilizer * density, data = crop_data)
summary(anova_results)















