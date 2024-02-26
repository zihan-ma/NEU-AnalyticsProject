# Week 3
# R Script.R
# Student Name: Zihan Ma
# Class NameALY6010: Probability Theory and Introductory Statistics
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

# Data imported from "us-counties-2023.csv"
library(readr)
median_income_by_state = read_csv("DataSets/median_income_by_state_2010_20230307.csv")

median_income_by_state$southern <- ifelse(median_income_by_state$State %in% 
                                            c("Delaware", "Florida", "Georgia",
                                              "Maryland", "North Carolina", 
                                              "South Carolina", "Virginia",
                                              "West Virginia", "Alabama",
                                              "Kentucky", "Mississippi", 
                                              "Tennessee", "Arkansas", 
                                              "Louisiana", "Oklahoma", 
                                              "Texas"), "South", "Other")

southern_data <- median_income_by_state %>%
  group_by(southern) %>%
  summarize(median_income = median(`2010`)) %>%
  filter(southern == "South")

median_income_south <- median_income_by_state[median_income_by_state$southern == "South", ]



# Create bar plot
ggplot(median_income_south, aes(x = State, y = `2010`, fill = southern)) +
  geom_bar(stat = "identity", color = "black") +
  scale_fill_manual(values = "#FFC107", labels = c("Southern States"), name = "Region") +
  labs(title = "Table 1: Median Income by Southern State", x = "State", y = "Median Income ($)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  geom_hline(yintercept = 55520, linetype = "dashed", color = "brown") +
  annotate("text", x = 10, y = 56500, label = "Median Income Across US", color = "brown", fontface = "bold")

# Running one-sample t-test
t.test(median_income_by_state[median_income_by_state$southern == "South", ]$`2010`, 
       mu = median_income_by_state[median_income_by_state$State == "United States",
                                   ]$`2010`, alternative = "two.sided")





