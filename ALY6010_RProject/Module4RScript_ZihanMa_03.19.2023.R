# Module4RScript_ZihanMa_03.19.2023
# Week 2
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


# Data imported from "world_happiness_report_20230314.csv"
library(readr)
world_happiness = read_csv("DataSets/world_happiness_report_20230314.csv")

# Define rich and poor groups based on Logged GDP per capita
rich_countries = world_happiness %>% 
  filter(`Logged GDP per capita` >= median(`Logged GDP per capita`))
poor_countries = world_happiness %>% 
  filter(`Logged GDP per capita` < median(`Logged GDP per capita`))

# Perform the t-test
ttest_result = t.test(rich_countries$`Ladder score`, 
                      poor_countries$`Ladder score`, 
                      var.equal = FALSE)
ttest_result

# Boxplot of happiness scores by rich and poor countries
ggplot(world_happiness, 
       aes(x = `Logged GDP per capita` >= 
             median(world_happiness$`Logged GDP per capita`), 
           y = `Ladder score`, fill = factor(`Logged GDP per capita` >= 
                                               median(world_happiness$`Logged GDP per capita`)))) +
  geom_boxplot() +
  labs(title = "Table 1.1: Happiness Scores of Rich and Poor Countries", x = "Countries", 
       y = "Happiness Score") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "lightgreen"), 
                    labels = c("Poor Countries", "Rich Countries"),
                    name = "Countries") +
  scale_fill_discrete(name = "Legend", labels=c("Poor Countries", "Rich Countries"))

# Histograms of happiness scores for rich and poor countries
ggplot(world_happiness, aes(x = `Ladder score`, 
                            fill = factor(`Logged GDP per capita` >= 
                                            median(world_happiness$`Logged GDP per capita`)))) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 20) +
  labs(title = "Table 1.2: Overlapping Histogram of Happiness Scores", 
       x = "Happiness Score", y = "Frequency") +
  scale_fill_manual(values = c("lightblue", "lightgreen"), 
                    labels = c("Poor Countries", "Rich Countries")) +
  theme_minimal() +
  scale_fill_discrete(name = "Legend", labels=c("Poor Countries", "Rich Countries"))