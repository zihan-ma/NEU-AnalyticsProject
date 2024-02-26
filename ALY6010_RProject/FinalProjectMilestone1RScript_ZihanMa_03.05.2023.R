# Week 2 
# Final Project — Milestone 1
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
library(officer)

# Data imported from "Video_Games.csv"
library(readr)
Video_Games_Sales <- read_csv("DataSets/Video_Games.csv")
View(Video_Games_Sales)

# Sumit Kumar Shukla (2023). Video Games Sales [Dataset]. https://data.world/sumitrock/video-games-sales


summary(Video_Games)

# Remove rows with no NA values
Video_Games_Sales_Fullline <- Video_Games_Sales[complete.cases(Video_Games_Sales), ]
summary(Video_Games_Fullline)


kbl(summary(Video_Games_Sales), digits=2, row_label_position='c', align = "cccccccccccccccc", caption = "Table 1.1: Basic Attrubites of Video Games Sales") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

kbl(summary(Video_Games_Sales_Fullline), digits=2, row_label_position='c', align = "cccccccccccccccc", caption = "Table 1.2: Basic Attrubites of Video Games Sales with No Missing Line") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


# Create a scatterplot of North American sales vs. European sales 
ggplot(Video_Games_Sales_Fullline, aes(x = EU_Sales, y = NA_Sales)) + 
  geom_point(color = "#5A5A5A", size = 3) + 
  labs(x = "European Sales", y = "North American Sales", title = "Table 2.1: Relationship Between North American and European Video Game Sales") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        panel.grid.major = element_line(color = "#F0F0F0"))

# Create a bar chart of the number of video games released by year
Video_Games_Sales_Fullline %>% 
  ggplot(aes(x = Year_of_Release)) +
  geom_bar(fill = "#8DD3C7") +
  labs(x = "Year of Release", y = "Number of Games Released", title = "Table 2.2: Number of Video Games Released by Year") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        panel.grid.major = element_line(color = "#F0F0F0"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "#5A5A5A")) +
  coord_flip() +
  scale_fill_manual(values = brewer.pal(8, "Accent"))

# Create a bar chart of the top 10 video game publishers by number of games released 
Video_Games_Sales_Fullline %>% 
  group_by(Publisher) %>% 
  summarise(n = n()) %>% 
  top_n(10, n) %>% 
  ggplot(aes(x = fct_reorder(Publisher, n), y = n, fill = Publisher)) + 
  geom_col(color = "#5A5A5A") + 
  labs(x = "Publisher", y = "Number of Games Released", title = "Table 2.3: Top 10 Video Game Publishers by Number of Games Released") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        panel.grid.major = element_line(color = "#F0F0F0"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "#5A5A5A"),
        legend.position = "none") +
  coord_flip()

# Create histogram of global sales
ggplot(Video_Games_Sales_Fullline, aes(x = Global_Sales)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "dodgerblue") +
  labs(x = "Global Sales (in millions)", y = "Count", title = "Table 2.4: Histogram of Global Sales") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), 
        axis.text = element_text(size = 14), 
        axis.title = element_text(size = 16), 
        panel.grid.major = element_line(color = "#F0F0F0"),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(color = "#5A5A5A")) +
  scale_y_log10()