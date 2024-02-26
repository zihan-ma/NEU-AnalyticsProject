# Week 1
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

# Data imported from "us-counties-2023.csv"
library(readr)
us_counties_2023 <- read_csv("DataSets/us-counties-2023.csv")

# remove two specific column                                                                                                               sheet = "Data", range = "B5:F40")
COVID19US_State = us_counties_2023[,-c(1, 2, 4)]

# use aggregate() to sum the scores by name
COVID19US_State_sum = aggregate(cbind(cases, deaths) ~ state, data = COVID19US_State, FUN = sum)

# reshape the data frame into long format
COVID19US_State_sum_long = gather(COVID19US_State_sum, key = "variable", value = "value", -state)

# create the plot with bars cut into two parts
barplot1 = ggplot(COVID19US_State_sum_long, aes(x = state, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "identity", width = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  coord_flip() +
  theme_classic()

# add a new column to the data frame 
deathrate = c(COVID19US_State_sum$deaths/COVID19US_State_sum$cases*100)

# add the new column to the data frame using cbind()
COVID19US_State_sum_deathrate <- cbind(COVID19US_State_sum, deathrate)

barplot2 = ggplot(COVID19US_State_sum_deathrate, aes(x = state, y = deathrate, fill = deathrate)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red") +
  xlab("State") +
  ylab("Death Rate") +
  ggtitle("Death Rate by State")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(limits = c(0, 1.5))+
  coord_flip()

# create a Word document
m1docfile <- read_docx("Module1R_ZihanMa_02.26.2023.docx")

# add the plot to the Word document
print(barplot1, target = fp_paste("barplot1.png"), bg = "transparent")
m1docfile <- body_add_img(m1docfile, src = "barplot1.png", width = 5, height = 4)

print(barplot2, target = fp_paste("barplot2.png"), bg = "transparent")
m1docfile <- body_add_img(m1docfile, src = "barplot2.png", width = 5, height = 4)

# save the Word document
print(m1docfile, target = "Module1R_ZihanMa_02.26.2023.docx")








