# Milestone2RScript_ZihanMa_03.19.2023
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


# Data imported from "LBOI_07.02_12_09_V1.xls"
library(readxl)
LBOI_2005 <- read_excel("DataSets/LBOI_07.02_12_09_V1.xls", 
                                  sheet = "Table 1", skip = 1)
library(readxl)
LBOI_2007 <- read_excel("DataSets/LBOI_07.02_12_09_V1.xls", 
                                  sheet = "Table 2", skip = 1)

# Keep columns "Area code", "Local Authority name", "Proportion" in LBOI_2005
LBOI_2005_sub = LBOI_2005[, c("Area code", "Local Authority name", "Proportion")]

# Keep columns "Area code", "Area Name", "Proportion" in LBOI_2007
LBOI_2007_sub = LBOI_2007[, c("Area code", "Area Name", "Proportion")]

# Merge data frames by id column
per_merged_LBOI = merge(LBOI_2005_sub, LBOI_2007_sub, by = "Area code")
with_eng_merged_LBOI = per_merged_LBOI[, -4]
merged_LBOI = with_eng_merged_LBOI[with_eng_merged_LBOI$`Area code` != "ENG", ]

# Hypothesis 1
Hypothesis_1 = t.test(merged_LBOI$Proportion.x, merged_LBOI$Proportion.y, paired = TRUE)
Hypothesis_1

# Hypothesis 2
target = 12
result_2005 = t.test(merged_LBOI$Proportion.x, mu = target)
result_2005

# Hypothesis 3
result_2007 <- t.test(merged_LBOI$Proportion.y, mu = target)
result_2007
