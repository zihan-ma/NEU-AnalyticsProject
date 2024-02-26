# Libraries used in this document
library(readxl)
library(dplyr)
library(tidyverse)
library(knitr)
library(magrittr)

# wine.xlsx is used in this library
wine2023 <- read_excel("DataSets/wine.xlsx")
View(wine)

dplyr::glimpse(wine2023)

#Descriptive statistics 

wine2023$Phenols

#working with categorical variable

WineType = wine2023$WineType

as.data.frame(table(WineType))

WineType
  table()%%
  barplot()%%
  
#working with categorical variable
  
WineType = wine2023$WineType  











