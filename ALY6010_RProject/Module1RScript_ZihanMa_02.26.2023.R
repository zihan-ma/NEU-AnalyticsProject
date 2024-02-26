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
barplot1

# add a new column to the data frame 
deathrate = c(COVID19US_State_sum$deaths/COVID19US_State_sum$cases*100)

# add the new column to the data frame using cbind()
COVID19US_State_sum_deathrate = cbind(COVID19US_State_sum, deathrate)


# # not working heatmap
# barplot2 = ggplot(COVID19US_State_sum_deathrate, aes(x = state, y = deathrate, fill = deathrate)) +
#   geom_bar(stat = "identity") +
#   scale_fill_gradient(low = "blue", high = "red") +
#   xlab("State") +
#   ylab("Death Rate") +
#   ggtitle("Death Rate by State")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   scale_y_continuous(limits = c(0, 1.5))+
#   coord_flip()
# barplot2

# # Not working picture output
# # create a Word document
# m1docfile <- read_docx("Module1R_ZihanMa_02.26.2023.docx")
# 
# # add the plot to the Word document
# print(barplot1, target = fp_paste("barplot1.png"), bg = "transparent")
# m1docfile <- body_add_img(m1docfile, src = "barplot1.png", width = 5, height = 4)
# 
# print(barplot2, target = fp_paste("barplot2.png"), bg = "transparent")
# m1docfile <- body_add_img(m1docfile, src = "barplot2.png", width = 5, height = 4)
# 
# # save the Word document
# print(m1docfile, target = "Module1R_ZihanMa_02.26.2023.docx")
# 


# add a new column to the data frame 
deathrate = c((us_counties_2023$deaths/us_counties_2023$cases)*100)

# add the new column to the data frame using cbind()
COVID19US_Counties_deathrate = cbind(us_counties_2023, deathrate)

# Create an ordinal variable by dividing num_var into equal-sized intervals
COVID19US_Counties_deathrate$ord_var_dr = as.numeric(cut_number(COVID19US_Counties_deathrate$deathrate,10))

# Create an ordinal variable by dividing num_var into equal-sized intervals
COVID19US_Counties_deathrate$ord_var_case = as.numeric(cut_number(COVID19US_Counties_deathrate$cases,10))


# Create a frequency table of the Alaska
Alaska = COVID19US_Counties_deathrate[COVID19US_Counties_deathrate$state == "Alaska",]
Alaska_table = table(Alaska$ord_var_dr) 

# Create a frequency table of the Massachusetts
Massachusetts = COVID19US_Counties_deathrate[COVID19US_Counties_deathrate$state == "Massachusetts",]
Massachusetts_table = table(Massachusetts$ord_var_dr) 

# Create a frequency table of the Florida
Florida = COVID19US_Counties_deathrate[COVID19US_Counties_deathrate$state == "Florida",]
Florida_table = table(Florida$ord_var_dr) 

kbl(Alaska_table, digits=2, row_label_position='c', align = "cc", col.names = c("Death Rate Level", "Frequency"), caption = "Table 1: Alaska") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
kbl(Massachusetts_table, digits=2, row_label_position='c', align = "cc", col.names = c("Death Rate Level", "Frequency"), caption = "Table 2: Massachusetts") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
kbl(Florida_table, digits=2, row_label_position='c', align = "cc", col.names = c("Death Rate Level", "Frequency"), caption = "Table 3: Florida") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


FiveStates = COVID19US_Counties_deathrate[COVID19US_Counties_deathrate$state %in% c("Alaska", "Massachusetts", "Florida", "Indiana", "Georgia"),]

# Create a cross tabulation
xtabs_table_dr = xtabs(~ state + ord_var_dr, data = FiveStates)
addmargins(xtabs_table_dr)%>%
kbl(digits=2, row_label_position='c', align = "cccccccccccc", caption = "Table 4.1: Five State Death Rate") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
round(100 * prop.table(xtabs_table_dr, 1), 2)%>%
  kbl(digits=2, row_label_position='c', align = "cccccccccccc", caption = "Table 4.2: Five State Death Rate(%)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))



xtabs_table_case = xtabs(~ state + ord_var_case, data = FiveStates)
addmargins(xtabs_table_case)%>%
  kbl(digits=2, row_label_position='c', align = "cccccccccccc", caption = "Table 5.1: Five State Cases") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
round(100 * prop.table(xtabs_table_case, 1), 2)%>%
  kbl(digits=2, row_label_position='c', align = "cccccccccccc", caption = "Table 5.2: Five State Cases(%)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))


xtabs_table_drcase = xtabs(~ ord_var_case + ord_var_dr, data = FiveStates)
addmargins(xtabs_table_drcase)%>%
  kbl(digits=2, row_label_position='c', align = "cccccccccccc", caption = "Table 6.1: Cases/Death Rate Relations") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))
round(100 * prop.table(xtabs_table_drcase, 1), 2)%>%
  kbl(digits=2, row_label_position='c', align = "cccccccccccc", caption = "Table 6.2: Cases/Death Rate Relations(%)") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))



