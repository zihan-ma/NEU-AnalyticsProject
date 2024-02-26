
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



library(readr)
Cambridge_Property_Database_FY2016_FY2021 <- read_csv("DataSets/Cambridge_Property_Database_FY2016-FY2021.csv")
View(Cambridge_Property_Database_FY2016_FY2021)



parking_gar_data <- Cambridge_Property_Database_FY2016_FY2021 %>%
  filter(PropertyClass == 'PARKING-GAR', YearOfAssessment %in% 2019:2021)

yearly_avg <- parking_gar_data %>%
  group_by(YearOfAssessment) %>%
  summarise(average_building_value = mean(AssessedValue, na.rm = TRUE), .groups = "drop")

print(yearly_avg)

ggplot(yearly_avg, aes(x = YearOfAssessment, y = average_building_value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(x = "Year", y = "Average Building Value", 
       title = "Average Building Value for PARKING-GAR (2019-2021)") +
  scale_x_continuous(breaks = yearly_avg$YearOfAssessment) 

