# Load the necessary libraries
library(tidyverse)


library(readr)
video_games <- read_csv("DataSets/archive/Video Games Dataset.csv")
View(video_games)

summary(video_games)

str(video_games)

# Perform ANOVA test
result <- aov(Global_Sales ~ Platform, data = video_games)

# Check the ANOVA table
summary(result)

# Post-hoc analysis for pairwise comparisons (optional)
posthoc <- TukeyHSD(result)
print(posthoc)

# Plot the means and confidence intervals (optional)
plot(posthoc)

# Interpret the results and draw conclusions based on the ANOVA and post-hoc analyses






