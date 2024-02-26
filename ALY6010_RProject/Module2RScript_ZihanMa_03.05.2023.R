# Week 2
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

# Data imported from "aoe_data.csv"
library(readr)
matching <- read_csv("DataSets/aoe_data.csv")

# drop some columns from data frame
matching = matching[, c(5, 8, 9, 10, 15)]

# select all Byzantines matches 
Byzantines_all = matching[matching$p1_civ == "Byzantines" | matching$p2_civ == "Byzantines",]

# remove rows both are Byzantines
Byzantines = Byzantines_all[!(Byzantines_all$p1_civ == "Byzantines" & Byzantines_all$p2_civ == "Byzantines"),]

#Find winner
Byzantines_win_0 = Byzantines[Byzantines$p1_civ == "Byzantines" & Byzantines$winner == "0",]
Byzantines_win_1 = Byzantines[Byzantines$winner == "1" & Byzantines$p2_civ == "Byzantines",]
Byzantines_win = rbind(Byzantines_win_0, Byzantines_win_1)

# add new column with same character value in every row
Byzantines_win_new = Byzantines_win %>% mutate(Win_Civ = "Byzantines")

# select all Teutons matches 
Teutons_all = matching[matching$p1_civ == "Teutons" | matching$p2_civ == "Teutons",]

# remove rows both are Teutons
Teutons = Teutons_all[!(Teutons_all$p1_civ == "Teutons" & Teutons_all$p2_civ == "Teutons"),]

#Find winner
Teutons_win_0 = Teutons[Teutons$p1_civ == "Teutons" | Teutons$winner == "0",]
Teutons_win_1 = Teutons[Teutons$winner == "1" & Teutons$p2_civ == "Teutons",]
Teutons_win = rbind(Teutons_win_0, Teutons_win_1)

# add new column with same character value in every row
Teutons_win_new = Teutons_win %>% mutate(Win_Civ = "Teutons")

# NOT WORKING
# # switch columns p1_civ and p2_civ if p1_civ is = Byzantines
# if (any(Teutons_win$winner > 0)) {
# #  new_cols_Teutons = c("duration", "elo", "p2_civ", "p1_civ", "winner")
#   Teutons_win_new = Teutons_win[, c("duration", "elo", "p2_civ", "p1_civ", "winner", setdiff(names(Teutons_win), c("duration", "elo", "p2_civ", "p1_civ", "winner")))]
# } else {
#   Teutons_win_new = Teutons_win
# }


# select all Britons matches 
Britons_all = matching[matching$p1_civ == "Britons" | matching$p2_civ == "Britons",]

# remove rows both are Britons
Britons = Britons_all[!(Britons_all$p1_civ == "Britons" & Britons_all$p2_civ == "Britons"),]

#Find winner
Britons_win_0 = Britons[Britons$p1_civ == "Britons" | Britons$winner == "0",]
Britons_win_1 = Britons[Britons$winner == "1" & Britons$p2_civ == "Britons",]
Britons_win = rbind(Britons_win_0, Britons_win_1)

# add new column with same character value in every row
Britons_win_new = Britons_win %>% mutate(Win_Civ = "Britons")

# summary for all sample
all_summary = summary(matching)
kbl(all_summary, digits=2, row_label_position='c', align = "ccccc", caption = "Table 1.1: Basic Attrubites of All Sample") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# summary for Byzantines
Byzantines_summary = summary(Byzantines)
kbl(Byzantines_summary, digits=2, row_label_position='c', align = "ccccc", caption = "Table 1.2: Basic Attrubites of Byzantines") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# summary for Teutons
Teutons_summary = summary(Teutons)
kbl(Teutons_summary, digits=2, row_label_position='c', align = "ccccc", caption = "Table 1.3: Basic Attrubites of Teutons") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# summary for Britons
Britons_summary = summary(Britons)
kbl(Britons_summary, digits=2, row_label_position='c', align = "ccccc", caption = "Table 1.4: Basic Attrubites of Britons") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))

# create scatter chart for Byzantines with duration and elo as variables
ggplot(Byzantines, aes(x = duration, y = elo, color = elo)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(x = "Duration (sec)", y = "Elo rating") + 
  ggtitle("Table 2.1: Scatter Chart of Duration vs. Elo for Byzantines") +
  theme_bw()

# create scatter chart for Teutons with duration and elo as variables
ggplot(Teutons, aes(x = duration, y = elo, color = elo)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(x = "Duration (sec)", y = "Elo rating") + 
  ggtitle("Table 2.2: Scatter Chart of Duration vs. Elo for Teutons") +
  theme_bw()

# create scatter chart for Britons with duration and elo as variables
ggplot(Britons, aes(x = duration, y = elo, color = elo)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE, color = "red") + 
  labs(x = "Duration (sec)", y = "Elo rating") + 
  ggtitle("Table 2.3: Scatter Chart of Duration vs. Elo for Britons") +
  theme_bw()

# create plot with multiple geom_point() layers
ggplot() + 
  geom_point(data = Byzantines, aes(x = duration, y = elo, color = "Byzantines")) + 
  geom_point(data = Teutons, aes(x = duration, y = elo, color = "Teutons")) + 
  geom_point(data = Britons, aes(x = duration, y = elo, color = "Britons")) + 
  scale_color_manual(values = c("Byzantines" = "red", "Teutons" = "green", "Britons" = "blue")) + 
  labs(x = "Duration (sec)", y = "Elo rating", color = "Data Frame") + 
  geom_point(alpha = 0.1) + 
  ggtitle("Table 2.4: Overlapping Scatter Charts of Three Civilaztions") +
  theme_bw()

# combine data frames and add group variable
Civilaztions <- rbind(Byzantines_win_new, Teutons_win_new, Britons_win_new)
Civilaztions$group = rep(c("Byzantines", "Teutons", "Britons"), length.out = 38351)

# ma = if (Civilaztions$winner == 1) {Civilaztions$p2_civ} else {Civilaztions$p1_civ}
# create jitter chart
ggplot(Civilaztions, aes(x = Win_Civ, y = elo, color = Win_Civ)) + 
  geom_jitter() + 
  labs(x = "Civilaztions", y = "elo", color = "Civilaztions") + 
  ggtitle("Table 3.1: Jitter Chart for Three Civilaztions")

matching_win = matching[matching$winner == "0",]
# create jitter chart
ggplot(matching_win, aes(x = p1_civ, y = elo, color = p1_civ)) + 
  geom_jitter() + 
  labs(x = "Civilaztions", y = "elo", color = "Civilaztions") + 
  ggtitle("Table 3.2: Jitter Chart for All Civilaztions")+ 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Create the box plot
ggplot(Civilaztions, aes(x = Win_Civ, y = elo, fill = Win_Civ)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
#  geom_jitter(aes(x = Win_Civ, y = elo, color = Win_Civ), alpha = 0.8) +
  scale_color_manual(values = c("blue", "red", "green")) +
  scale_fill_manual(values = c("blue", "red", "green")) +
  labs(x = "Civilization", y = "elo") +
  ggtitle("Table 4.1: Box Plot for Three Civilaztions") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())

# Create the box plot
ggplot(matching_win, aes(x = p1_civ, y = elo, fill = p1_civ)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.2) +
  #  geom_jitter(aes(x = p1_civ, y = elo, color = p1_civ), alpha = 0.8) +
  labs(x = "Civilization", y = "elo") +
  geom_hline(yintercept = median(matching_win$elo[matching_win$p1_civ == "Britons"]), color = "red", size = 1, alpha = 0.5) +
  ggtitle("Table 4.2: Box Plot for All Civilaztions") +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        axis.text = element_text(size = 14),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 14),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank())









