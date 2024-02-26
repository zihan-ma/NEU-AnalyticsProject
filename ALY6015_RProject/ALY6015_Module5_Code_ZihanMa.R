# ALY6015_Module5_Code_ZihanMa.r
# 06.25.2023
# Week 5
# Student Name: Zihan Ma
# Class Name: ALY6015 80472 
#             Intermediate Analytics SEC 04 Spring 2023 CPS [BOS-B-HY]
# Instructor:  Prof. Valeriy Shevchenko

# No extra library and data set used in this project


# --------------------------Task_1----------------------------
# Game Attendance

# Insert the attendance data
data <- c(6210, 3150, 2700, 3012, 4875, 3540, 6127, 2581, 2642, 2573, 2792, 2800, 2500, 3700, 6030, 5437, 2758, 3490, 2851, 2720)

# Perform a Wilcoxon signed rank test
wilcox_test <- wilcox.test(data, mu = 3000)

# Print the results
print(wilcox_test)

# --------------------------Task_2----------------------------
# Lottery Ticket Sales

# Define the number of "successes" and the total number of trials
successes <- 15
n <- 40

# Perform the binomial test
binom_test <- binom.test(successes, n, p = 0.5, alternative = "less")

# Print the results
print(binom_test)

# --------------------------Task_3----------------------------
# Lengths of Prison Sentences

# Insert the sentence length data
male_sentence_length <- c(8, 12, 6, 14, 22, 27, 32, 24, 26, 19, 15, 13)
female_sentence_length <- c(7, 5, 2, 3, 21, 26, 30, 9, 4, 17, 23, 12, 11, 16)

# Perform the Wilcoxon rank sum test
wilcox_test <- wilcox.test(male_sentence_length, female_sentence_length)

# Print the results
print(wilcox_test)

# --------------------------Task_4----------------------------
# Winning Baseball Games

# Input the data
NL <- c(89, 96, 88, 101, 90, 91, 92, 96, 108, 100, 95)
AL <- c(108, 86, 91, 97, 100, 102, 95, 104, 95, 89, 88, 101)

# Conduct the Kruskal-Wallis test
kruskal_test <- kruskal.test(list(NL, AL))

# Print the results
print(kruskal_test)

# --------------------------Task_5----------------------------
# Wilcoxon Signed-Rank Test

# no coding for this part

# --------------------------Task_6----------------------------
# Mathematics Literacy Scores

# Input the data
Western_Hemisphere <- c(527, 406, 474, 381, 411)
Europe <- c(520, 510, 513, 548, 496)
Eastern_Asia <- c(523, 547, 547, 391, 549)

# Conduct the Kruskal-Wallis test
kruskal_test <- kruskal.test(list(Western_Hemisphere, Europe, Eastern_Asia))

# Print the results
print(kruskal_test)

# --------------------------Task_7----------------------------
# Subway and Commuter Rail Passengers

# Insert the ridership data
subway_ridership <- c(845, 494, 425, 313, 108, 41)
rail_ridership <- c(39, 291, 142, 103, 33, 38)

# Compute the Spearman rank correlation coefficient
cor_test <- cor.test(subway_ridership, rail_ridership, method = "spearman")

# Print the results
print(cor_test)


# --------------------------Task_8----------------------------
# Prizes in Caramel Corn Boxes

set.seed(114514) # Ensures the result is reproducible

simulate_experiment <- function() {
  prizes <- 1:4
  collected_prizes <- integer(0)
  boxes_count <- 0
  while(length(unique(collected_prizes)) < 4) {
    new_prize <- sample(prizes, 1)
    collected_prizes <- c(collected_prizes, new_prize)
    boxes_count <- boxes_count + 1
  }
  return(boxes_count)
}

# Repeat the experiment 40 times
results <- replicate(40, simulate_experiment())

# Find the average number of boxes
average_boxes <- mean(results)

print(average_boxes)

# --------------------------Task_9----------------------------
# Lottery Winner

set.seed(114514) # Ensures the result is reproducible

simulate_experiment <- function() {
  letters <- c("b", "i", "g")
  probabilities <- c(0.6, 0.3, 0.1)
  collected_letters <- character(0)
  tickets_count <- 0
  while(length(unique(collected_letters)) < 3) {
    new_letter <- sample(letters, 1, prob = probabilities)
    collected_letters <- c(collected_letters, new_letter)
    tickets_count <- tickets_count + 1
  }
  return(tickets_count)
}

# Repeat the experiment 40 times
results <- replicate(40, simulate_experiment())

# Find the average number of tickets
average_tickets <- mean(results)

print(average_tickets)


