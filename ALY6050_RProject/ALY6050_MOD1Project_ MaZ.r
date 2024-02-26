# ALY6050_MOD1Project_ MaZ.r
# 05.30.2023
# Week 1
# Student Name: Zihan Ma
# Class Name: ALY6050 80478 
#             Intro to Enterprise Analytics SEC 09 Spring 2023 CPS
# Instructor:  Prof. Richard He

# Un-comment to install package as needed
# install.packages("tidyverse")

# Libraries used in this script
library(tidyverse)    # A collection of basic R packages such as ggplot2, tibble, tidyr, readr, dplyr, stringr, purr, and forcats.

# --------------------------BASE DATA----------------------------

# Winning rates

prob_celtics_win_home <- 0.62

prob_celtics_win_visiting <- (1-0.59)

prob_heat_win_home <- 0.59

prob_heat_win_visiting <- (1-0.62)

# Bets

bet_celtics_win <- 200

bet_celtics_lose <- -210

# --------------------------q1----------------------------

# I
# P(Celtics Win Series) = p(C) * p(C) + p(C) * p(H) * p(C) + p(H) * p(C) * p(C)
Celtics_Win_Series_q1i = 
  prob_celtics_win_home * prob_celtics_win_home + 
  prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home + 
  prob_heat_win_visiting * prob_celtics_win_home * prob_celtics_win_home
Celtics_Win_Series_q1i

# II

  # Calculate the probabilities for each outcome
  P1 <- (prob_celtics_win_home^2)  # Celtics win the series
  P2 <- ((1 - prob_celtics_win_home)^2)  # Celtics lose the series
  P3 <- (prob_celtics_win_home * (1 - prob_celtics_win_home) * prob_celtics_win_home)  # Series goes to the third game, Celtics win
  P4 <- ((1 - prob_celtics_win_home) * prob_celtics_win_home * (1 - prob_celtics_win_home))  # Series goes to the third game, Celtics lose
  P5 <- (prob_celtics_win_home^2)  # Celtics win the first two games, series ends
  P6 <- ((1 - prob_celtics_win_home)^2)  # Celtics lose the first two games, series ends
  
  # Calculate the net win values
  net_win_q1ii <- c(2 * bet_celtics_win, 2 * bet_celtics_lose, bet_celtics_win + bet_celtics_lose,
               bet_celtics_lose + bet_celtics_win, bet_celtics_win + bet_celtics_win, bet_celtics_lose + bet_celtics_lose)
  
  # Calculate the expected net win (E(X))
  E_X_q1ii <- sum(net_win_q1ii * c(P1, P2, P3, P4, P5, P6))
  E_X_q1ii
  
  # Calculate the standard deviation of X
  SD_X_q1ii <- sqrt(sum((net_win_q1ii - E_X_q1ii)^2 * c(P1, P2, P3, P4, P5, P6)))
  SD_X_q1ii

# III
  
  # set.seed(114514)  # Set a seed for reproducibility
  
  # Generate 5,000 random values for X
  Y <- sample(net_win_q1ii, size = 5000, replace = TRUE, prob = c(P1, P2, P3, P4, P5, P6))
  
  # Calculate the sample mean of Y
  mean_Y <- mean(Y)
  
  # Calculate the standard error of the mean
  se <- sd(Y) / sqrt(length(Y))
  
  # Calculate the 95% confidence interval
  lower <- mean_Y - 1.96 * se
  upper <- mean_Y + 1.96 * se
  
  # Check if E(X) from Part (ii) is within the confidence interval
  is_within_interval <- (lower <= E_X_q1ii) && (E_X_q1ii <= upper)
  is_within_interval 

# IV
  
  # Construct the frequency distribution for Y
  freq_table <- table(Y)
  
  # Perform the Chi-square goodness-of-fit test
  chi_square_test <- chisq.test(freq_table)
  chi_square_test
  

  # --------------------------q2----------------------------
  
  # I
  Celtics_Win_Series_q2i <- prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home
  Celtics_Win_Series_q2i
  
  # II
  P1 <- (prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home)  # Celtics win the series
  P2 <- ((1 - prob_celtics_win_home) * prob_heat_win_home * (1 - prob_celtics_win_home))  # Celtics lose the series
  P3 <- (prob_celtics_win_home^2)  # Series goes to the third game, Celtics win
  P4 <- ((1 - prob_celtics_win_home)^2)  # Series goes to the third game, Celtics lose
  P5 <- (prob_celtics_win_home * prob_heat_win_home * (1 - prob_celtics_win_home))  # Celtics win the first two games, series ends
  P6 <- ((1 - prob_celtics_win_home) * prob_heat_win_visiting * prob_celtics_win_home)  # Celtics lose the first two games, series ends
  
  net_win_q2ii <- c(2 * bet_celtics_win, 2 * bet_celtics_lose, bet_celtics_win + bet_celtics_lose,
                    bet_celtics_lose + bet_celtics_win, bet_celtics_win + bet_celtics_win, bet_celtics_lose + bet_celtics_lose)
  
  E_X_q2ii <- sum(net_win_q2ii * c(P1, P2, P3, P4, P5, P6))
  E_X_q2ii
  
  SD_X_q2ii <- sqrt(sum((net_win_q2ii - E_X_q2ii)^2 * c(P1, P2, P3, P4, P5, P6)))
  SD_X_q2ii
  
  # III
  set.seed(114514)  # Set a seed for reproducibility
  
  Y <- sample(net_win_q2ii, size = 5000, replace = TRUE, prob = c(P1, P2, P3, P4, P5, P6))
  
  mean_Y <- mean(Y)
  se <- sd(Y) / sqrt(length(Y))
  lower <- mean_Y - 1.96 * se
  upper <- mean_Y + 1.96 * se
  is_within_interval <- (lower <= E_X_q2ii) && (E_X_q2ii <= upper)
  is_within_interval
  
  # IV
  freq_table <- table(Y)
  chi_square_test <- chisq.test(freq_table)
  chi_square_test

# --------------------------q3----------------------------

  # I
  Celtics_Win_Series_q3i <- prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home * 
    prob_heat_win_home * prob_celtics_win_home * prob_heat_win_home +
    prob_heat_win_home * prob_celtics_win_home * prob_heat_win_visiting * 
    prob_celtics_win_home * prob_heat_win_home * prob_celtics_win_home
  Celtics_Win_Series_q3i
  
  # II
  P1 <- (prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home * 
           prob_heat_win_home * prob_celtics_win_home * prob_heat_win_home)  # Celtics win the series
  P2 <- (prob_heat_win_home * prob_celtics_win_home * prob_heat_win_visiting * 
           prob_celtics_win_home * prob_heat_win_home * prob_celtics_win_home)  # Celtics lose the series
  P3 <- (prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home * 
           prob_heat_win_home * prob_celtics_win_home)  # Series goes to the fifth game, Celtics win
  P4 <- (prob_heat_win_home * prob_celtics_win_home * prob_heat_win_visiting * 
           prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home)  # Series goes to the fifth game, Celtics lose
  P5 <- (prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home * 
           prob_heat_win_home * prob_celtics_win_home * prob_heat_win_home)  # Celtics win the first three games, series ends
  P6 <- (prob_heat_win_home * prob_celtics_win_home * prob_heat_win_visiting * 
           prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home)  # Celtics lose the first three games, series ends
  
  net_win_q3ii <- c(2 * bet_celtics_win, 2 * bet_celtics_lose, bet_celtics_win + bet_celtics_lose,
                    bet_celtics_lose + bet_celtics_win, bet_celtics_win + bet_celtics_win, bet_celtics_lose + bet_celtics_lose)
  
  E_X_q3ii <- sum(net_win_q3ii * c(P1, P2, P3, P4, P5, P6))
  E_X_q3ii
  
  SD_X_q3ii <- sqrt(sum((net_win_q3ii - E_X_q3ii)^2 * c(P1, P2, P3, P4, P5, P6)))
  SD_X_q3ii
  
  # III
  set.seed(114514)  # Set a seed for reproducibility
  
  Y <- sample(net_win_q3ii, size = 5000, replace = TRUE, prob = c(P1, P2, P3, P4, P5, P6))
  
  mean_Y <- mean(Y)
  se <- sd(Y) / sqrt(length(Y))
  lower <- mean_Y - 1.96 * se
  upper <- mean_Y + 1.96 * se
  is_within_interval <- (lower <= E_X_q3ii) && (E_X_q3ii <= upper)
  is_within_interval
  
  # IV
  freq_table <- table(Y)
  chi_square_test <- chisq.test(freq_table)
  chi_square_test

# --------------------------q4----------------------------

  # I
  Celtics_Win_Series_q4i <- prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home * 
    prob_heat_win_home * prob_celtics_win_home * prob_heat_win_home *
    prob_celtics_win_home +
    prob_heat_win_home * prob_celtics_win_home * prob_heat_win_visiting * 
    prob_celtics_win_home * prob_heat_win_home * prob_celtics_win_home *
    prob_heat_win_home
  Celtics_Win_Series_q4i
  
  # II
  P1 <- (prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home * 
           prob_heat_win_home * prob_celtics_win_home * prob_heat_win_home * prob_celtics_win_home)  # Celtics win the series
  P2 <- (prob_heat_win_home * prob_celtics_win_home * prob_heat_win_visiting * 
           prob_celtics_win_home * prob_heat_win_home * prob_celtics_win_home * prob_heat_win_visiting)  # Celtics lose the series
  P3 <- (prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home * 
           prob_heat_win_home * prob_celtics_win_home * prob_heat_win_home)  # Series goes to the seventh game, Celtics win
  P4 <- (prob_heat_win_home * prob_celtics_win_home * prob_heat_win_visiting * 
           prob_celtics_win_home * prob_heat_win_home * prob_celtics_win_home * prob_heat_win_visiting)  # Series goes to the seventh game, Celtics lose
  P5 <- (prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home * 
           prob_heat_win_home * prob_celtics_win_home * prob_heat_win_home * prob_celtics_win_home)  # Celtics win the first four games, series ends
  P6 <- (prob_heat_win_home * prob_celtics_win_home * prob_heat_win_visiting * 
           prob_celtics_win_home * prob_heat_win_visiting * prob_celtics_win_home * prob_heat_win_home)  # Celtics lose the first four games, series ends
  
  net_win_q4ii <- c(2 * bet_celtics_win, 2 * bet_celtics_lose, bet_celtics_win + bet_celtics_lose,
                    bet_celtics_lose + bet_celtics_win, bet_celtics_win + bet_celtics_win, bet_celtics_lose + bet_celtics_lose)
  
  E_X_q4ii <- sum(net_win_q4ii * c(P1, P2, P3, P4, P5, P6))
  E_X_q4ii
  
  SD_X_q4ii <- sqrt(sum((net_win_q4ii - E_X_q4ii)^2 * c(P1, P2, P3, P4, P5, P6)))
  SD_X_q4ii
  
  # III
  set.seed(114514)  # Set a seed for reproducibility
  
  Y <- sample(net_win_q4ii, size = 5000, replace = TRUE, prob = c(P1, P2, P3, P4, P5, P6))
  
  mean_Y <- mean(Y)
  se <- sd(Y) / sqrt(length(Y))
  lower <- mean_Y - 1.96 * se
  upper <- mean_Y + 1.96 * se
  is_within_interval <- (lower <= E_X_q4ii) && (E_X_q4ii <= upper)
  is_within_interval
  
  # IV
  freq_table <- table(Y)
  chi_square_test <- chisq.test(freq_table)
  chi_square_test
