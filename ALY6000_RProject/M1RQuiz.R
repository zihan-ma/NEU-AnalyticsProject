# Class:      ALY6000-20312.202325
# Report:     Module 1 R Quiz
# Professor:  Dr. Dee Chiluiza
# Student:    Zihan Ma
# Date:       01/23/2023

x1 = c(11, 15, 16, 20, 27, 30)
x2 = c(4, 6, 8, 10, 14, 25)

q1 = sqrt(sum(x1*x2))
q1

x1 = c(3,4,6,8,12)
x2 = c(5,3,6,9,15)

q2 = sqrt(x1*x2)
q2

vectorValues = c(460.998, 314.4, 290.475, 247.900, 309.306, 165.8)

q3 = matrix(vectorValues, nrow = 3, byrow = TRUE)
q3

#q5 = c{"blue", "red", "yellow"}
#q5

# Create the data frame.
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)
# Print the data frame.			
print(emp.data)

summary(emp.data)
emp.data$start_date

cars = 25

mean_price = 3000

total = cars*mean_price
cars*mean_price = price
total

x <- c(1,2,3,4,5,6)

q11 = mean(x)+median(x)^2
q11

poker_vector = c(140, -50, 20, -120, 240)
days_vector = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
names(poker_vector) = days_vector
winnings = poker_vector  > 0

winnings

vector_a = c(5, 9, 12, 4, 13, 14, 7, 16, 22, 12, 19, 17, 14, 19)

print(vector_a>15)

x1 = c(11, 15, 16, 20, 27, 30)

x2 = c(4, 6, 8, 10, 14, 25)

q14 = sum(x1*x2)^2
q14

poker_vector = c(140, -50, 20, -120, 240)
days_vector = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
names(poker_vector) = days_vector
poker_vector 

vector_a = c(3, 4, 6, 2, 7, 4, 9)
sum(vector_a^2)

poker_vector <- c(140, -50, 20, -120, 240)
days_vector <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
names(poker_vector) <- days_vector
print(poker_vector > 0)

a = c(12,14, 5, 34, 28, 32, 22, 26, 18, 55, 52, 4, 70)
print(median(a))
boxplot(a)

data(iris)
summary(iris)

mean_x  = 3.3489274753
round((mean_x), decimals = 2)

weight = c(20,22,30,24,44,75,120,150,200,80)
mean(weight)



