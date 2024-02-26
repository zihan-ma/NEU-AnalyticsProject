# Class:      ALY6000-20312.202325
# Report:     Module 1 Project
# Professor:  Dr. Dee Chiluiza
# Student:    Zihan Ma
# Date:       01/15/2023

#print(paste("", round(, 2)))

# Task 1: Create a vector named car_speed and enter data from table 1.

Speed1 = c(32.48, 33,59, 57.00, 58.45, 58.83, 59.31, 60.28, 62.40, 62.71, 63.39, 64.26, 66.87, 70.26, 70.64, 73.43, 74.91, 76.69, 76.85, 77.07, 77.33) # enter rest of values

# Task 2: Obtain the average speed

meanSpeed1 = mean(Speed1)
#print(paste("The mean speed 1 is", round(Speed1, 2), "miles per hour"))
print(paste("Task 2: Obtain the average speed", round(meanSpeed1, 2)))

# Task 3: Obtain the median speed

medianSpeed1 = median(Speed1)
print(paste("Task 3: Obtain the median speed", round(medianSpeed1, 2)))

# Task 4: Obtain the standard deviation

SDspeed1 = sd(Speed1)
print(paste("Task 4: Obtain the standard deviation", round(SDspeed1, 2)))

# Task 5: Obtain the quantiles:

quantilespeed1 = quantile(Speed1)
print(paste("Task 5: Obtain the quantiles:", quantilespeed1))

# Task 6: Delete values 32.48

Speed2 = Speed1[ !Speed1 == '32.48']

print(paste("Task 6: Delete values 32.48. test result"))
print(paste(Speed1)) 
print(paste(Speed2))

meanSpeed2 = mean(Speed2)
medianSpeed2 = median(Speed2)
SDspeed2 = sd(Speed2)
quantilespeed2 = quantile(Speed2)

# Task 7: Add values 98.21

Speed3 = append(Speed1, 98.21)

print(paste("Task 7: Add values 98.21. test result"))
print(paste(Speed1)) 
print(paste(Speed3))

meanSpeed3 = mean(Speed3)
medianSpeed3 = median(Speed3)
SDspeed3 = sd(Speed3)
quantilespeed3 = quantile(Speed3)

# Task 8: Use vectors to store and organize objects




# Task 9: Present the values using a table




# Task 10: Using Speed 3, present all values above 70. 




# Task 11: Sum values that are higher than 70. 




# Task 12: Sum values that are lower 70. 




# Task 13: Duplicate the following table using R codes. 




# Task 14: Write a summary of your project 1











