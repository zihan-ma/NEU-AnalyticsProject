# ALY6015
# Module 1
# Multiple Linear Regression example

# Un-comment to install package as needed
# install.packages("ggplot2")
# install.packages("corrplot")

library(ggplot2)  # advanced histograms plotting 
library(corrplot) # correlation matrix visualization
library(car)      # error correlation tests


# Read housing data set. When prompted navigate and open Housing.csv file
housing <- read.table(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)

#Add custom column names for loaded dataset
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value",
                    "ValuePerSqFt", "Boro")

# Show sample from the neginning of the dataset
head(housing)

# Visualize value per sq.ft using histograms
ggplot(housing, aes(x=ValuePerSqFt)) + geom_histogram(binwidth=10) + labs(x="Value per Square Foot")

# Check normality of value per sq ft
qqnorm(housing$ValuePerSqFt, pch=16, col="red", main=expression("Check ValuePerSqFt Normality"))

# Add blue line to check normality
qqline(housing$ValuePerSqFt, col="blue")  

# Visualize value per sq.ft. but add colors based on boro
ggplot(housing, aes(x=ValuePerSqFt, fill=Boro)) + geom_histogram(binwidth=10) + labs(x="Value per Sq Ft")

# Get multiple linear regression function
myModel <- lm(ValuePerSqFt ~ Units + SqFt + Boro, data = housing)
# Show model data
summary(myModel)

# Output results of error correlation test
durbinWatsonTest(myModel)

# Check linearity between each predictor and target variable
# avPlots(myModel)

# Construct and display residuals plot for the model
# Keep hitting enter in the Console window to see plots
plot(myModel)

# Reformat plot screen to show 4 plots (2 rows and 2 columns)
par(mfrow = c(2,2))

# Show all four plots at once
plot(myModel)


# Rest back screen formatting to 1 plot
par(mfrow = c(1,1))

# Check model linearity using Component + Residual Plots
crPlots(myModel)

# Get the VIF for the lm model
vif(myModel)

# Extract Units, SqFt, Income, IncomePerSqFt, Expense, ExpensePerSqFt columns only
m1 <- housing[,c(3,5,6,7,8,9)]

# Create correlation matrix
myCorr <- cor(m1)

# Visualize correlation matrix
corrplot(myCorr)

# Show correlation matrix data
myCorr

