# ALY6015
# Week 1
# Simple linera regression example:

# Show all available datasets
data()

# Show explanaton on dataset cars 
? cars

# Show all data points of the dataset cars
plot(cars$speed, cars$dist, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",  las = 1)

# Create simple linear regression model 
myModel <- lm(cars$dist~cars$speed)

# Add linear regression model line to the data point plot
abline(myModel)

# Show contents of the linear regression model
myModel

# Predict distance using created liner regression model
predictDist <-predict(myModel)

# Show predicted distance values
predictDist

# Add predicted values into plot using blue color
points(predictDist~cars$speed, xlab = "Speed (mph)", ylab = "Stopping distance (ft)",  las = 1, col="blue", bg="blue", pch=21)

# Calculate the residal values for the model
resid(myModel)

# test that calculated RSS equal to once stored withn model
# resulted vector should have zeroes if true
resid(myModel) - myModel$residuals

# Get the number of observations
n <- length (cars$dist)

# Plot the red lines representing the difference
# between existing and predicted values for each data point
for (i in 1 : n) {
  # Plot line from X,Y in data set to X, predictedY in model
  lines(c(cars$speed[i], cars$speed[i]), c(cars$dist[i], predictDist[i]), col="red")
}
    
