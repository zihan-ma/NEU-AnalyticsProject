# set pseudorandom number generator
set.seed(10)

# Attach Packages
library(tidyverse)    # data manipulation and visualization
library(kernlab)      # SVM methodology
library(e1071)        # SVM methodology
library(ISLR)         # contains example data set "Khan"
library(RColorBrewer) # customized coloring of plots

## Support Vector Machine methodology is sound for any number of dimensions, 
## but becomes difficult to visualize for more than 2.

#############################
## Linearly separable data ## -- SVC
#############################

## If the classes are separable by a linear boundary, 
## we can use a Maximal Margin Classifier to find the classification boundary.

# We generate 40 random observations and assign them to two classes
# Construct sample data set - completely separated
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 3/2
dat <- data.frame(x=x, y=as.factor(y))

# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

# The goal of the maximal margin classifier(SVC) is to identify the linear boundary 
# that maximizes the total distance between the line and the closest point in each class.

# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", scale = FALSE)
# Plot Results
plot(svmfit, dat)

## In the plot, points that are represented by an “X” are the support vectors, 
## or the points that directly affect the classification line. 
## The points marked with an “o” are the other points, 
## which do not affect the calculation of the line.

# Using kernlab package
# fit model and produce plot
kernfit <- ksvm(x, y, type = "C-svc", kernel = 'vanilladot')
plot(kernfit, data = x)

# kernlab shows a little more detail than e1071, 
# showing a color gradient that indicates how confidently a new point would be 
# classified based on its features.

#################################
## Non-linearly separable data ## --  SVM
#################################

# Construct sample data set - not completely separated
x <- matrix(rnorm(20*2), ncol = 2)
y <- c(rep(-1,10), rep(1,10))
x[y==1,] <- x[y==1,] + 1
dat <- data.frame(x=x, y=as.factor(y))

# Plot data set
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

# In the case of data that is not linearly separable, however, 
# the cost = argument takes on real importance. This quantifies the penalty associated 
# with having an observation on the wrong side of the classification boundary. 
# We can plot the fit in the same way as the completely separable case.

# Fit Support Vector Machine model to data set
svmfit <- svm(y~., data = dat, kernel = "linear", cost = 10)
# Plot Results
plot(svmfit, dat)

# Fit Support Vector Machine model to data set
kernfit <- ksvm(x,y, type = "C-svc", kernel = 'vanilladot', C = 100)
# Plot results
plot(kernfit, data = x)

## Instead of specifying a cost up front, we can use the tune() function from 
## e1071 to test various costs and identify which value produces the best fitting model.

# find optimal cost of misclassification
tune.out <- tune(svm, y~., data = dat, kernel = "linear",
                 ranges = list(cost = c(0.001, 0.01, 0.1, 1, 5, 10, 100),
                               gamma = c(0.1,0.5,1)))
# extract the best model
(bestmod <- tune.out$best.model)

# Create a table of misclassified observations
ypred <- predict(bestmod, dat)
(misclass <- table(predict = ypred, truth = dat$y))
accuracy = sum(diag(misclass))/ sum(misclass); accuracy

#Support Vector Classifiers are a subset of the group of classification structures known as Support Vector Machines. 
#Support Vector Machines can construct classification boundaries that are nonlinear in shape. 
#The options for classification structures using the svm() command from the e1071 package are 
#linear, polynomial, radial, and sigmoid. To demonstrate a nonlinear classification boundary, 
#we will construct a new data set.

# construct larger random data set
x <- matrix(rnorm(200*2), ncol = 2)
x[1:100,] <- x[1:100,] + 2.5
x[101:150,] <- x[101:150,] - 2.5
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x,y=as.factor(y))

# Plot data
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000", "#FF0000")) +
  theme(legend.position = "none")

#Notice that the data is not linearly separable, and furthermore, isn’t all clustered together in a single group. 
#There are two sections of class 1 observations with a cluster of class 2 observations in between. 
#To demonstrate the power of SVMs, we’ll take 100 random observations from the set and use them to 
#construct our boundary. We set kernel = "radial" based on the shape of our data and plot 
#the results.

# set pseudorandom number generator
set.seed(123)
# sample training data and fit model
train <- base::sample(200,100, replace = FALSE)
svmfit <- svm(y~., data = dat[train,], kernel = "radial", gamma = 1, cost = 1)
# plot classifier
plot(svmfit, dat)

# Using kernlab
# Fit radial-based SVM in kernlab
kernfit <- ksvm(x[train,],y[train], type = "C-svc", kernel = 'rbfdot', C = 1, scaled = c())
# Plot training data
plot(kernfit, data = x[train,])

# Let's tune the cost
# tune model to find optimal cost, gamma values
tune.out <- tune(svm, y~., data = dat[train,], kernel = "radial",
                 ranges = list(cost = c(0.1,1,10,100,1000),
                               gamma = c(0.5,1,2,3,4)))
# show best model
tune.out$best.model

(valid <- table(true = dat[-train,"y"], pred = predict(tune.out$best.model,
                                                       newx = dat[-train,])))
accuracy = sum(diag(valid))/ sum(valid); accuracy

## SVMs for Multiple Classes

# construct data set
x <- rbind(x, matrix(rnorm(50*2), ncol = 2))
y <- c(y, rep(0,50))
x[y==0,2] <- x[y==0,2] + 2.5
dat <- data.frame(x=x, y=as.factor(y))
# plot data set
ggplot(data = dat, aes(x = x.2, y = x.1, color = y, shape = y)) + 
  geom_point(size = 2) +
  scale_color_manual(values=c("#000000","#FF0000","#00BA00")) +
  theme(legend.position = "none")

# fit model
svmfit <- svm(y~., data = dat, kernel = "radial", cost = 10, gamma = 1)
# plot results
plot(svmfit, dat)

ypred <- predict(svmfit, dat)
(misclass <- table(predict = ypred, truth = dat$y))
accuracy = sum(diag(misclass))/ sum(misclass); accuracy

# Using kernlab
# fit and plot
kernfit <- ksvm(as.matrix(dat[,2:1]),dat$y, type = "C-svc", kernel = 'rbfdot', 
                C = 100, scaled = c())

# Create a fine grid of the feature space
x.1 <- seq(from = min(dat$x.1), to = max(dat$x.1), length = 100)
x.2 <- seq(from = min(dat$x.2), to = max(dat$x.2), length = 100)
x.grid <- expand.grid(x.2, x.1)

# Get class predictions over grid
pred <- predict(kernfit, newdata = x.grid)

# Plot the results
cols <- brewer.pal(3, "Set1")
plot(x.grid, pch = 19, col = adjustcolor(cols[pred], alpha.f = 0.05))

classes <- matrix(pred, nrow = 100, ncol = 100)
contour(x = x.2, y = x.1, z = classes, levels = 1:3, labels = "", add = TRUE)

points(dat[, 2:1], pch = 19, col = cols[predict(kernfit)])

