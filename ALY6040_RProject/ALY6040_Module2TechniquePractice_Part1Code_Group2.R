# Decision Tree Mushrooms

# #Installing libraries
# install.packages('rpart')
# install.packages('caret')
# install.packages('rpart.plot')
# install.packages('rattle')
# install.packages('readxl')

#Loading libraries
library(rpart,quietly = TRUE)
library(caret,quietly = TRUE)
library(rpart.plot,quietly = TRUE)
library(rattle)
library(readxl)
library(ggplot2) # additional library for advanced visualization

#Reading the data set as a dataframe

mushrooms <- read_excel("mushrooms.xlsx")

# structure of the data
str(mushrooms)


# number of rows with missing values
nrow(mushrooms) - sum(complete.cases(mushrooms))

# deleting redundant variable `veil.type`
mushrooms$veil.type <- NULL


#analyzing the odor variable
table(mushrooms$class,mushrooms$odor)

number.perfect.splits <- apply(X=mushrooms[-1], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$class,col)
  sum(t == 0)
})


# Descending order of perfect splits
order <- order(number.perfect.splits,decreasing = TRUE)
number.perfect.splits <- number.perfect.splits[order]

# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")


#data splicing
set.seed(12345)
train <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)),replace = FALSE)
# training set
mushrooms_train <- mushrooms[train,]
# test set
mushrooms_test <- mushrooms[-train,]


# penalty matrix
penalty.matrix <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)


# building the classification tree with rpart
tree <- rpart(class~.,
              data=mushrooms_train,
              parms = list(loss = penalty.matrix),
              method = "class")


# Visualize the decision tree with rpart.plot
rpart.plot(tree, nn=TRUE)


# choosing the best complexity parameter "cp" to prune the tree
cp.optim <- tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]
# tree prunning using the best complexity parameter. For more in
tree <- prune(tree, cp=cp.optim)


#Testing the model
pred <- predict(object=tree,mushrooms_test[-1],type="class")


#Calculating accuracy
t <- table(mushrooms_test$class,pred) 
confusionMatrix(t) 

# ----------------------------------------------

#Rename the "gill-size" column to something without a special character
names(mushrooms)[names(mushrooms) == "gill-size"] <- "gillsize"

#analyzing the gill-size variable
table(mushrooms$gillsize,mushrooms$'stalk-color-above-ring')

number.perfect.splits_gillsize <- apply(X=mushrooms[-9], MARGIN = 2, FUN = function(col){
  t <- table(mushrooms$gillsize,col)
  sum(t == 0)
})

# Descending order of perfect splits
order_gillsize <- order(number.perfect.splits_gillsize,decreasing = TRUE)
number.perfect.splits_gillsize <- number.perfect.splits_gillsize[order]

# Plot graph
par(mar=c(10,2,2,2))
barplot(number.perfect.splits_gillsize,
        main="Number of perfect splits vs feature",
        xlab="",ylab="Feature",las=2,col="wheat")

#data splicing
set.seed(12345)
train <- sample(1:nrow(mushrooms),size = ceiling(0.80*nrow(mushrooms)),replace = FALSE)
# training set
mushrooms_train_gillsize <- mushrooms[train,]
# test set
mushrooms_test_gillsize <- mushrooms[-train,]

# Penalty matrix for "gill-size"
penalty.matrix_gillsize <- matrix(c(0,1,10,0), byrow=TRUE, nrow=2)

# building the classification tree with rpart for "stalk-shape"
tree_gillsize <- rpart(gillsize~.,
                    data=mushrooms_train_gillsize,
                    parms = list(loss = penalty.matrix_gillsize),
                    method = "class")

# choosing the best complexity parameter "cp" to prune the tree
cp.optim_gillsize <- tree_gillsize$cptable[which.min(tree_gillsize$cptable[,"xerror"]),"CP"]

# tree prunning using the best complexity parameter. For more in
tree_gillsize <- prune(tree_gillsize, cp=cp.optim_gillsize)

# Visualize the decision tree with rpart.plot
rpart.plot(tree_gillsize, nn=TRUE)

# Testing the model
pred_gillsize <- predict(object=tree_gillsize,mushrooms_test_gillsize[-9],type="class")

# Calculating accuracy
t_gillsize <- table(mushrooms_test_gillsize$gillsize,pred_gillsize) 
confusionMatrix(t_gillsize) 
























