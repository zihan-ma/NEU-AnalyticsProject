# ALY6015
# Feature selection

# Read data file. When prompted select housing.csv
housing <- read.table(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)

# Add column ames to the data set
names(housing) <- c("Neighborhood", "Class", "Units", "YearBuilt",
                    "SqFt", "Income", "IncomePerSqFt", "Expense",
                    "ExpensePerSqFt", "NetIncome", "Value",
                    "ValuePerSqFt", "Boro")

# Preview beginning of the dataset
head(housing)

# the lowest model is the null model, basically the straight average
nullModel <- lm(ValuePerSqFt ~ 1, data=housing)

# the largest model we will accept
fullModel <- lm(ValuePerSqFt ~ Units + SqFt*Boro + Boro*Class, data=housing)

# try different models
# start with nullModel, do not go above fullModel, # work in both directions
houseStep <- step(nullModel, scope=list(lower=nullModel, upper=fullModel), direction="both")


