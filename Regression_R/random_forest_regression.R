# Random Forest Regression

# Importing the dataset
dataset = read.csv('Data.csv')

# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$PE, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Random Forest Regression to the dataset
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
regressor = randomForest(x = training_set[1:4],
                         y = training_set$PE,
                         ntree = 500)

# Predicting a new result with Random Forest Regression
y_pred = predict(regressor, test_set)

preds <- y_pred
actual <- array(test_set$PE)
mean(actual)
rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
adrsq = 1-(1-rsq)*(1913/1909)