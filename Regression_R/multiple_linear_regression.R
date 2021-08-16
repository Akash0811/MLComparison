# Multiple Linear Regression

# Importing the dataset
dataset = read.csv('Data.csv')

# # Encoding categorical data
# dataset$State = factor(dataset$State,
#                        levels = c('New York', 'California', 'Florida'),
#                        labels = c(1, 2, 3))

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$PE, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Multiple Linear Regression to the Training set
regressor = lm(formula = PE ~ .,
               data = training_set)
summary(regressor)
# Predicting the Test set results
y_pred = predict(regressor, newdata = test_set)

preds <- y_pred
actual <- array(test_set$PE)
mean(actual)
rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
adrsq = 1-(1-rsq)*(1913/1909)
