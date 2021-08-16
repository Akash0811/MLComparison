# Polynomial Regression

# Importing the dataset
dataset = read.csv('Data.csv')

# Splitting the dataset into the Training set and Test set
# # install.packages('caTools')
# library(caTools)
# set.seed(123)
split = sample.split(dataset$PE, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
# training_set = scale(training_set)
# test_set = scale(test_set)

# Fitting Polynomial Regression to the dataset
training_set$AT2 = training_set$AT^2
training_set$AT3 = training_set$AT^3
training_set$AT4 = training_set$AT^4

training_set$V2 = training_set$V^2
training_set$V3 = training_set$V^3
training_set$V4 = training_set$V^4

training_set$AP2 = training_set$AP^2
training_set$AP3 = training_set$AP^3
training_set$AP4 = training_set$AP^4

training_set$RH2 = training_set$RH^2
training_set$RH3 = training_set$RH^3
training_set$RH4 = training_set$RH^4
poly_reg = lm(formula = PE ~ .,
              data = training_set)


# Predicting a new result with Polynomial Regression

test_set$AT2 = test_set$AT^2
test_set$AT3 = test_set$AT^3
test_set$AT4 = test_set$AT^4

test_set$V2 = test_set$V^2
test_set$V3 = test_set$V^3
test_set$V4 = test_set$V^4

test_set$AP2 = test_set$AP^2
test_set$AP3 = test_set$AP^3
test_set$AP4 = test_set$AP^4

test_set$RH2 = test_set$RH^2
test_set$RH3 = test_set$RH^3
test_set$RH4 = test_set$RH^4

y_pred = predict(poly_reg, test_set)

preds <- y_pred
actual <- array(test_set$PE)
mean(actual)
rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
rsq <- 1 - rss/tss
adrsq = 1-(1-rsq)*(1913/1909)