# KNN Classifier

# Importing the dataset
dataset = read.csv('Data.csv')

dataset = dataset[2:11]

dataset$Class = factor(dataset$Class , levels=c(2,4))
# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Class, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
library(dataPreparation)
scales <- build_scales(training_set, cols = "auto",
                       verbose = TRUE)

training_set[,1:9] = fast_scale(training_set[,1:9], scales = scales,
                                verbose = TRUE)
test_set[,1:9] = fast_scale(test_set[,1:9], scales = scales, verbose = TRUE)


# Fitting Logistic REgression to the Training Set
# classifier = glm(formula = Purchased ~ .,
#                  family = binomial,
#                data = training_set)
# 
# summary(classifier)
# Create classifier here
# Fitting and Predicting
library(class)
y_pred = knn(train = training_set[,1:9],
             test = test_set[,1:9],
             cl = training_set$Class,
             k = 5)


# # Predicting the test set results
# prob_pred = predict(classifier, type = 'response' , newdata = test_set)
# y_pred = ifelse(prob_pred > 0.5, 1,0)

cm = table(test_set[,10],y_pred)
