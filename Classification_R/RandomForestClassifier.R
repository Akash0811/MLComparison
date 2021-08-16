# Random Forest Classifier

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
library(randomForest)
classifier = randomForest(Class ~.,
                   data = training_set,
                   ntree = 5)

# Predicting the test set results
y_pred = predict(classifier, newdata = test_set[-10], type = "class")

cm = table(test_set[,10],y_pred)

