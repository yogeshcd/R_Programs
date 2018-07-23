#SVM on iris dataset

library(ISLR)

df = iris

str(iris)
head(iris)

#Spliting data.
library(caTools)
bool = sample.split(df$Sepal.Length, 0.7)

library(dplyr)
train = subset(df, bool == T)
test = subset(df, bool == F)

#Building model
library(e1071)
?svm
svmModel = svm(Species ~., train)
summary(svmModel)

predicted = predict(svmModel, test[1:4])

table(predicted, test$Species)

# Tuning for cost and gama.
tuneModel = tune(svm, train.x = train[1:4], train.y = train[,5], kernel = 'radial', range = list(cost= 10^(-1:2), gamma = c(0.5, 1,1.5)))
summary(tuneModel)
#6    1.0   1.0 0.05636364 0.08015137

tunedSvmModel = svm(Species ~., train)
summary(tunedSvmModel)
tunedPredicted = predict(tunedSvmModel, test[1:4])
table(tunedPredicted, test$Species)
