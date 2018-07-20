library(rpart)
library(rpart.plot)
library(ggplot2)
df = read.csv("kyphosis.csv")
str(df)
head(df)
##################
#Kyphosis-a factor with levels absent present indicating if a kyphosis (a type of deformation) was present after the operation.
#Age-in months
#Number-the number of vertebrae involved
#Start-the number of the first (topmost) vertebra operated on.
##################

#EDA of variables.
ggplot(df, aes(Age)) + geom_bar(binwidth = 10)
ggplot(df, aes(Start)) + geom_bar()
ggplot(df, aes(Number)) + geom_bar()

#Tree model.
?rpart
tree = rpart(Kyphosis ~., data=df, method="class")
plot(tree, uniform = T)
text(tree, use.n = T, all = T)

#From package rpart.plot.
prp(tree)

#Random forest.
library(randomForest)
?randomForest
rFModel = randomForest(Kyphosis~., data = df)
print(rFModel)

#Gini test of Variables.
importance(rFModel)
