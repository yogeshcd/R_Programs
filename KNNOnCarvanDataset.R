#Using Carvan data set for predicting Caravan insurance policy
library(ISLR)
df = Caravan
str(df)

summary(df$Purchase)
any(is.na(df))

#While using KNN classifier, scale of variables matters as it 
#identifying the observations that are nearest to it.
var(df[,1])
var(df[,2])

yLabel = df[,86]

standardizedDf = scale(df[,-86])
typeof(standardizedDf)
head(standardizedDf)
standardizedDf = as.data.frame(standardizedDf)
var(standardizedDf[,1])
var(standardizedDf[,2])


#Spliting data into training and testing.
library(caTools)

boolVal = sample.split(standardizedDf$MOSTYPE, SplitRatio = 0.7)

library(dplyr)
trainDf = subset(standardizedDf, boolVal == T)
testDf = subset(standardizedDf, boolVal == F)
trainPurchase = subset(yLabel, boolVal == T)
testPurchase = subset(yLabel, boolVal == F)
typeof(trainDf)
typeof(trainPurchase)
head(trainDf)


#Building Model
library(class)
predictedPurchase = knn(trainDf, testDf, trainPurchase, k = 5)


#Misclassification error rate.
mean(testPurchase != predictedPurchase)


#To find optimum value for k.
error = NULL

for(i in 1:20){
  predictedPurchase = knn(trainDf, testDf, trainPurchase, k =i)
  error[i] = mean(testPurchase != predictedPurchase)
  
}

library(ggplot2)

error = as.data.frame(error)
ggplot(data = error, aes(1:20, error)) + geom_point() + geom_line(lty='dotted', color = 'red') + scale_x_continuous(breaks = seq(min(0), max(20)))
