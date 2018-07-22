#Create lm for 3rd semister grade.
getwd()
df = read.csv("student-mat.csv")
str(df)
View(df)
dim(df)
head(df)
summary(df)
##################################

#31 G1 - first period grade (numeric: from 0 to 20)
#31 G2 - second period grade (numeric: from 0 to 20)
#32 G3 - final grade (numeric: from 0 to 20, output target)

##################################
any(is.na(df))
#FALSE


#EDA 
library(ggplot2)
library(ggthemes)
library(dplyr)

#Filtering numerical columns for correlation.
numCols = sapply(df, is.numeric)
corData = cor(df[,numCols])
cor(corData)

library(corrgram)
corrgram(df,order=TRUE, lower.panel = panel.shade, upper.panel = panel.pie, text.panel = panel.txt)

#Histogram for G3 variable
ggplot(df, aes(G3)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue') + theme_minimal()

#Split data in traning and testing.
library(caTools)
#Create a new column of boolean values
boolColumn = sample.split(df$age, SplitRatio = 0.7)
typeof(boolColumn)
str(boolColumn)


test = subset(df, boolColumn == T) # Using subset function from dplyr package
train = subset(df, boolColumn == F)


#Model building
lmModel = lm(G3~., data = df)
summary(lmModel)

#Ploting graph for resudial of model
res = residuals(lmModel)
res = as.data.frame(res)

ggplot(res, aes(res)) + geom_histogram(alpha = 0.5, fill = 'blue') #Model created -ve grades.

#Predicting
prediction = predict(lmModel, test)

#Creating result (as.data.frame) for computing mse, r squared
result = cbind(prediction, test$G3)
typeof(result)
result = as.data.frame(result)
typeof(result)
str(result)

#There are some -ve grades in precistion columns
any(result$prediction < 0)

#Function for converting -ve values to 0
#Accept each value row by row by using sapply
toZero = function(x){
  if (x < 0)
    return(0)
  else
    return(x)
}

result$prediction <- sapply(result$prediction,toZero)
any(result$prediction < 0)
mse = mean((result$prediction - result$V2)^2)
mse
rmse = mse ^ 0.5

#For calculating R-squared value
SSE = sum((result$prediction - result$V2)^2)
SST = sum( (mean(df$G3) - result$V2)^2)

R2 = 1 - SSE/SST
R2

