#Titanic data set from kaggle

df = read.csv("titanic_train.csv")
str(df)
head(df)

#EDA
library(Amelia)
missmap(df, col = c("yellow", "black")) #Age column has missiong values.

library(ggplot2)
#Bar chart for Survived
ggplot(df,aes(Survived)) + geom_bar(aes(fill = factor(Survived)), alpha = 0.5) + scale_y_continuous(breaks = seq(min(0), max(600), by=20))

#Bar chart for Pclass
ggplot(df, aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)), alpha = 0.5)

#Bar chart for Pclass
ggplot(df, aes(Sex)) + geom_bar(aes(fill = factor(Sex)))

#Histogram for Age
ggplot(df, aes(Age)) + geom_histogram(alpha = 0.5, bins = 20, fill = 'blue') 

#Histogram for SibSp 
ggplot(df, aes(SibSp)) + geom_bar(aes(fill = factor(SibSp)), alpha = 0.5)

#Histogram for Fare.
ggplot(df, aes(Fare)) + geom_histogram(fill = 'blue', alpha = 0.5)

#Box plot of Pclass vs age.
maxAge = max(df$Age, na.rm = T)

ggplot(df, aes(Pclass, Age)) + geom_boxplot(aes(group = Pclass, fill = factor(Pclass)), alpha = 0.5) + scale_y_continuous(breaks = seq(min(0), max(maxAge), by= 2))


#Replacing NA's in Age columns according to the median age of different Pclass

ImputeAge = function(age, pclass){
  out = age

  for(i in 1 : length(age)){
    
    if(is.na(age[i])){
      
      if(pclass[i] == 1){
        out[i] = 37
        #print(out[i])
      }else if(pclass[i] == 2){
        
        out[i] = 29
        #print(out[i])
      }else{
        out[i] = 24
        #print(out[i])
      }
      
    }else{
      out[i] = age[i]
    }
    
    
  }
#  print(length(out))
#  print(any(is.na(out)))
  return(out)
}

ageDouble = ImputeAge(df$Age, df$Pclass)

any(is.na(ageDouble))
df$Age = ageDouble
any(is.na(df))

#Building logistic regression regression.
str(df)

library(dplyr)
df = select(df, -PassengerId, -Name, -Ticket, -Cabin)
str(df)
df$Survived = as.factor(df$Survived)
df$Pclass = as.factor(df$Pclass)
df$SibSp = as.factor(df$SibSp)
df$Parch = as.factor(df$Parch)

logModel = glm(Survived ~ ., family = binomial(link = 'logit'), data = df)
summary(logModel)


#Dividin df in training and testing datasets.
library(caTools)
library(dplyr)
boolVector = sample.split(df$Survived, SplitRatio = 0.7)

dfTrain = subset(df, boolVector == T)
dfTest = subset(df, boolVector == F)
length(dfTrain$Survived)
length(dfTest$Survived)

#Building model.
newLogModel = glm(dfTrain$Survived ~., family = binomial(link = 'logit'), data = dfTrain)

predictedSurvived = predict(newLogModel, newdata = dfTest, type = 'response') #type = 'response': values are betweeen 0 and 1
head(predictedSurvived)
predictedSurvived = ifelse(predictedSurvived >0.5, 1,0)
head(predictedSurvived)
any(is.na(predictedSurvived))
any(is.na(dfTest$Survived))
length(predictedSurvived)

missClassificationError = mean(predictedSurvived != dfTest$Survived)
modelAccuracy = 1 - missClassificationError
table(dfTest$Survived, predictedSurvived > 0.5)
