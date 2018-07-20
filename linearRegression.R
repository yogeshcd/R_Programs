#Create lm for 3rd semister grade.
getwd()
df = read.csv("student-mat.csv")
str(df)
View(df)
dim(df)
head(df)
summary(df)
##################################
#1 school - student's school (binary: 'GP' - Gabriel Pereira or 'MS' - Mousinho da Silveira)
#2 sex - student's sex (binary: 'F' - female or 'M' - male)
#3 age - student's age (numeric: from 15 to 22)
#4 address - student's home address type (binary: 'U' - urban or 'R' - rural)
#5 famsize - family size (binary: 'LE3' - less or equal to 3 or 'GT3' - greater than 3)
#6 Pstatus - parent's cohabitation status (binary: 'T' - living together or 'A' - apart)
#7 Medu - mother's education (numeric: 0 - none, 1 - primary education (4th grade), 2 â€“ 5th to 9th grade, 3 â€“ secondary education or 4 â€“ higher education)
#8 Fedu - father's education (numeric: 0 - none, 1 - primary education (4th grade), 2 â€“ 5th to 9th grade, 3 â€“ secondary education or 4 â€“ higher education)
#9 Mjob - mother's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')
#10 Fjob - father's job (nominal: 'teacher', 'health' care related, civil 'services' (e.g. administrative or police), 'at_home' or 'other')
#11 reason - reason to choose this school (nominal: close to 'home', school 'reputation', 'course' preference or 'other')
#12 guardian - student's guardian (nominal: 'mother', 'father' or 'other')
#13 traveltime - home to school travel time (numeric: 1 - less than 15 min., 2 - 15 to 30 min., 3 - 30 min. to 1 hour, or 4 - more than 1 hour)
#14 studytime - weekly study time (numeric: 1 - less than 2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - more than 10 hours)
#15 failures - number of past class failures (numeric: n if between 1 and 3 , else 4)
#16 schoolsup - extra educational support (binary: yes or no)
#17 famsup - family educational support (binary: yes or no)
#18 paid - extra paid classes within the course subject (Math or Portuguese) (binary: yes or no)
#19 activities - extra-curricular activities (binary: yes or no)
#20 nursery - attended nursery school (binary: yes or no)
#21 higher - wants to take higher education (binary: yes or no)
#22 internet - Internet access at home (binary: yes or no)
#23 romantic - with a romantic relationship (binary: yes or no)
#24 famrel - quality of family relationships (numeric: from 1 - very bad to 5 - excellent)
#25 freetime - free time after school (numeric: from 1 - very low to 5 - very high)
#26 goout - going out with friends (numeric: from 1 - very low to 5 - very high)
#27 Dalc - workday alcohol consumption (numeric: from 1 - very low to 5 - very high)
#28 Walc - weekend alcohol consumption (numeric: from 1 - very low to 5 - very high)
#29 health - current health status (numeric: from 1 - very bad to 5 - very good)
#30 absences - number of school absences (numeric: from 0 to 93)

#these grades are related with the course subject, Math or Portuguese:
  
#31 G1 - first period grade (numeric: from 0 to 20)
#31 G2 - second period grade (numeric: from 0 to 20)
#32 G3 - final grade (numeric: from 0 to 20, output target)

##################################
any(is.na(df))
#FALSE...great :')


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


test = subset(df, sample == T) # Using subset function from dplyr package
train = subset(df, sample == F)


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

