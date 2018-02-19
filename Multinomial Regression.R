##The data set contains variables on 200 students. 
##The outcome variable is prog, program type. 
##The predictor variables are social economic status, ses, a three-level categorical variable and writing score, write, a continuous variable. 

# Loading the required packages
library(foreign)
library(nnet)
library(stargazer)
# Getting the sample data from UCLA
mydata = read.dta(file.choose())
# Checking the output (dependent) variable
table(mydata$ses)
##low middle high
##47 95 58
# By default the first category is the reference.
# To change it so ‘middle’ is the reference type
mydata$ses2 = relevel(mydata$ses, ref = "middle")
##NOTE: This section is based on the UCLA website http://www.ats.ucla.edu/stat/
 
dataset <- sample(1:150,120)
 train <- mydata[dataset,]
  test <- mydata[-dataset,]
    
    
    
    multi1 = multinom(ses2 ~ science + socst + female, data=train)
summary(multi1)


z <- summary(multi1)$coefficients/summary(multi1)$standard.errors
z

multitest = multinom(ses2 ~ science + socst + female, data=test)
summary(multitest)

y <- summary(multitest)$coefficients/summary(multitest)$standard.errors
y



##These are the logit coefficients relative to the reference category. For example,
##under ‘science’, the -0.02 suggests that for one unit increase in ‘science’ score,
##the logit coefficient for ‘low’ relative to ‘middle’ will go down by that amount,-0.02.
##In other words, if your science score increases one unit, your chances of staying in the middle ses category are
##higher compared to staying in low ses.

library(caret)
pred = predict(multitest, newdata=test)
accuracy <- table(pred, test[,"ses2"])
sum(diag(accuracy))/sum(accuracy)

confusionMatrix(data=pred, test$ses2)

##accuracy of .58% is achieved here


##all variables

multiall = multinom(ses2 ~ ., data=train)
summary(multiall)


z <- summary(multiall)$coefficients/summary(multiall)$standard.errors
z

multitestall = multinom(ses2 ~ ., data=test)
summary(multitestall)

y <- summary(multitestall)$coefficients/summary(multitestall)$standard.errors
y

pred = predict(multitestall, newdata=test)
accuracy <- table(pred, test[,"ses2"])
sum(diag(accuracy))/sum(accuracy)

confusionMatrix(data=pred, test$ses2)


###Upon adding other predictors the accuracy has improved 



##other accuracy validation methods


library(pscl)
pR2(multi1) ##model with few parameters has closer to 1 value in McFadden test

pR2(multiall)##model with all the parameters has insignificant value >1 indicating no prediction power.


cls<-predict(multitest,newdata = test,type = "class")
clsprob<-predict(multitest,newdata = test,type = "pro")
