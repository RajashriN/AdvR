##imputation with iris data set

datairis <- iris

##generating 10% missing values

library(missForest)
iris.mis <- prodNA(iris, noNA = 0.1)
summary(iris.mis)
##sepal.length - 14 NA
##Sepal.width - 16 NA
##petal.length - 15 na
##petal.width - 11 NA
##specied - 19 NA

iris.mis <- subset(iris.mis, select = -c(Species))
 summary(iris.mis)
 md.pattern(iris.mis)
 
 ##The summary tells us that there are 101 observations without null,
 ##12 observations with null in sepal length
 ##13 obs woth speal width and so on
 imputed_Data <- mice(iris.mis, m=5, maxit = 50, method = 'pmm', seed = 500)
 
 ##m = 5 imputed data sets,maxit = 50 iterations,method = predictive mean modelling,since we have numerical data
 ##logreg when we have categorical data with 2 levels,ployreg when there are >2 levels
 summary(imputed_Data)
 
 
 imputed_Data$imp$Sepal.Width
 completeData <- complete(imputed_Data,2) 
 View(completeData)
 
 
 
 