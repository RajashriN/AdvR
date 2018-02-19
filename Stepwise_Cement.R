##stepwise regression

##cement data set to predict the hardening effect by various chemicals


cement <- read.csv(file.choose())
summary(cement)

##model building


##split

cement$rowno <- 1:dim(cement)[1]
train<-subset(cement,rowno <= dim(cement)[1]*0.7); 
test<-subset(cement,rowno > dim(cement)[1]*0.7)
nrow(test)
##verifying the variable significance

step_regall<-lm(y ~ 1,data = train)
summary(step_regall)

step_reg<- lm(y ~ .,data = train)
summary(step_reg)

str(cement)

step(step_regall, scope=list(lower=step_regall, upper=step_reg), direction="forward")


step(step_reg, scope=list(lower=step_reg, upper=step_regall), direction="backward")

##step function to determine AIC for different combinations of parameters




stpe_final <- lm(y ~ trical.AL+trical..Si , data=train)
summary(stpe_final)



library(leaps)

leaps <- regsubsets(y ~ trical.AL+trical..Si,data=train,nbest=10)

plot(leaps, scale="adjr2"); plot(leaps, scale="bic")

##Validating using corrplot

dim(cement)
length(dim(cement))
corrplot(cor(cement), method="circle",shade.col=NA, tl.col="black", tl.srt=45)
##positive predictors are trical.AL,trical.Si
##Negative predictors are tetracal.FE,dical.Si




