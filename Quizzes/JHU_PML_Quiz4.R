## JHU Practical Machine Learning

## Michael Hunt

## Quiz Four

## August 2015

## QUESTION ONE

# clean the workspace
rm(list=ls())

# Load the vowel.train and vowel.test data sets: 
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

str(vowel.train)
str(vowel.test)

# set y variable to be a factor in both data sets.
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

str(vowel.train)
str(vowel.test)

table(vowel.train$y)
table(vowel.test$y)

## Fit random forest model
library(caret)
set.seed(33833)
RFmodel<-train(y~.,method="rf",data=vowel.train)

# RF prediction on test set
predictRFtest<-predict(RFmodel,newdata=vowel.test)

# RF confusion table
ctRF_test<-table(vowel.test$y, predictRFtest)
ctRF_test

# RF accuracy
RFaccuracy_test<-sum(diag(ctRF_test))/sum(ctRF_test) # testing set
RFaccuracy_test

## Fit GBM model
library(caret)
set.seed(33833)
GBMmodel<-train(y~.,method="gbm",data=vowel.train)

# GBM prediction on test set
predictGBMtest<-predict(GBMmodel,newdata=vowel.test)

# GBM confusion table
ctGBM_test<-table(vowel.test$y, predictGBMtest)
ctGBM_test
ctRF_test # rf table for comparison

# GBM accuracy
GBMaccuracy_test<-sum(diag(ctGBM_test))/sum(ctGBM_test) # testing set
GBMaccuracy_test
RFaccuracy_test # rf accuracy for comparison

#compare the two
qplot(predictRFtest,predictGBMtest,data=vowel.test,color=y,size=12)
df<-data.frame(vowel.test$y,predictRFtest,predictGBMtest)
dfAgree<-subset(df,df[,2]==df[,3])
dfBothCorrect<-subset(dfAgree,dfAgree[,1]==dfAgree[,2])
# accuracy on test samples where both models agree
nrow(dfBothCorrect)/nrow(dfAgree)

## QUESTION TWO

rm(list=ls())

#load Alzheimer's data
library(caret)
library(gbm)
set.seed(3433)
library(AppliedPredictiveModeling)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]
testinga<-testing
testinga$diagnosis<-as.numeric(testing$diagnosis)-1

# Build three different models
set.seed(62433)
mod1 <- train(diagnosis ~.,method="gbm",data=training)
set.seed(62433)
mod2 <- train(diagnosis ~.,method="rf",data=training)
set.seed(62433)
mod3 <- train(diagnosis ~.,method="lda",data=training)

#Stack the predictions together using random forests ("rf"). 
# What is the resulting accuracy on the test set?
# Is it better or worse than each of the individual predictions? 

# Predict on the testing set 
pred1 <- predict(mod1,testing)
pred2 <- predict(mod2,testing)
pred3 <- predict(mod3,testing)

# Accuracies of each model
ctmod1_test<-table(testing$diagnosis, pred1)
accuracy_mod1<-sum(diag(ctmod1_test))/sum(ctmod1_test)
accuracy_mod1

ctmod2_test<-table(testing$diagnosis, pred2)
accuracy_mod2<-sum(diag(ctmod2_test))/sum(ctmod2_test)
accuracy_mod2

ctmod3_test<-table(testing$diagnosis, pred3)
accuracy_mod3<-sum(diag(ctmod3_test))/sum(ctmod3_test)
accuracy_mod3

# stack the models
dfStack<-data.frame(pred1,pred2,pred3,diagnosis=testing$diagnosis)

#fit a model using rf that combines the predictors
combModFit <- train(diagnosis ~.,method="rf",
                    data=dfStack)
combPred <- predict(combModFit,dfStack)

# combined accuracy
ct_comb<-table(testing$diagnosis, combPred)
accuracy_comb<-sum(diag(ct_comb))/sum(ct_comb)
accuracy_comb

accuracy_mod1
accuracy_mod2
accuracy_mod3

## QUESTION THREE - lasso

rm(list=ls())

# load the concrete data
library(caret)
set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
str(concrete)

# neat way to set up formula with lots of predictors
prednames <- names(concrete)[-9]
Form <- formula(paste("CompressiveStrength~", paste(prednames,collapse="+"), sep=""))

rss <- matrix(0, nr=1, nc=3)
colnames(rss) <- c("lm","lasso","lasso2")

getrss <- function(y, yhat) mean((y-yhat)^2)

set.seed(3523)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]


#Set the seed to 233 and fit a lasso model to predict Compressive Strength
library(lasso2)
set.seed(233)


fitl1 <- l1ce(Form, data=training, bound=seq(0.01,1,0.01))
gcvres <- gcv(fitl1)
best.bound <- gcvres[which.min(gcvres[,"gcv"]), "rel.bound"]
fitl1

# inspection of fit suggests cement is last variable to be set to zero as bound
# is reduced.

fitl1 <- l1ce(Form, data=training, bound=best.bound)
yhatlasso <- predict(fitl1, newdata=testing)
rss[1,2] <- getrss(testing$CompressiveStrength, yhatlasso)
fitl1

# now try lasso using caret for comparison
set.seed(233)
library(elasticnet)
fit <- train(Form,method="lasso",data=training)
yhatlasso2 <- predict(fit, newdata=testing)
rss[1,3] <- getrss(testing$CompressiveStrength, yhatlasso2)

rss
fit$finalModel
plot(fit$finalModel)


## QUESTION FOUR - forecasting

rm(list=ls())
# download data if not yet already done so
if(!file.exists("./data/gaData.csv")){
        fileURL<-"https://d396qusza40orc.cloudfront.net/predmachlearn/gaData.csv"
        download.file(fileURL,destfile="./data/gaData.csv")
}


library(lubridate)  # For year() function below
dat = read.csv("./data/gaData.csv")
training = dat[year(dat$date) < 2012,]
testing = dat[(year(dat$date)) > 2011,]
tstrain = ts(training$visitsTumblr) # create time series from training tumblr visits
tstest = ts(testing$visitsTumblr)

str(training)
str(testing)

# Fit a model using the bats() function in the forecast package to the training
# time series.
library(forecast)
fit<-bats(tstrain)
fcast <- forecast(fit,h=length(tstest))

df<-data.frame(fcast[2],fcast[6],fcast[5],testing$visitsTumblr)
df$lower.1<-NULL
df$upper.1<-NULL

nrow(subset(df,testing.visitsTumblr>=lower.2 & testing.visitsTumblr<=upper.2))/nrow(df)

# 96% of points lie within 95% prediction interval


## QUESTION FIVE - support vector machine

rm(list=ls())

set.seed(3523)
library(AppliedPredictiveModeling)
data(concrete)
inTrain = createDataPartition(concrete$CompressiveStrength, p = 3/4)[[1]]
training = concrete[ inTrain,]
testing = concrete[-inTrain,]

library(e1071)

# Set the seed to 325 and fit a support vector machine using the e1071 package
# to predict Compressive Strength using the default settings.
# Predict on the testing set. What is the RMSE? 

set.seed(325)
fit<-svm(CompressiveStrength~.,data=training)
yhat<-predict(fit,testing)

getrmse <- function(y, yhat) sqrt(mean((y-yhat)^2))

round(getrmse(testing$CompressiveStrength,yhat),2)
