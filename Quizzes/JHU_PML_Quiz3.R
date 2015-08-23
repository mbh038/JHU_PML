## JHU Practical Machine Learning

## Michael Hunt

## Quiz Three

## August 2015

## Question One

rm(list=ls())

# load cell segmentation data
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

str(segmentationOriginal)

table(segmentationOriginal$Case)

## subset the data according to Case variable
training<-subset(segmentationOriginal,Case=="Train")
testing<-subset(segmentationOriginal,Case=="Test")

nrow(training)
nrow(testing)

# fit a CART model
set.seed(125)
fit <- train( Class~ .,method="rpart",data=testing)

plot(fit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(fit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

## Question Three

rm(list=ls())

# load olive oil data
library(pgmm)
data(olive)
olive = olive[,-1]
str(olive)

# Fit a classification tree where Area is the outcome variable.
set.seed(125)
fit <- train( Area~ .,method="rpart",data=olive)

plot(fit$finalModel, uniform=TRUE, 
     main="Classification Tree")
text(fit$finalModel, use.n=TRUE, all=TRUE, cex=.8)

newdata = as.data.frame(t(colMeans(olive)))

predict(fit,newdata=newdata)

## Question Four

rm(list=ls())

# load South African heart disease data
library(ElemStatLearn)
data(SAheart)
SAheart$chd<-as.factor(SAheart$chd)

#create training and test data sets
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]
nrow(subset(testSA,chd=="1"))

# inspect data
str(SAheart)

# fit logistic regression model
set.seed(13234)
fit<-train(chd~age+alcohol+obesity+tobacco+typea+ldl,data=trainSA,method="glm",family="binomial")

# predict on training and test sets
predictSAtrain<-predict(fit,newdata=trainSA)
predictSAtest<-predict(fit,newdata=testSA)

# confusion tables
ctSA_train<-table(trainSA$chd, predictSAtrain)
ctSA_train
ctSA_test<-table(testSA$chd, predictSAtest)
ctSA_test

# accuracy
SAaccuracy_train<-sum(diag(ctSA_train))/sum(ctSA_train) # training set
SAaccuracy_test<-sum(diag(ctSA_test))/sum(ctSA_test) # testing set

# misclassification rate
1- SAaccuracy_train # training set
1- SAaccuracy_test # testing set

## Question Five

rm(list=ls())

# load libraries
library(caret)

# load data
library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 

# inspect data
str(vowel.train)
str(vowel.test)

summary(vowel.train)
summary(vowel.test)

# convert y to factor
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

#
set.seed(33833)
fit <- train( y~ .,method="rf",data=vowel.train)
varImp(fit)
