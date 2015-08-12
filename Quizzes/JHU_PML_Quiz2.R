## JHU Practical Machine Learning

## Michael Hunt

##Question One

rm(list=ls())
library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

adData = data.frame(diagnosis,predictors)
testIndex = createDataPartition(diagnosis, p = 0.50,list=FALSE)
training = adData[-testIndex,]
testing = adData[testIndex,]


# adData = data.frame(predictors)
# trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
# training = adData[trainIndex,]
# testing = adData[-trainIndex,]

# adData = data.frame(diagnosis,predictors)
# train = createDataPartition(diagnosis, p = 0.50,list=FALSE)
# test = createDataPartition(diagnosis, p = 0.50,list=FALSE)

# adData = data.frame(diagnosis,predictors)
# trainIndex = createDataPartition(diagnosis,p=0.5,list=FALSE)
# training = adData[trainIndex,]
# testing = adData[trainIndex,]

## Question Two

library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(1000)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]]
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

hist(training$Superplasticizer) # highly skewed data!
summary(training$Superplasticizer)
table(training$Superplasticizer)


## Question Three

# preProc <- preProcess(log10(spam[,-58]+1),method="pca",pcaComp=2)
# spamPC <- predict(preProc,log10(spam[,-58]+1))
# plot(spamPC[,1],spamPC[,2],col=typeColor)

library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

# select those predictor vriables in the training set that begin with IL
trainingIL<-training[ , grepl( "^IL" , names( training ) ) ]
preProc <- preProcess(trainingIL,method="pca",thresh=0.8)
preProc$numComp

# result is 7 - so 7 IL predictors of the 12 available are required to explain 80% of the variance.


## Question Four
rm(list=ls())
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
ILpredictors<-predictors[ , grepl( "^IL" , names(predictors) ) ]
adData = data.frame(diagnosis,ILpredictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]


#NO PCA Model
modFitnoPCA <- train(training$diagnosis ~., method="glm", data=training)
confusionMatrix(testing$diagnosis, predict(modFitnoPCA, testing))

#PCA Model
preProc <- preProcess(training[,-1],method="pca",thresh=.8)

# find PCA predictor values for each observation in train and test sets
trainPC <- predict(preProc,training[,-1])
testPC <- predict(preProc,testing[,-1])

#fit glm model to PCA training predictors
PCAFit <- train(training$diagnosis ~ .,method="glm",data=trainPC)
confusionMatrix(testing$diagnosis,predict(PCAFit,testPC))

