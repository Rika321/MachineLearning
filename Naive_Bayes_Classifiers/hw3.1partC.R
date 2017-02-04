setwd('~/cs498ml/hw1')
getwd()
rm(list=ls())
library(caret)
library(klaR)

#import data into wdat and filled up myx and myy 
wdat<-read.csv('pima-indians-diabetes.data', header = FALSE)
myx<-wdat[,-c(9)]
myy<-as.factor(wdat[,9])

#random partition with 20% test data and 80% of training data 
cdp<-createDataPartition(y=myy, p=.8, list=FALSE)

#get the training data set    
trainx<-myx[cdp,]
trainy<-myy[cdp]
model<-train(trainx, trainy, 'nb', trControl=trainControl(method='cv', number=10))
teclasses<-predict(model,newdata=myx[-cdp,])
confusionMatrix(data=teclasses, myy[-cdp])

