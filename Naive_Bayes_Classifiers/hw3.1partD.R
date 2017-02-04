setwd('~/cs498ml/hw1')
getwd()
rm(list=ls())
library(caret)
library(klaR)

#import data into wdat and filled up myx and myy 
wdat<-read.csv('pima-indians-diabetes.data', header = FALSE)

#declare an array to store the results. 
score<-array(dim=20)

for (wi in 1:20)
{ 
  myx<-wdat[,-c(9)]
  myy<-as.factor(wdat[,9])
  #random partition with 20% test data and 80% of training data 
  cdp<-createDataPartition(y=myy, p=.8, list=FALSE)
  
  #use svm to process the data 
  svm<-svmlight(myx[cdp,], myy[cdp], pathsvm='~/cs498ml/svm_light_linux64/')
  labels<-predict(svm, myx[-cdp,])
  foo<-labels$class
  
  #calculate the score. 
  score[wi]<- sum(foo==myy[-cdp])/(sum(foo==myy[-cdp])+sum(!(foo==myy[-cdp])))
}

#print the reuslt
print(score)
print(mean(score))
print(sd(score))
