setwd('~/cs498ml/hw1')
getwd()
rm(list=ls())
library(caret)
library(klaR)

##import data into wdat and filled up bigx and bigy 
wdat<-read.csv('pima-indians-diabetes.data', header = FALSE)
bigx <- wdat[,-c(9)]
bigy <- wdat[,9]

#create array to store testscore and training score 
trscore<-array(dim=20)
tescore<-array(dim=20)

for (wi in 1:20)
{  #random partition with 20% test data and 80% of training data 
  wtd<-createDataPartition(y=bigy, p=.8, list=FALSE)
  
  #get the training data set    
  trainingx<-bigx[wtd, ]
  trainingy<-bigy[wtd]
  
  #divide the training data into positive and negative parts. 
  trainposflag <- trainingy>0
  postivetrainx <- trainingx[trainposflag,]
  negativtrainx <- trainingx[!trainposflag, ]
  
  #calculate the mean and sd for posive train data 
  ptrainmean=sapply(postivetrainx,mean,na.rm=TRUE)
  ptrainsd=sapply(postivetrainx,sd,na.rm=TRUE)
  
  #calculate the offsets, scales and log for postive train data. 
  ptrainoffsets<-t(t(trainingx)-ptrainmean)
  ptrainscales<-t(t(ptrainoffsets)/ptrainsd)
  ptrainlogs<--(1/2)*rowSums(apply(ptrainscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(ptrainsd))
  
  #calculate the mean and sd for negative train data 
  ntrainmean=sapply(negativtrainx,mean,na.rm=TRUE)
  ntrainsd=sapply(negativtrainx,sd,na.rm=TRUE)
  
  #calculate the offsets, scales and log for negative train data.
  ntrainoffsets<-t(t(trainingx)-ntrainmean)
  ntrainscales<-t(t(ntrainoffsets)/ntrainsd)
  ntrainlogs<--(1/2)*rowSums(apply(ntrainscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(ntrainsd))
  
  # classify
  fact = ptrainlogs>ntrainlogs
  right = fact==trainingy
  trscore[wi] = sum(right)/(sum(right)+sum(!right))
  
  
  #get the test data set    
  testx<-bigx[-wtd,]
  testy<-bigy[-wtd ]
  
  #calculate the offsets, scales and log for positive test data
  ptestoffsets<-t(t(testx)-ptrainmean)
  ptestscales<-t(t(ptestoffsets)/ptrainsd)
  ptestlogs<--(1/2)*rowSums(apply(ptestscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(ptrainsd))
  
  #calculate the offsets, scales and log for negative test data
  ntestoffsets<-t(t(testx)-ntrainmean)
  ntestscales<-t(t(ntestoffsets)/ntrainsd)
  ntestlogs<--(1/2)*rowSums(apply(ntestscales,c(1, 2), function(x)x^2), na.rm=TRUE)-sum(log(ntrainsd))
  
  # classify
  factte<-ptestlogs>ntestlogs
  testright<-factte==testy
  tescore[wi]<-sum(testright)/(sum(testright)+sum(!testright))
}
print(tescore)
print(mean(tescore))
print(sd(tescore))
#it takes a while to print the final result
