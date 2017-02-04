setwd('~/cs498ml/hw1')
getwd()
rm(list=ls())
library(caret)
library(klaR)



#import data into wdat and filled up myx and myy and drop any records with missing attributes.
rwdat<-read.csv('processed.cleveland.data', header = FALSE,  na.strings=c("NA","NaN", "?") )


#make sure 14th data are quantized into 2 classes, ruduce the problem to hw3.1 parta. 
for (i in c(14))
{
  vw<-rwdat[, i]>0
  rwdat[vw, i]=1
}

bigx <- rwdat[,-c(14)]
bigy <- rwdat[,c(14)]


#create array to store testscore and training score 
trscore<-array(dim=20)
tescore<-array(dim=20)

for (wi in 1:20)
{  
  #random partition with 15% test data and 85% of training data 
  wtd<-createDataPartition(y=bigy, p=.85, list=FALSE)
  
  #get the training data set    
  trainingx<-bigx[wtd, ]
  trainingy<-bigy[wtd]
  
  #divide the training data into positive and negative parts. 
  zeroflag <- trainingy==0
  posflag <- trainingy>0
  postivetrainx <- trainingx[posflag,]
  negativtrainx <- trainingx[zeroflag,]
  
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

