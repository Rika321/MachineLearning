setwd('~/cs498ml/hw1')
getwd()
rm(list=ls())
library(caret)
library(klaR)



#import data into wdat and filled up myx and myy and drop any records with missing attributes.
rwdat<-read.csv('processed.cleveland.data', header = FALSE,  na.strings=c("?") )

#remove any rows with attribute "Na"
wdat<- rwdat[complete.cases(rwdat), ]
str(wdat)


myx<-wdat[,-c(14)]
myy<-as.factor(wdat[,14])

tescore<-array(dim=20)
for(i in 1:20){
  #random partition
  cdp<-createDataPartition(y=myy, p=.8, list=FALSE)
 
  #get the training data set    
  trainx<-myx[cdp,]
  trainy<-myy[cdp]
  
  #put them into train function
  model<-train(trainx, trainy, 'nb', trControl=trainControl(method='cv', number=10))
  teclasses<-predict(model,newdata=myx[-cdp,])
  
  #get the confusionMatrix object
  cm <- confusionMatrix(data=teclasses, myy[-cdp])
  
  #get the mean score from the confusionMatrix object 
  tescore[i] <-cm$overall['Accuracy']
}
print(tescore)
print(mean(tescore))
print(sd(tescore))
#it takes a will to execute 

#it takes a while to print the final result

