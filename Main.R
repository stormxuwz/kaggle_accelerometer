###Prapare the data
library(ff)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(class)


source("/Users/XuWenzhao/Developer/kaggle_accelerometer//Funclist.R")

setwd("/Users/XuWenzhao/Developer/DataSource/Kaggle/")

ffload("traindata.ff")
ffload("testdata.ff")
load("feature.RData")
load("basic.RData")
load("trainFeature.RData")
load("testFeature.RData")

###Extract the features

num.test=10
name_row=c()
traindata=as.data.frame(train.Y)

for(i in 1:20){
  name_row=c(name_row,paste("X",i,sep=""))
  traindata[,paste("X",i,sep="")]=train.X[,i]
}
 

colnames(train.X)=name_row

a=Sys.time()
trainFeature=trainFeatureExtract(x,device)
traintime=Sys.time()-a
save(trainFeature,file="trainFeature.RData")

b=Sys.time()
testFeature=testFeatureExtract(y,0)
testtime=Sys.time()-b
save(testFeature,file="testFeature.RData")



train.Y=as.factor(trainFeature[,1])



train.X=trainFeature[,2:20]
train.X=scale(train.X)
centering=attr(train.X,"scaled:center")
scaling=attr(train.X,"scaled:scale")

test.X=testFeature[,2:20] 
test.X=scale(test.X,center=centering,scale=scaling)

#test.seqindex=testFeature[,1]

#save(trainFeature,testFeature,file="feature.RData")


knn.model=knn(train=train.X,test=test.X,cl=train.Y,k=1)
question$PredictDevice=as.numeric(as.character(knn.model))
question$IsTrue=ifelse(question$QuizDevice==question$PredictDevice,1,0)
write.csv(question[,c(1,5)],file="answer-k1.csv",row.names=F)
### Train the classifier
rf.model=randomForest(x=train.X,y=train.Y,importance=T,ntree=10000)
rf.model=randomForest(train.Y~.,data=traindata,importance=T,ntree=10000)

rf.Yhat=predict(rf.model,test.X)

### Predict

question$PredictDevice=0
question$PredictDevice[1:100]=knn.model


question$PredictDevice[1:100]=as.numeric(as.character(rf.Yhat))
