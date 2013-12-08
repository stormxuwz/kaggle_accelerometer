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
load("basic.RData")
load("trainFeature2_modified.RData")
load("testFeature2_modified.RData")


######################################
#########    Extract the features
######################################

a=Sys.time()
trainFeature=trainFeatureExtract(x,device)
traintime=Sys.time()-a
save(trainFeature,file="trainFeature2_modified.RData")

b=Sys.time()
testFeature=testFeatureExtract(y,0)
testtime=Sys.time()-b
save(testFeature,file="testFeature2_modified.RData")


######################################
#########    Prepare the data
######################################


train.Y=as.factor(trainFeature[,1])

train.X=trainFeature[,2:20]
train.X=scale(train.X)
centering=attr(train.X,"scaled:center")
scaling=attr(train.X,"scaled:scale")

test.X=testFeature[,2:20] 
test.X=scale(test.X,center=centering,scale=scaling)

#######################################################
## Multi-label Classifier
#######################################################
### Run Knn
knn.model=knn(train=train.X,test=test.X,cl=train.Y,k=1)
question$PredictDevice=as.numeric(as.character(knn.model))

### Train the classifier
rf.model=randomForest(x=train.X,y=train.Y,importance=T,ntree=5000)
rf.Yhat=predict(rf.model,test.X)
question$PredictDevice=as.numeric(as.character(rf.Yhat))


## Run ANN
formu_name="V1~V2"
for(i in 3:21) formu_name=paste(formu_name,"+","V",i,sep="")
formu=formula(formu_name)
nn <- neuralnet(formu, data=trainFeature, hidden=2, err.fct="ce", linear.output=FALSE)


###generate IS True
question$IsTrue=ifelse(question$QuizDevice==question$PredictDevice,1,0)
write.csv(question[,c(1,5)],file="answer-lda.csv",row.names=F)




##################################################
#######  Run 0-1 classifier
##################################################
require("kernlab")
require(MASS)
trainFeature=as.data.frame(trainFeature)
trainFeature$V1=as.factor(trainFeature$V1)


testFeatures1=testFeature
testFeatures1=as.data.frame(testFeatures1)
testFeatures1$device=question$QuizDevice
classifierSet=list()

for(i in device){
	train=trainFeature
  print(i)
	train$V1=ifelse(trainFeature$V1==i,1,0)
  
  index_1=which(train$V1==1)
  index_0=which(train$V1==0)
  
  train.new=rbind(train[index_1,],train[sample(index_0,length(index_1)*2),])
	
  
  ### Different classifier ########
  #classifierSet[[i]]=ksvm(V1~.,data=train.new,kernel="vanilladot")
  classifierSet[[i]]=lda(V1~.,data=train.new)

}

pre <- function(i){
  print(i)
	dev=testFeatures1[i,22]
	seq=testFeatures1[i,2:21]
	#model=svmResult[[dev]]
  model=classifierSet[[dev]]
	predresult=predict(model,seq)$class
  return(predresult)
}

result=sapply(1:nrow(testFeature),pre)

question$IsTrue=as.numeric(as.character(result))



#########Write the result 
write.csv(question[,c(1,4)],file="answer-lda.csv",row.names=F)


