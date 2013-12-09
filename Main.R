###Prapare the data
library(ff)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(class)

source("/Users/XuWenzhao/Developer/kaggle_accelerometer/Funclist.R")
setwd("/Users/XuWenzhao/Developer/DataSource/Kaggle/")

ffload("traindata.ff")
ffload("testdata.ff")
load("basic.RData")
load("trainFeature_cov_300.RData")
load("testFeature2_cov_300.RData")

FeatureName=c("V1","xmean","ymean","zmean","xvar","yvar","zvar","Amean","Avar","covxy","covxz","covyz","freX","freY","freZ","freA","eneX","eneY","eneZ","eneA","hour")
rownames(trainFeature)=NULL
rownames(testFeature)=NULL

######################################
#########    Extract the features
######################################

a=Sys.time()
trainFeature=trainFeatureExtract(x,device)
traintime=Sys.time()-a
save(trainFeature,file="trainFeature_cov_300.RData")

b=Sys.time()
testFeature=testFeatureExtract(y,0)
testtime=Sys.time()-b
save(testFeature,file="testFeature2_cov_300.RData")


######################################
#########    Prepare the data
######################################

trainFeature=as.data.frame(trainFeature)
trainFeature$V1=as.factor(trainFeature$V1)
testFeature=as.data.frame(testFeature)

featureIndex=c(2:21)

train.Y=as.factor(trainFeature[,1])

train.X=trainFeature[,featureIndex]
train.X=scale(train.X)
centering=attr(train.X,"scaled:center")
scaling=attr(train.X,"scaled:scale")

test.X=testFeature[,featureIndex] 
test.X=scale(test.X,center=centering,scale=scaling)

#######################################################
## Run Multi-label Classifier
#######################################################
### Run Knn
cv.ratio=NULL

for(i in 1:5){
  trainNum=nrow(train.X)
  train.Index=sample(1:trainNum,floor(0.8*trainNum))
  valid.Index=c(1:trainNum)[-train.Index]
  train.X_train=train.X[train.Index,]
  train.Y_train=train.Y[train.Index]
  valid.X=train.X[valid.Index,]
  valid.Y=train.Y[valid.Index]
	 knn.model.valid=knn(train=train.X_train,test=valid.X,cl=train.Y_train,k=5,use.all=T)
   right=sum(as.numeric(as.character(valid.Y))==as.numeric(as.character(knn.model.valid)))
  cv.ratio=c(cv.ratio,right/length(valid.Y))
}
print(cv.ratio)
knn.model=knn(train=train.X,test=test.X,cl=train.Y,k=5)
question$PredictDevice=as.numeric(as.character(knn.model))

###generate IS True
question$IsTrue=ifelse(question$QuizDevice==question$PredictDevice,1,0)
write.csv(question[,c(1,5)],file="answer-knn5.csv",row.names=F)




##################################################
#######  Run 0-1 classifier
##################################################
require("kernlab")
require(MASS)
library(glmnet)
library(gbm)
library(randomForest)


classifierSet_gbm=list()
parameterSet_gbm=list()

classifierSet_svm=list()
classifierSet_rf=list()
classifierSet_lda=list()
classifierSet_lasso=list()
classifierSet_logit=list()
#trainFeature_08_index=sample(1:nrow(trainFeature),floor(0.8*nrow(trainFeature)))
#trainFeature_02_index=c(1:nrow(trainFeature))[-trainFeature_08_index]
  
train=trainFeature[,c(1,featureIndex)]

for(i in device){
  print(i)
  train=trainFeature[,c(1,featureIndex)]
  train$V1=ifelse(train$V1==i,1,0)
  
  index_1=which(train$V1==1)
  index_0=which(train$V1==0)
  
  train.new=rbind(train[index_1,],train[sample(index_0,length(index_1)),]) 
  #6 is a tuning parameters 
	train.new$V1=as.factor(train.new$V1)
	
  ### Different classifier ########
  classifierSet_svm[[i]]=ksvm(V1~.,data=train.new,kernel="rbfdot")
  classifierSet_rf[[i]]=randomForest(V1~.,data=train.new,importance=F,ntree=500)
  classifierSet_lda[[i]]=lda(V1~.,data=train.new)
	classifierSet_lasso[[i]]=cv.glmnet(as.matrix(train.new[,-1]),train.new[,1],family="binomial",alpha=1)
  classifierSet_logit[[i]]=glm(V1~.,data=train.new,family=binomial,maxit=200)
}

save(classifierSet_svm,classifierSet_rf,classifierSet_lda,classifierSet_lasso,file="classifier.RData")

#### 0-1 results
###################################
pred_trainerror <- function(i){
  dev=i
  seqs=trainFeature[trainFeature$V1==i,featureIndex]
  model=classifierSet[[dev]]
  predresult=predict(model,seqs)
  if(class(model)=="lda")
    predresult=as.numeric(as.character(predresult$class))
  return(predresult)
}

pred_trainerror2 <- function(i){ # for rf and svm
  dev=i
  seqs=trainFeature[trainFeature$V1==i,featureIndex]
  model=classifierSet[[dev]]
  
  if(class(model)[1]=="glm"){
    predresult=predict(model,seqs,type = "response")
  }else{
    predresult=predict(model,as.matrix(seqs),type="response",s="lambda.min")
  }
  predresult=ifelse(predresult>0.5,1,0)
  return(predresult)
}

pred <- function(i){ #for rf and svm
  print(i)
	dev=i
	seqs=testFeature[question$QuizDevice==i,featureIndex]
	model=classifierSet[[dev]]
	predresult=predict(model,seqs)
  if(class(model)=="lda")
    predresult=as.numeric(as.character(predresult$class))
  return(predresult)
}

pred2 <- function(i){ #for gbm and lasso
  dev=i
  seqs=testFeature[question$QuizDevice==i,featureIndex]
  model=classifierSet[[dev]]
  
  if(class(model)[1]=="glm"){
    predresult=predict(model,seqs,type = "response")
  }else{
    predresult=predict(model,as.matrix(seqs),type="response",s="lambda.min")
  }
  predresult=ifelse(predresult>0.5,1,0)
  return(predresult)
}


trainerror <- function(trainerrordata){
  totalerror=c()
  for(error in trainerrordata){
    a=sum(error==0)
    totalerror=c(totalerror,a)
  }
  return(1-sum(totalerror)/nrow(trainFeature)) ### =0.70973333
}



testerror <- function(result,filename){
  question$IsTrue=-1
  for(i in 1:length(device)){
    question[question$QuizDevice==device[i],"IsTrue"]=as.numeric(as.character(result[[i]]))
  }
  write.csv(question[,c("QuestionId","IsTrue")],file=filename,row.names=F)
}





trainerrorSet=c(0,0,0,0,0)
#Do SVM
classifierSet=classifierSet_svm
trainerror_svm=lapply(device,pred_trainerror)
trainerrorSet[1]=trainerror(trainerror_svm)
result_svm=lapply(device,pred)
testerror(result_svm,"svm.csv")

#Do random forest
classifierSet=classifierSet_rf
trainerror_rf=lapply(device,pred_trainerror)
trainerrorSet[2]=trainerror(trainerror_rf)

result_rf=lapply(device,pred)
testerror(result_rf,"rf.csv")

#Do lda
classifierSet=classifierSet_lda
trainerror_lda=lapply(device,pred_trainerror)
trainerrorSet[3]=trainerror(trainerror_lda)

result_lda=lapply(device,pred)
testerror(result_lda,"lda.csv")

#Do Lasso
classifierSet=classifierSet_lasso
trainerror_lasso=lapply(device,pred_trainerror2)
trainerrorSet[4]=trainerror(trainerror_lasso)

result_lasso=lapply(device,pred2)
testerror(result_lasso,"lasso.csv")


#Do Logit
classifierSet=classifierSet_logit
trainerror_logit=lapply(device,pred_trainerror2)
trainerrorSet[5]=trainerror(trainerror_logit)

result_logit=lapply(device,pred2)
testerror(result_logit,"logit.csv")








########discard code ########
# 
# result_gbm=list()
# pred_gbm <- function(i){
#   dev=i
#   seqs=testFeature[question$QuizDevice==i,featureIndex]
#   model=classifierSet_gbm[[dev]]
#   best.iter=parameterSet_gbm[[i]]
#   
#   Yhat=predict(model,seqs,best.iter)
#   Yhat=plogis(Yhat)  ### Convert to p value from logit value
#   Yhat=ifelse(Yhat>0.5,1,0)
#   return(Yhat)
# }
# result_gbm=lapply(device,pred_gbm)



# ### Train the classifier
# rf.model=randomForest(x=train.X,y=train.Y,importance=T,ntree=500)
# rf.Yhat=predict(rf.model,test.X)
# question$PredictDevice=as.numeric(as.character(rf.Yhat))
# 
# 
# ## Run ANN
# formu_name="V1~V2"
# for(i in 3:21) formu_name=paste(formu_name,"+","V",i,sep="")
# formu=formula(formu_name)
# nn <- neuralnet(formu, data=trainFeature, hidden=2, err.fct="ce", linear.output=FALSE)


#gbm.result=gbm(V1~.,data=train.new,n.trees=5000,shrinkage=0.005,bag.fraction=0.5,train.fraction=1,distribution="adaboost",cv.folds = 3)
#classifierSet_gbm[[i]]=gbm.result
#parameterSet_gbm[[i]]=gbm.perf(gbm.result,method="cv")
