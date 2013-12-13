###Prapare the data
setwd("/Users/XuWenzhao/Developer/DataSource/Kaggle/")

## Load data and do feature extraction
# source("/Users/XuWenzhao/Developer/kaggle_accelerometer/readData.R")
#library(ff)
#ffload("traindata.ff")
#ffload("testdata.ff")
load("basic.RData")
load("trainFeature_cov_300_2.RData")
load("testFeature2_cov_300_2.RData")

FeatureName=c("V1","xmean","ymean","zmean","xvar","yvar","zvar","Amean","Avar","covxy","covxz","covyz","freX","freY","freZ","freA","eneX","eneY","eneZ","eneA","hour","sampleFreq")
rownames(trainFeature)=NULL
rownames(testFeature)=NULL

######################################
#########    Prepare the data
######################################

trainFeature=as.data.frame(trainFeature)
trainFeature$V1=as.factor(trainFeature$V1)
testFeature=as.data.frame(testFeature)

featureIndex=c(2:21) ### This is the feature index.


#######################################################
## Run Multi-label Classifier
#######################################################
require(class)
### Prepare the data for knn
train.Y=as.factor(trainFeature[,1])
train.X=trainFeature[,featureIndex]
train.X=scale(train.X)
centering=attr(train.X,"scaled:center")
scaling=attr(train.X,"scaled:scale")

test.X=testFeature[,featureIndex] 
test.X=scale(test.X,center=centering,scale=scaling)

### Run Knn
for(knn.k in c(1,5)){
	cv.ratio=NULL
	
	##### Cross validation
	for(i in 1:5){
		## Do a cross validation that every time test on 20% of the data training data set. 
		## Repeat for 5 times.
  		trainNum=nrow(train.X)
  		train.Index=sample(1:trainNum,floor(0.8*trainNum))
  		valid.Index=c(1:trainNum)[-train.Index]
  		train.X_train=train.X[train.Index,]
  		train.Y_train=train.Y[train.Index]
  		valid.X=train.X[valid.Index,]
  		valid.Y=train.Y[valid.Index]
	
  		knn.model.valid=knn(train=train.X_train,test=valid.X,cl=train.Y_train,k=knn.k,use.all=T)
  		right=sum(as.numeric(as.character(valid.Y))==as.numeric(as.character(knn.model.valid)))
  		cv.ratio=c(cv.ratio,right/length(valid.Y))
	}
	
	print("The cross validation accuracy is")
	print(cv.ratio)
	save(cv.ratio,file=paste("MLknnCrossValidation_",knn.k,".RData",sep=""))
	
	##### Do prediction on test data
	knn.model=knn(train=train.X,test=test.X,cl=train.Y,k=5)
	question$PredictDevice=as.numeric(as.character(knn.model))

	### Generate IS True
	question$IsTrue=ifelse(question$QuizDevice==question$PredictDevice,1,0)
	write.csv(question[,c(1,5)],file=paste("answer-knn",knn.k,".csv",sep=""),row.names=F)
}


##################################################
#######  Run 0-1 classifier
##################################################
require("kernlab")
require(MASS)
library(glmnet)
library(gbm)
library(randomForest)

classifierSet_svm=list() # Gaussian kernel
classifierSet_svm2=list() # Linear kernel
classifierSet_rf=list() #Random Forest
classifierSet_lda=list() # Lda
classifierSet_lasso=list() # Lasso
classifierSet_logit=list() # Logistic regression
classifierSet_knn=list() # knn
rfErrorRate=list() # out of bag error in random forest

#trainFeature_08_index=sample(1:nrow(trainFeature),floor(0.8*nrow(trainFeature)))
#trainFeature_02_index=c(1:nrow(trainFeature))[-trainFeature_08_index]

negativeSize=1

for(i in device){
 	print(i)
  	train=trainFeature
  	train$V1=ifelse(train$V1==i,1,0) # Assign label
  
  	index_1=which(train$V1==1)
  	index_0=which(train$V1==0)
  
  	train.new=rbind(train[index_1,],train[sample(index_0,length(index_1)*negativeSize),]) 
  	train.new$V1=as.factor(train.new$V1)
	
	test.new=testFeature[question$QuizDevice==i,]
	save(train.new,file=paste("./trainFeature/trainDataForDevice_",i,".RData",sep=""))
	save(test.new,file=paste("./testFeature/testDataForDevice",i,".RData",sep=""))
	
  ### Different classifier ########
  	train.new=train.new[,c(1,featureIndex)]
  	classifierSet_svm[[i]]=ksvm(V1~.,data=train.new,kernel="rbfdot")
	classifierSet_svm2[[i]]=ksvm(V1~.,data=train.new,kernel="vanilladot")
  	classifierSet_rf[[i]]=randomForest(V1~.,data=train.new,importance=F,ntree=500)
  	classifierSet_lda[[i]]=lda(V1~.,data=train.new)
	classifierSet_lasso[[i]]=cv.glmnet(as.matrix(train.new[,-1]),train.new[,1],family="binomial",alpha=1)
  	classifierSet_logit[[i]]=glm(V1~.,data=train.new,family=binomial,maxit=200)
	#classifierSet_knn[[i]]=train.new # The classifier of knn can be regared as its training set.
	
	rm(train.new)
}

save(classifierSet_svm,classifierSet_rf,classifierSet_lda,classifierSet_lasso,classifierSet_logit,file="classifier.RData")

################################
#### 0-1 Classifier Result
###################################
pred_trainerror <- function(i){
  	print(i)
	dev=i
  	load(paste("./trainFeature/trainDataForDevice_",i,".RData",sep=""))
  	seqs=train.new[,featureIndex]
	model=classifierSet[[dev]]
	
	if(method=="svm"){
		predresult=predict(model,seqs)
		predresult=as.numeric(as.character(predresult))
	}
	else if(method=="lasso"){
		predresult=predict(model,as.matrix(seqs),type="response",s="lambda.min")
		predresult=ifelse(predresult>0.5,1,0)
		predresult=predresult[,1]
	}
	else if(method=="lda"){
		predresult=predict(model,seqs)
		predresult=as.numeric(as.character(predresult$class))
	}
	else if(method=="gbm"){
		predresult=predict(model,seqs,type = "response")
		predresult=ifelse(predresult>0.5,1,0)
	}
	else if(method=="logit"){
		predresult=predict(model,seqs,type="response")
		predresult=ifelse(predresult>0.5,1,0)
	}
	else if(method=="rf"){
		predresult=model$confusion
	}
	else{
		print("unknow method")
		stop()
	}
  return(list(predresult,as.numeric(as.character(train.new[,1]))))
}


pred_test <- function(i){
  print(i)
	dev=i
	load(paste("./testFeature/testDataForDevice",i,".RData",sep=""))
	seqs=test.new[,featureIndex]
	rm(test.new)
	model=classifierSet[[dev]]
	
	if(method=="rf" | method=="svm" ){	
		predresult=predict(model,seqs)
		predresult=as.numeric(as.character(predresult))
	}
	else if(method=="lasso"){
		predresult=predict(model,as.matrix(seqs),type="response",s="lambda.min")
		predresult=ifelse(predresult>0.5,1,0)
		predresult=predresult[,1]
	}
	else if(method=="lda"){
		predresult=predict(model,seqs)
		predresult=as.numeric(as.character(predresult$class)) 
	}
	else if(method=="gbm"){
		predresult=predict(model,seqs,type = "response")
		predresult=ifelse(predresult>0.5,1,0)
	}
	else if(method=="logit"){
		predresult=predict(model,seqs,type="response")
		predresult=ifelse(predresult>0.5,1,0)
	}
	else{
		print("unknow method")
		stop()
	}
  return(predresult)
}


### Summary of train error
trainerror <- function(trainerrordata){
  	correctRatio=c()
  	for(perf in trainerrordata){
  		if(method=="rf"){
  			correctNum=perf[[1]][1]+perf[[1]][4]
  		}else{
  			correctNum=sum(perf[[1]]==perf[[2]])
  		}
	correctRatio=c(correctRatio,correctNum/length(perf[[2]]))
	}
  return(mean(correctRatio))
}


### Summary of test error
testerror <- function(result,filename){
  question$IsTrue=-1
  for(i in 1:length(device)){
    question[question$QuizDevice==device[i],"IsTrue"]=as.numeric(as.character(result[[i]]))
  }
  write.csv(question[,c("QuestionId","IsTrue")],file=filename,row.names=F)
}


trainerrorSet=c(0,0,0,0,0,0,0)

#Do SVM
method <<- "svm"
classifierSet=classifierSet_svm
trainerror_svm=lapply(device,pred_trainerror)
trainerrorSet[1]=trainerror(trainerror_svm)
result_svm=lapply(device,pred_test)
testerror(result_svm,"svm.csv")

classifierSet=classifierSet_svm2
trainerror_svm2=lapply(device,pred_trainerror)
trainerrorSet[2]=trainerror(trainerror_svm2)
result_svm2=lapply(device,pred_test)
testerror(result_svm,"svm2.csv")


#Do random forest
method <<- "rf"
classifierSet=classifierSet_rf
trainerror_rf=lapply(device,pred_trainerror)
trainerrorSet[3]=trainerror(trainerror_rf)
result_rf=lapply(device,pred_test)
testerror(result_rf,"rf.csv")

#Do lda
method <<- "lda"
classifierSet=classifierSet_lda
trainerror_lda=lapply(device,pred_trainerror)
trainerrorSet[4]=trainerror(trainerror_lda)
result_lda=lapply(device,pred_test)
testerror(result_lda,"lda.csv")

#Do Lasso
method <<- "lasso"
classifierSet=classifierSet_lasso
trainerror_lasso=lapply(device,pred_trainerror)
trainerrorSet[5]=trainerror(trainerror_lasso)
result_lasso=lapply(device,pred_test)
testerror(result_lasso,"lasso.csv")


#Do Logit
method <<- "logit"
classifierSet=classifierSet_logit
trainerror_logit=lapply(device,pred_trainerror)
trainerrorSet[6]=trainerror(trainerror_logit)
result_logit=lapply(device,pred_test)
testerror(result_logit,"logit.csv")

#Do KNN
method <<- "knn"
trainerror_knn=list()
result_knn=list()
for(i in 1:length(device)){
	dev=device[i]
	trainerr.knn=c()
	#train.knn=classifierSet_knn[[i]]
	load(paste("./trainFeature/trainDataForDevice_",dev,".RData",sep=""))
	train.knn=train.new
	rm(train.new)
	train.knn.X=train.knn[,featureIndex]
	train.knn.Y=as.factor(train.knn[,1])
	knnCV=knn.cv(train.knn.X,train.knn.Y,k=5)
	trainerror_knn[[i]]=list(as.numeric(as.character(knnCV)),as.numeric(as.character(train.knn.Y)))
	load(paste("./testFeature/testDataForDevice",dev,".RData",sep=""))
	seqs=test.new[,featureIndex]
	result_knn[[i]]=knn(train=train.knn.X,test=seqs,cl=train.knn.Y,k=5)
	rm(seqs)
}

trainerrorSet[7]=trainerror(trainerror_knn)
testerror(result_knn,"knn.csv")






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