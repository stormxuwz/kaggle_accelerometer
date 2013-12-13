require("kernlab")
require(MASS)
library(glmnet)
library(gbm)
library(randomForest)
library(class)

device=c(89)
featureIndex=c(2:21)  # Features Selection



classifierSet_svm=list() # Gaussian kernel
classifierSet_svm2=list() # Linear kernel
classifierSet_rf=list() #Random Forest
classifierSet_lda=list() # Lda
classifierSet_lasso=list() # Lasso
classifierSet_logit=list() # Logistic regression
classifierSet_knn=list()


################################
#### 0-1 Classifier training
###################################
for(i in device){
  load(paste("trainDataForDevice_",i,".RData",sep=""))
  ### Different classifier ########
  train.new=train.new[,c(1,featureIndex)]
  classifierSet_svm[[i]]=ksvm(V1~.,data=train.new,kernel="rbfdot")
  classifierSet_svm2[[i]]=ksvm(V1~.,data=train.new,kernel="vanilladot")
  classifierSet_rf[[i]]=randomForest(V1~.,data=train.new,importance=F,ntree=500)
  classifierSet_lda[[i]]=lda(V1~.,data=train.new)
  classifierSet_lasso[[i]]=cv.glmnet(as.matrix(train.new[,-1]),train.new[,1],family="binomial",alpha=1)
  classifierSet_logit[[i]]=glm(V1~.,data=train.new,family=binomial,maxit=200)
  rm(train.new)
}



pred_trainerror <- function(i){
  print(i)
  dev=i
  load(paste("trainDataForDevice_",i,".RData",sep=""))
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
    predresult=model$confusion  # To calculate OOB error
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
  load(paste("testDataForDevice",i,".RData",sep=""))
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

trainerrorSet=c(0,0,0,0,0,0,0)

#Do SVM
method <<- "svm"
classifierSet=classifierSet_svm
trainerror_svm=lapply(device,pred_trainerror)
trainerrorSet[1]=trainerror(trainerror_svm)
result_svm=lapply(device,pred_test)


classifierSet=classifierSet_svm2
trainerror_svm2=lapply(device,pred_trainerror)
trainerrorSet[2]=trainerror(trainerror_svm2)
result_svm2=lapply(device,pred_test)




#Do random forest
method <<- "rf"
classifierSet=classifierSet_rf
trainerror_rf=lapply(device,pred_trainerror)
trainerrorSet[3]=trainerror(trainerror_rf)
result_rf=lapply(device,pred_test)


#Do lda
method <<- "lda"
classifierSet=classifierSet_lda
trainerror_lda=lapply(device,pred_trainerror)
trainerrorSet[4]=trainerror(trainerror_lda)
result_lda=lapply(device,pred_test)


#Do Lasso
method <<- "lasso"
classifierSet=classifierSet_lasso
trainerror_lasso=lapply(device,pred_trainerror)
trainerrorSet[5]=trainerror(trainerror_lasso)
result_lasso=lapply(device,pred_test)



#Do Logit
method <<- "logit"
classifierSet=classifierSet_logit
trainerror_logit=lapply(device,pred_trainerror)
trainerrorSet[6]=trainerror(trainerror_logit)
result_logit=lapply(device,pred_test)


#Do KNN
method <<- "knn"
trainerror_knn=list()
result_knn=list()
for(i in 1:length(device)){
  dev=device[i]
  trainerr.knn=c()
  #train.knn=classifierSet_knn[[i]]
  load(paste("trainDataForDevice_",dev,".RData",sep=""))
  train.knn=train.new
  rm(train.new)
  train.knn.X=train.knn[,featureIndex]
  train.knn.Y=as.factor(train.knn[,1])
  knnCV=knn.cv(train.knn.X,train.knn.Y,k=5)
  trainerror_knn[[i]]=list(as.numeric(as.character(knnCV)),as.numeric(as.character(train.knn.Y)))
  load(paste("testDataForDevice",dev,".RData",sep=""))
  seqs=test.new[,featureIndex]
  result_knn[[i]]=knn(train=train.knn.X,test=seqs,cl=train.knn.Y,k=5)
  rm(seqs)
}

trainerrorSet[7]=trainerror(trainerror_knn)



finalResult=data.frame(svm_gaus=result_svm[[i]],svm_lin=result_svm2[[i]],rf=result_rf[[i]],
                       lda=result_lda[[i]],lasso=result_lasso[[i]],logit=result_logit[[i]],knn=result_knn[[i]])

trainerrorSet=data.frame(svm_gaus=trainerrorSet[1],svm_lin=trainerrorSet[2],rf=trainerrorSet[3],lda=trainerrorSet[4],lasso=trainerrorSet[5],logit=trainerrorSet[6],knn=trainerrorSet[7])



print("Training Accuracy")
trainerrorSet

print("Final Prediction Answer")
head(finalResult)
