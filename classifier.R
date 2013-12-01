classifier <- function(train.X,train.Y,test.X){
	library(MASS)
	library(e1071)
	library(klaR)
	library(tree)
	library(glmnet)
	library(kernlab)
	library(randomForest)
	library(gbm)
	library(class)
	
	## This function is a set of different classifiers
	
	Y=as.factor(traindata$label)
	trainX=traindata[,]
	
	#KNN
	knn.model=knn(trainX,Y,cl=1,k=10)
	
	
	#ANN
	
	
	
	#SVM
	svm.model=ksvm(Y~.,data=traindata,kernel="polydot",kpar=list(degree=2))
	predresult=lapply(dataSets.X,function(X) predict(model,X))
	
	#RandomTree
	rf.model=randomForest(x=train.X,y=train.Y,importance=T,ntree=1000)
	sortedImpt = sort(importance(rf.result,scale = F)[,3], decreasing = T )
	
	predresult=lapply(dataSets.X,function(X) predict(model,X))
	
	#gbm
	
}