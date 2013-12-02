###Prapare the data



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

testFeature=testFeatureExtract(y,sample[1:100])
train.Y=as.factor(trainFeature[,1])
train.X=trainFeature[,-1]

test.X=testFeature[,-1] 
test.seqindex=testFeature[,1]

save(trainFeature,testFeature,file="feature.RData")

### Train the classifier
rf.model=randomForest(x=train.X,y=train.Y,importance=T,ntree=10000)
rf.model=randomForest(train.Y~.,data=traindata,importance=T,ntree=10000)

rf.Yhat=predict(rf.model,test.X)

### Predict

question$PredictDevice=0
question$PredictDevice[1:100]=knn.model


question$PredictDevice[1:100]=as.numeric(as.character(rf.Yhat))
