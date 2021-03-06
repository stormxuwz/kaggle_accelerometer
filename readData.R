source("/Users/XuWenzhao/Developer/kaggle_accelerometer/loadData.R")
source("/Users/XuWenzhao/Developer/kaggle_accelerometer/feature.R")

setwd("/Users/XuWenzhao/Developer/DataSource/Kaggle/")


#load("trainFeature.RData")
#load("testFeature.RData")

################################################
####### Reading the data 
################################################

# Reading the train data ###
x<- read.csv.ffdf(file="/Users/XuWenzhao/Developer/DataSource/Kaggle/train.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)
no.device=x[,5]
device=unique(no.device)
# Reading the testing data
y<- read.csv.ffdf(file="/Users/XuWenzhao/Developer/DataSource/Kaggle/test.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)
no.sample=y[,5]
sample=unique(no.sample)
# Reading questions and basic information
question=read.csv("questions.csv",header=T)
no.device=x[,5]
device=unique(no.device)
no.sample=y[,5]
sample=unique(no.sample)
# Store the x, and y
ffsave(x, file="traindata.ff")
ffsave(y, file="testdata.ff")
save(no.device,device,no.sample,sample,question,file="basic.RData")


## Save all the training device seperately
traindata=list()
testdata=list()

for(i in device){
	print(i)
	deviceName=paste("Train_Device",as.character(i),sep="_")
	tmp=x[x[,5]==i,1:5]
	tmp=timechange(tmp)
	#traindata[[deviceName]]=tmp
	save(tmp,file=paste("train/",deviceName,".RData",sep=""))
}

### The following data save the test sequences, which is not feasible since their 
for(i in ){
  print(i)
  sampleName=paste("Test_Sample",as.character(i),sep="_")
  tmp=y[y[,5]==i,1:5]
  tmp=timechange(tmp)
  #testdata[[sampleName]]=tmp
  save(tmp,file=paste("test/",sampleName,".RData",sep=""))
}


save(seqs,file=paste("testfeatureForDevice",i,".RData",sep=""))




######################################
#########    Extract the features
######################################

ffload("traindata.ff")
ffload("testdata.ff")
#load("feature.RData")
load("basic.RData")

a=Sys.time()
trainFeature=trainFeatureExtract(x,device)
traintime=Sys.time()-a
save(trainFeature,file="trainFeature_cov_400.RData")
print(traintime)

b=Sys.time()
testFeature=testFeatureExtract(y)
testtime=Sys.time()-b
save(testFeature,file="testFeature_cov_400.RData")
print(testtime)




################################################
### Random select device to explore the data set
################################################

#for(i in sample(device,10)){
#  tmp=getDevice(x,i)
#  tmp=timechange(tmp)
#  plotdevice(tmp)
#}

#for(i in sample(dd$SequenceId,10)){
#  tmp=getDevice(y,i)
#  tmp=timechange(tmp)
#  print(range(tmp$time))
#  plotdevice(tmp)
#}



### Code/function that are not used ########
### Find the nearest time period
simiTime <- function(testSample) {
	professedDevice=question[question$SequenceId==testSample$SequenceId[1],3]
	
	testSample$hour=sapply(testSample$time,getHour)
	trainset=getDevice(x,professedDevice)
	trainset$hour=sapply(trainset$time,getHour)
	
	timeRange=c(testSample$hour[1]-0.5,testSample$hour[300]+0.5)
	
	if(timeRange[1]<0)
		timeRange[1]=24+timeRange[1]
	if(timeRange[2]>24)
		timeRange[2]=timeRange[2]-24
	
	dataSet=trainset[trainset$hour>timeRange[1] & trainset$hour<timeRange[2],]
	
	plotdevice(dataSet)
}