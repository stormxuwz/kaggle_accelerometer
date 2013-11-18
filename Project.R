library("ff")
library(lubridate)
library(ggplot2)
library(gridExtra)

setwd("/Users/XuWenzhao/Documents/Developer/kaggle_accelerometer")
ffload("data")

loadDevice=function(deviceNo){
	deviceName=paste("Train_Device_",as.character(deviceNo),sep="")
	load(paste("train/",deviceName,".RData",sep=""))
	return(tmp)
}

getDevice=function(dataset,num=0){
  no.range=unique(dataset[,5])
  if(num==0){
    num=sample(no.range,1)
  }
  tmp=dataset[dataset[,5]==num,1:5]
	tmp=timechange(tmp)
  #print(nrow(tmp))
  return(tmp)
}
 
timechange=function(dataset){
  getHour=function(time){
    t=hour(time)+minute(time)/60
    return(t) }
  
  dataset$time=as.POSIXct(dataset$T/1000, origin="1970-01-01", tz="GMT")
  #dataset$hour=sapply(dataset$time,getHour)
  return(dataset)
}

#ti=sapply(tmp[,5],getHour)
plotdevice=function(dataset,plt=F){
  a=ggplot(data=dataset)
  a=a+geom_point(aes(time,X,colour="X"),size=1)
  a=a+geom_point(aes(time,Y,colour="Y"),size=1)
  a=a+geom_point(aes(time,Z,colour="Z"),size=1)
  a=a+ylab("Acceleration")+xlab(paste(as.character(range(dataset$time)[1]),"To",as.character(range(dataset$time)[2])))
  a=a+scale_colour_manual("", breaks = c("X", "Y", "Z"),values = c("green4", "red", "blue"))
  a=a+theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"))
  
	if(plt==T){
		png(paste(dataset[,5],".png",sep=""),width=2000,height=500,res=130)
	  print(a)
	  dev.off()
	}
	return(a)
}

plotdevice2=function(dataset,plt=F,mid=0.5){
	datanum=nrow(dataset)
	
	dataplt=dataset[(mid*datanum-250):(mid*datanum+250),]
	a0=ggplot(data=dataplt)
  a=a0+geom_point(aes(time,X),colour="green4")+ylab("X")
	b=a0+geom_point(aes(time,Y),colour="red")+ylab("Y")
	c=a0+geom_point(aes(time,Z),colour="blue")+ylab("Z")
	
  c=c+xlab(paste(as.character(range(dataplt$time)[1]),"To",as.character(range(dataplt$time)[2])))
 
 	grid.arrange(a,b,c,nrow=3)
 
	if(plt==T){
		png(paste(dataset[,5],".png",sep=""),width=2000,height=1000,res=130)
		arrange.grid(a,b,c,nrow=3)
	  dev.off()
	}
}


getSampleFromDevice=function(deviceNo){
  seqIndex=question[question$QuizDevice==deviceNo,2]
  seqlist=list()
  for (i in sample(seqIndex,20)){
    tmp=y[y[,5]==i,1:5]
    tmp=timechange(tmp)
    seqlist[[as.character(i)]]=tmp
  }
  
  return(list(seqIndex,seqlist))
}


plotCompare=function(deviceNo){
	seqIndex=question[question$QuizDevice==deviceNo,2]
	num=sample(seqIndex,3)
	
	devicedata=getDevice(x,deviceNo)
	devicedata=timechange(devicedata)
	
	tmp1=y[y[,5]==num[1],1:5]
  tmp1=timechange(tmp1)
	
	tmp2=y[y[,5]==num[2],1:5]
  tmp2=timechange(tmp2)
	
	tmp3=y[y[,5]==num[3],1:5]
  tmp3=timechange(tmp3)
	
	deviceplt=plotdevice(devicedata)
	sampleplt1=plotdevice(tmp1)
	sampleplt2=plotdevice(tmp2)
	sampleplt3=plotdevice(tmp3)
	
	grid.arrange(deviceplt,sampleplt1,sampleplt2,sampleplt3,nrow=2,ncol=2)
}





for(i in sample(device,10)){
  tmp=getDevice(x,i)
  tmp=timechange(tmp)
  plotdevice(tmp)
}

for(i in sample(dd$SequenceId,10)){
  tmp=getDevice(y,i)
  tmp=timechange(tmp)
  print(range(tmp$time))
  plotdevice(tmp)
}



### Reading the data ####

##Reading the train data ###
x<- read.csv.ffdf(file="train.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)
no.device=x[,5]
device=unique(no.device)

traindata=list()
for(i in device){
	print(i)
	deviceName=paste("Train_Device",as.character(i),sep="_")
	tmp=x[x[,5]==i,1:5]
	tmp=timechange(tmp)
	traindata[[deviceName]]=tmp
	save(tmp,file=paste("train/",deviceName,".RData",sep=""))
}

save(traindata,file="traindata.RData")

ffsave(x, dir=getwd())
ffsave(y, dir=getwd())


##Reading the testing data
y<- read.csv.ffdf(file="test.csv", header=TRUE, VERBOSE=TRUE, first.rows=10000, next.rows=50000, colClasses=NA)
no.sample=y[,5]
sample=unique(no.sample)

testdata=list()

for(i in sample){
  print(i)
  sampleName=paste("Test_Sample",as.character(i),sep="_")
  tmp=y[y[,5]==i,1:5]
  tmp=timechange(tmp)
  #testdata[[sampleName]]=tmp
  #deviceList[[as.character(i)]]=tmp
  save(tmp,file=paste("test/",sampleName,".RData",sep=""))
}

save(testData,file="testdata.Rdata")



question=read.csv("questions.csv",header=T)

