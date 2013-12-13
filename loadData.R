#####################################
# The functions are used to load data, get a certain data and plot 
######################################
require(ff)
require(ggplot2)
require(lubridate)

######### Load training data of a device from .R file
loadDevice=function(deviceNo){
  deviceName=paste("Train_Device_",as.character(deviceNo),sep="")
  load(paste("train/",deviceName,".RData",sep=""))
  return(tmp)
}

###################################    
## get training data/test sequences from ff object
## x is the training data
## y is the test data
## num: the specific device/test sequence ID; 0 indicates give a random
#################################### 
   
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

####### Get 20 test sequences that with professed deviceNo
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

###### Add hour feature to the data
getHour=function(time){
  t=hour(time)+minute(time)/60
  return(t) 
} 

timechange=function(dataset){
  dataset$time=as.POSIXct(dataset$T/1000, origin="1970-01-01", tz="GMT")
  dataset$hour=hour(dataset$time)+minute(dataset$time)/60
  return(dataset)
}

################################
### Many plot functions to explore the data
##################################

#### Plot a single sequence
plotdevice=function(dataset,plt=F){
  a=ggplot(data=dataset)
  a=a+geom_line(aes(time,X,colour="X"))
	a=a+geom_point(aes(time,X),size=1)
  a=a+geom_line(aes(time,Y,colour="Y"))
	a=a+geom_point(aes(time,Y),size=1)
  a=a+geom_line(aes(time,Z,colour="Z"))
	a=a+geom_point(aes(time,Z),size=1)
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

#### Plot a 300 point sequence with midpoint as mid in the arg list
plotdevice2=function(dataset,plt=F,mid=0.5){
  datanum=nrow(dataset)
  
  dataplt=dataset[(mid*datanum-150):(mid*datanum+150),]
  a0=ggplot(data=dataplt)
  a=a0+geom_line(aes(time,X),colour="green4")+ylab("X")
	a=a+geom_point(aes(time,X),colour="black",size=0.8)
	b=a0+geom_line(aes(time,Y),colour="red")+ylab("Y")
	b=b+geom_point(aes(time,Y),colour="black",size=0.8)
  c=a0+geom_line(aes(time,Z),colour="blue")+ylab("Z")
	c=c+geom_point(aes(time,Z),colour="black",size=0.8)
  
  c=c+xlab(paste(as.character(range(dataplt$time)[1]),"To",as.character(range(dataplt$time)[2])))
  
  grid.arrange(a,b,c,nrow=3)
  
  if(plt==T){
    png(paste(dataset[,5],".png",sep=""),width=2000,height=1000,res=130)
    arrange.grid(a,b,c,nrow=3)
    dev.off()
  }
}



#### Plot a traindevice and three test sequence that is professed from that device
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


##### Plot three sequences together
plotcompare2 <- function(sample,device1,device2){
  plt1=plotdevice(sample)
  plt2=plotdevice(device1)
  plt3=plotdevice(device2)
  grid.arrange(plt1,plt2,plt3,nrow=3)
}
