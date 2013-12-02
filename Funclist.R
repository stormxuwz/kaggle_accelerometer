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

getHour=function(time){
  t=hour(time)+minute(time)/60
  return(t) 
} 

timechange=function(dataset){
  dataset$time=as.POSIXct(dataset$T/1000, origin="1970-01-01", tz="GMT")
  dataset$hour=hour(dataset$time)+minute(dataset$time)/60
  return(dataset)
}

#ti=sapply(tmp[,5],getHour)
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


featureExtract <- function(trainDevice) {
  deviceNo=trainDevice[1,5]
  trainDevice$A=sqrt(trainDevice$X^2+trainDevice$Y^2+trainDevice$Z^2)
	
	# mean and variance of each component and the net value
	xmean=mean(trainDevice$X)
  ymean=mean(trainDevice$Y)
  zmean=mean(trainDevice$Z)
  xvar=var(trainDevice$X)
  yvar=var(trainDevice$Y)
  zvar=var(trainDevice$Z)
  Amean=mean(trainDevice$A)
	Avar=var(trainDevice$A)
	
	# correlation of each component
	covmatrix=cov(trainDevice[,c("X","Y","Z")])
	covxy=covmatrix[1,2]
	covxz=covmatrix[1,3]
	covyz=covmatrix[2,3]
	
	#cormatrix=cor(trainDevice[,c("X","Y","Z")])
	#corxy=cormatrix[1,2]
	#corxz=cormatrix[1,3]
	#coryz=cormatrix[2,3]
	
	# fourier analysis
	samplefre=mean(diff(trainDevice$T))
	time=diff(range(trainDevice$T))/1000
	
	fftfunc <- function(D,impt){ #D is the time series, crit is threhold. i.e. crit=0.1 means ignore the frequencies with below 10% amplitude.
		D_fft=fft(D)
		A=Mod(D_fft) #A is the amplitude
		A=A[1:(length(A)/2)]
					
		A_sort=sort(A,decreasing=T,index.return=T) # Sort the amplitude 
		dominfre=A_sort$x[2:impt+1]/time	# ignore the first component
		domineneg=A[A_sort$ix[2:impt+1]]	
		return(list(dominfre,domineneg))
	}
	
	xfft=fftfunc(trainDevice$X,4)
	yfft=fftfunc(trainDevice$Y,4)
	zfft=fftfunc(trainDevice$Z,4)
	Afft=fftfunc(trainDevice$A,4)
	
	freX=mean(xfft[[1]])
	freY=mean(yfft[[1]])
	freZ=mean(zfft[[1]])
	freA=mean(Afft[[1]])
	
	energX=mean(xfft[[2]])
	energY=mean(yfft[[2]])
	energZ=mean(zfft[[2]])
	energA=mean(Afft[[2]])
	
	# time dependent special features
	time=mean(trainDevice$hour)
	
		### to be determined
	
  feature=c(deviceNo,xmean,ymean,zmean,xvar,yvar,zvar,Amean,Avar,covxy,covxz,covyz,freX,freY,freZ,freA,energX,energY,energZ,energA,time)
  
  return(feature)
}


trainFeatureExtract <- function(rawdata,index){
  load(paste("/Users/XuWenzhao/Developer/DataSource/Kaggle/Train/Train_Device_",1017,".RData",sep=""))
  featurematrix=featureExtract(timechange(tmp))  ##initial Format
	rm(tmp)
  for(i in index){
    print(i)
    a=Sys.time()
    load(paste("/Users/XuWenzhao/Developer/DataSource/Kaggle/Train/Train_Device_",i,".RData",sep=""))
		#dataset=getDevice(rawdata,i)
    print("finishing loading")
    dataset=timechange(tmp)
    rm(tmp)
		EDlist=createED(dataset)
    print("finishing splitting")
    print(length(EDlist))
		for(subdata in EDlist){
      subdata=dataFilter(subdata)
			deviceFeature=featureExtract(subdata)
	    featurematrix=rbind(featurematrix,deviceFeature)
		}
    print(Sys.time()-a)
  }
	
	featurematrix=featurematrix[-1,]
  return(featurematrix)
}

testFeatureExtract <- function(rawdata,index){
	featurematrix=featureExtract(getDevice(rawdata))  ##initial Format
	
  if(index==0){
    count=nrow(rawdata)/300
    for(i in 1:count){
      print(i)
      dataset=rawdata[((i-1)*300+1):(i*300),]
      dataset=timechange(dataset)
   
      timeInterval=diff(dataset$T)
      timebreak=which(timeInterval>3*60*1000)
      maxtimebreak=which.max(timeInterval)
      
      if(length(timebreak)>0){
        data1=dataset[1:maxtimebreak,]
        data2=dataset[(maxtimebreak+1):300,]
        
        if(nrow(data1)>=nrow(data2))
          dataset=data1
        else
          dataset=data2
      }
			
      dataset=dataFilter(dataset)
      deviceFeature=featureExtract(dataset)
      featurematrix=rbind(featurematrix,deviceFeature)
    }
	}else{
	  for(i in index){
	    dataset=getDevice(rawdata,i)
	    #dataset=dataFilter(dataset)
	    deviceFeature=featureExtract(dataset)
	    featurematrix=rbind(featurematrix,deviceFeature)
	  }
	}
 
	featurematrix=featurematrix[-1,]
  return(featurematrix)
}


dataFilter <- function(dataset){
	dataset=dataset[diff(dataset$T)>1,]
  timerange=range(dataset$T)
  totaltime=diff(timerange)
  averageSamplingRate=floor(median(diff(dataset$T)))
  
  newSamplePoint=seq(from=timerange[1],to=timerange[2],by=averageSamplingRate)
  
  lint <- function(axisValue){
    # Linear Interpolation
    newvalue=approx(x=dataset$T,y=axisValue,xout=newSamplePoint)$y
    # 5-point smoothing
    ma5 = c(1, 1, 1, 1, 1)/5
    smoothvalue=filter(newvalue,ma5)
    naIndex=which(is.na(smoothvalue)==T)
    n=length(smoothvalue)
    iniValue=smoothvalue[3]
    endValue=smoothvalue[n-2]
    
    smoothvalue[1:2]=iniValue
    smoothvalue[(n-1):n]=endValue
    
    return(as.numeric(smoothvalue))
  }
  
  newX=lint(dataset$X)
  newY=lint(dataset$Y)
  newZ=lint(dataset$Z)
  
  newdata=data.frame(T=newSamplePoint)
  newdata$X=newX
  newdata$Y=newY
  newdata$Z=newZ
	newdata$Device=dataset[1,5]
	
  newdata=timechange(newdata)
  
  #plt1=plotdevice(dataset)
  #plt2=plotdevice(newdata)
  #grid.arrange(plt1,plt2,ncol=2)
  
	return(newdata)
}


createLabel <- function(traindata){
	# This is the function is to create the label of dataset. Instead of use multi class data set, we can use two data set.
	# To be determined	
}



createED <- function(traindata,range=400) {
	timedifference=diff(traindata$T)
	breaktime=which(timedifference>2*60*1000) #if the difference time is larger than 2 mins, then divided into 2 parts
	breaktime=breaktime
  seglist=list()
	breaktime=c(0,breaktime,nrow(traindata))
  
	if(length(breaktime)>2){
		for(i in 1:(length(breaktime)-1)){
      periodstart=breaktime[i]+1
			periodfinish=breaktime[i+1]
			seglist[[i]]=traindata[periodstart:periodfinish,]
		}
	}else{
		seglist[[1]]=traindata
	}

	EDlist=list()
	count=0
	for(subdata in seglist){
		no.part=floor(nrow(subdata)/range)
		if(no.part>0){
		  for (j in 1:no.part){
		    count=count+1
		    EDlist[[count]]=subdata[((j-1)*range+1):(j*range),]
		  }
		}
	}
	return(EDlist)
}


plotcompare2 <- function(sample,device1,device2){
  plt1=plotdevice(sample)
  plt2=plotdevice(device1)
  plt3=plotdevice(device2)
  grid.arrange(plt1,plt2,plt3,nrow=3)
}

