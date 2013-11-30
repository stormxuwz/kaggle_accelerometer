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
  deviceNo=trainDevice$Device[1] 
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
	cormatrix=cor(trainDevice[,c("X","Y","Z")])
	corxy=cormatrix[1,2]
	corxz=cormatrix[1,3]
	coryz=cormatrix[2,3]
	
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
	
		### to be determined
	
  feature=c(deviceNo,xmean,ymean,zmean,xvar,yvar,zvar,Amean,Avar,corxy,corxz,coryz,freX,freY,freZ,freA,energX,energY,energZ,energA)
  
  return(feature)
}




TraindataFeature <- function(traindata){
  feature=featureExtract(getDevice(x,device[1]))
  
  for(i in device){
    deviceFeature=featureExtract(getDevice(x,i))
    feature=rbind(feature,deviceFeature)
  }
  return(feature)
}


dataFilter <- function(traindata){
	traindata=traindata[diff(traindata$T)!>1,]
	
	
}


classifier <- function(traindata){}



createED <- function(traindata,range=300) {
	timedifference=diff(traindata$T)
	breaktime=which(timedifference>10*60*1000) #if the difference time is larger than 2 mins, then divided into 2 parts
	seglist=list()
	
	if(length(breaktime)>0){
		periodstart=1
		for(i in 1:length(breaktime)){
			periodfinish=breaktime[i]
			seglist[[i]]=traindata[periodstart:periodfinish,]
			periodstart=periodfinish+1
		}
	}else{
		seglist[[1]]=traindata
	}

	EDlist=list()
	count=0
	for(subdata in seglist){
		no.part=floor(nrow(subdata)/range)
		for (j in 1:no.part)
			count=count+1
			EDlist[[count]]=subdata[(j-1)*300+1:j*300,]
	}

	return(EDlist)
}
