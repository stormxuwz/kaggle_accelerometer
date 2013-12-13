############################################
##### Data filtering and Feature Extraction

## This .R file contains features extraction,
## data splitting, resampling and smoothing methods
#############################################



#############################################
##### The Feature Extraction method, and its 
##### implementation on training data and test data
#############################################

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
	
	#Using cov instead because some sequences might have zero standard deviation
	#cormatrix=cor(trainDevice[,c("X","Y","Z")])
	#corxy=cormatrix[1,2]
	#corxz=cormatrix[1,3]
	#coryz=cormatrix[2,3]
	
	# fourier analysis
	samplefre=mean(diff(trainDevice$T))
	time=diff(range(trainDevice$T))/1000
	
	fftfunc <- function(D,impt){ #D is the time series
	  D_fft=fft(D)    
		A=Mod(D_fft) #A is the amplitude
		A=A[1:(length(A)/2)]
					
		A_sort=sort(A,decreasing=T,index.return=T) # Sort the amplitude 
		dominfre=A_sort$x[2:impt+1]/time	# Dominant frequencies, begin from 2 to ignore the first component (the constant term)
		domineneg=A[A_sort$ix[2:impt+1]]	# Dominant frequency energies
		return(list(dominfre,domineneg))
	}
	
	xfft=fftfunc(trainDevice$X,2)
	yfft=fftfunc(trainDevice$Y,2)
	zfft=fftfunc(trainDevice$Z,2)
	Afft=fftfunc(trainDevice$A,2)
	
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
  feature=c(deviceNo,xmean,ymean,zmean,xvar,yvar,zvar,Amean,Avar,covxy,covxz,covyz,freX,freY,freZ,freA,energX,energY,energZ,energA,time,samplefre)
  
  return(feature)
}

trainFeatureExtract <- function(rawdata,index){
	
	### Load Device 1017 and do features extraction to initial the feature matrix
	### this feature will be removed at the end of this function
  load(paste("/Users/XuWenzhao/Developer/DataSource/Kaggle/Train/Train_Device_",1017,".RData",sep=""))
  featurematrix=featureExtract(timechange(tmp)) 
	rm(tmp)
	##############################
	
	# Begin
  for(i in index){
    print(i)
    a=Sys.time()
    load(paste("/Users/XuWenzhao/Developer/DataSource/Kaggle/Train/Train_Device_",i,".RData",sep=""))
		#dataset=getDevice(rawdata,i) # Read data from ff data object (This method is slower)
    print("finishing loading")
    dataset=timechange(tmp) # Add hour feature
    rm(tmp)
		EDlist=createED(dataset) # Create sequences
    print("finishing splitting")
    print(length(EDlist))
		for(subdata in EDlist){
      subdata=dataFilter(subdata)
			deviceFeature=featureExtract(subdata)
	    featurematrix=rbind(featurematrix,deviceFeature)
		}
    print(Sys.time()-a) #Print the time consumption on dealing with this device
  }
	
	featurematrix=featurematrix[-1,] # Remove the first 1017 device feature
  return(featurematrix)
}

testFeatureExtract <- function(rawdata,index=0){
	#Load a random test sequence and do features extraction to initial the feature matrix
	featurematrix=featureExtract(getDevice(rawdata)) 
	#########
	
	#Begin
  count=nrow(rawdata)/300
  for(i in 1:count){
    print(i)
    dataset=rawdata[((i-1)*300+1):(i*300),]
    dataset=timechange(dataset)
 
    timeInterval=diff(dataset$T)
    timebreak=which(timeInterval>3*60*1000) # If the test sequence has a gap longer than 3 mins, split it.
    maxtimebreak=which.max(timeInterval) # If there are multiple big gaps, find the longest gap and split it
    
    if(length(timebreak)>0){
      data1=dataset[1:maxtimebreak,]
      data2=dataset[(maxtimebreak+1):300,]
      
      if(nrow(data1)>=nrow(data2)) ## Choose the test sequence pieces with more data to replace the original test data
        dataset=data1
      else
        dataset=data2
    }
		
    dataset=dataFilter(dataset)
    deviceFeature=featureExtract(dataset)
    featurematrix=rbind(featurematrix,deviceFeature)
    }
		
    ### The following method is much much slower, do not use. 
	  #for(i in index){
	  #  dataset=getDevice(rawdata,i)
	  #  #dataset=dataFilter(dataset)
	  #  deviceFeature=featureExtract(dataset)
	  #  featurematrix=rbind(featurematrix,deviceFeature)
	  #}
 
	featurematrix=featurematrix[-1,]
  return(featurematrix)
}


##########################################
##### Data resamplling and smoothing methods
###########################################

dataFilter <- function(dataset){
	dataset=dataset[diff(dataset$T)>1,] ### eliminate the points which have multiple values on same sampling time
  timerange=range(dataset$T)
  totaltime=diff(timerange)
  averageSamplingRate=floor(median(diff(dataset$T))) ### Using median valuesto have a bettern estimate of sampling period
  
  newSamplePoint=seq(from=timerange[1],to=timerange[2],by=averageSamplingRate)
  
  lint <- function(axisValue){
    # Linear Interpolation to fill the gap
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
  
	#### Plot the results before and after data filtering process
  #plt1=plotdevice(dataset)
  #plt2=plotdevice(newdata)
  #grid.arrange(plt1,plt2,ncol=2,main=as.character(newdata$Device[1]))
	
	return(newdata)
}


################################
###### Splitting the data set
################################
createED <- function(traindata,range=300,min=2) {
	timedifference=diff(traindata$T)
	#if the difference time is larger than (min) minutes, then divide the data into 2 parts
	breaktime=which(timedifference>min*60*1000) 
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

	
	EDlist=list() ### EDlist is the final sequences that are used. 
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


