library(RMySQL)
library(ggplot2)
library(lubridate)
library(grid)
#library(gridExtra)
library(scales)
setwd("~/Dropbox/Courses/STAT 542 Statistical Learning/STAT 542 Project")
# 
#question <- read.csv("questions.csv",header = TRUE)
mydb <- dbConnect(MySQL(),user = "root",host = "localhost",dbname = "kaggle")

# number of records for each device
#rs <- dbSendQuery(mydb, statement = "select device, count(1) count from rawtrain group by device order by count asc")
#device_count = fetch(rs, n = -1)
#IDlist <- device_count$device
#hist(device_count$count)

# get data by deviceId
get_by_device<- function(deviceId){
    deviceData <- dbSendQuery(mydb, statement = paste("select * from rawtrain where device = ",deviceId,";",sep=""))
    retrieved <- fetch(deviceData,n=-1)
    return (retrieved)
}
# get data by sequenceId
get_by_sequence<- function(SequenceId){
    SequenceData <- dbSendQuery(mydb, statement = paste("select * from rawtest where SequenceId = ",SequenceId,";",seq=""))
    retrieved <- fetch(SequenceData,n=-1)
    return (retrieved)
}

# write to files according to device ID
write_to_file <- function(deviceID){
    retrieved <- get_by_device(deviceID);
    write.table(retrieved,paste("./devices/",deviceID,".csv",sep=""))
}


# transform UNIX timestamp to readable.
Unix_to_GMT <- function(dataframe){
    dataframe$Time = as.numeric(dataframe$Time)/1000;
    dataframe$Time=as.POSIXct(dataframe$Time, origin="1970-01-01", tz="GMT")
    return(dataframe)
}

plotdevice=function(Id,training){
    if (training){
        dataset<-get_by_device(Id)
        dataset<-Unix_to_GMT(dataset)
        a=ggplot(data=dataset)
        a=a+geom_point(aes(Time,X,colour="X"),size=1)
        a=a+geom_point(aes(Time,Y,colour="Y"),size=1)
        a=a+geom_point(aes(Time,Z,colour="Z"),size=1)
        #a=a+scale_x_datetime(breaks = date_breaks("1 sec"))
        a=a+ylab("Acceleration")+xlab(paste(as.character(range(dataset$Time)[1]),"To",as.character(range(dataset$Time)[2])))
        a=a+scale_colour_manual("", breaks = c("X", "Y", "Z"),values = c("green4", "red", "blue"))
        a=a+theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"))
        a=a+ggtitle(paste("Training data for device ",Id,sep=""))
        # need to print first to output to .png
        #png(paste("train_",dataset[,5],".png",sep=""),width=2000,height=500,res=130)
        #ggsave(paste("train_",dataset[,5],".png",sep=""))
        #print(a)
        return(a)
        #dev.off()
    }
    else {
        dataset<-get_by_sequence(Id)
        dataset<-Unix_to_GMT(dataset)
        a=ggplot(data=dataset)
        a=a+geom_point(aes(Time,X,colour="X"),size=1)
        a=a+geom_point(aes(Time,Y,colour="Y"),size=1)
        a=a+geom_point(aes(Time,Z,colour="Z"),size=1)
        #a=a+scale_x_datetime(breaks = date_breaks("1 sec"))
        a=a+ylab("Acceleration")+xlab(paste(as.character(range(dataset$Time)[1]),"To",as.character(range(dataset$Time)[2])))
        a=a+scale_colour_manual("", breaks = c("X", "Y", "Z"),values = c("green4", "red", "blue"))
        a=a+theme(axis.text=element_text(size=12,face="bold"),axis.title=element_text(size=14,face="bold"))
		a=a+scale_x_datetime(breaks = date_breaks("1 sec"))
        a=a+ggtitle(paste("Testing Sequence ",Id,sep=""))
        #png(paste("test_",dataset[,5],".png",sep=""),width=2000,height=500,res=130)
        #ggsave(paste("train_",Id,"_",dataset[,5],".png",sep=""))
        #print(a)
        return(a)
        #dev.off() 
    }
}


# for exploratary data analysis
plot_compare <- function(deviceId){
    getSeq <- dbSendQuery(mydb, statement = paste("select SequenceId from question where QuizDevice =",deviceId," limit 5",";",sep=""))
    
    SequenceId<-fetch(getSeq,n=-1)
    png(paste("Device ",deviceId,".png",sep=""))
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(2, 2)))
    vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
    plot1 <- plotdevice(deviceId,T)
    plot2 <- plotdevice(SequenceId[1,1],F)
    plot3 <- plotdevice(SequenceId[2,1],F)
    plot4 <- plotdevice(SequenceId[3,1],F)
    print(plot1,vp=vplayout(1,1))
    print(plot2,vp=vplayout(1,2))
    print(plot3,vp=vplayout(2,1))
    print(plot4,vp=vplayout(2,2))
    #grid.arrange(plot1,plot2,plot3,plot4,nrow=2,ncol=2)
    
    dev.off()
}