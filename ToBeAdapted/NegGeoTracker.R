TrackerClass.NegGeo.Old<-function(test,id,parameters){
  if (!is.numeric(test) || !is.numeric(id))
    stop("invalid arguments")
  st<-paste("Out_",test,"_T",id,sep="")
  data<-NA
  found=0
  if(exists(st,where=1)) {
    data<-get(st)  
    found<-1
    if(AreParametersEqual(parameters,data$Parameters)==FALSE)
      data<-Tracker.ChangeParameterObject(data,parameters)
  }
  if(found==0) {
    cdir<-paste("./DDropData/",sep="")
    fname<-paste("Out_",test,".csv",sep="")
    file <- list.files(path=cdir,pattern = fname)        
    if(length(file)!=1) {
      cat("Error reading file!")
      flush.console()        
    }
    else {      
      the.file<-paste(cdir,file,sep="")
      tmp<-read.csv(the.file,header=TRUE)   
      tmp<-subset(tmp,tmp$ObjectID==id)
      start<-tmp$MSec[1]
      Minutes<-tmp$MSec-start
      Minutes<-Minutes/(1000*60)
      tmp$Y<-tmp$Y * -1.0 ## flip the Y axis for NegGeo studies
      tmp<-data.frame(tmp,Minutes)
      data=list(Chamber=test,ID=id,Parameters=parameters,RawData=tmp)
      class(data)="TrackingObject"
      assign(st,data,pos=1)  
      data<-Tracker.Calculate.Sleep(data)     
    }
  }
  data 
}


TrackerClass.NegGeo<-function(test,id,parameters){
  if (!is.numeric(test) || !is.numeric(id))
    stop("invalid arguments")
  st<-paste("Out_",test,"_T",id,sep="")
  data<-NA
  found=0
  if(exists(st,where=1)) {
    data<-get(st)  
    found<-1
    if(AreParametersEqual(parameters,data$Parameters)==FALSE)
      data<-Tracker.ChangeParameterObject(data,parameters)
  }
  if(found==0) {
    cdir<-paste("./",sep="")
    fname<-paste("Out_",test,".csv",sep="")
    file <- list.files(path=cdir,pattern = fname)        
    if(length(file)!=1) {
      cat("Error reading file!")
      flush.console()        
    }
    else {      
      the.file<-paste(cdir,file,sep="")
      tmp<-read.csv(the.file,header=TRUE)   
      tmp<-subset(tmp,tmp$ObjectID==id)
      start<-tmp$MSec[1]
      Minutes<-tmp$MSec-start
      Minutes<-Minutes/(1000*60)
      tmp$Y<-tmp$Y * -1.0 ## flip the Y axis for NegGeo studies
      tmp<-data.frame(tmp,Minutes)
      data=list(Chamber=test,ID=id,Parameters=parameters,RawData=tmp)
      class(data)="TrackingObject"
      assign(st,data,pos=1)  
      data<-Tracker.Calculate.Sleep(data)     
    }
  }
  data 
}
