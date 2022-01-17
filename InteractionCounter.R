require(data.table)
require(reshape2)
require(readxl)
require(tibble)
require(plyr)
require(dplyr)
require(tidyr)


GetInteractionResults<-function(ic,p,interaction.cutoff.mm=8){
  counts <- ic %>% group_by(Frame) %>% summarise(Blobs = length(Frame))
  missings<-length(which(!(seq(min(counts$Frame), max(counts$Frame)) %in% counts$Frame)))
  ones<-sum(counts$Blobs==1)
  twos<-sum(counts$Blobs==2)
  more<-sum(counts$Blobs>2)

  frequencies<-c(missings,ones,twos,more)
  names(frequencies)<-c("Zero","One","Two","More")
  
  frameIndex <- subset(counts,Blobs==2)$Frame
  distances<-rep(NA,length(frameIndex))
  times<-distances
  msec<-times

  results<-data.frame(frameIndex,distances)
  names(results)<-c("Frame","Distance")

  ##TMP
  #results<-results[1:20,]
  #times<-times[1:10]
  #msec<-msec[1:20]
  #####

  for(i in 1:length(results$Distance)){
    if(i %% 1000 == 0)
      print(paste("Rep", i, "of",length(results$Distance)))
    tmp<-subset(ic,Frame==results$Frame[i])
    if(nrow(tmp)!=2)
      stop("Oops1")
    x<-tmp$CentroidX
    y<-tmp$CentroidY
    diffx<-x[1]-x[2]
    diffy<-y[1]-y[2]
    d<-sqrt(diffx*diffx+diffy*diffy)
    results$Distance[i]<-d
    times[i]<-tmp$Time[1]
    msec[i]<-tmp$Millisec[1]
  }

  Time<-strptime(times, "%m/%d/%Y %H:%M:%OS")
  tmp<-msec/1000
  
  Time$sec<-Time$sec+tmp

  ElapsedTimeMin<-difftime(as.POSIXct(Time),as.POSIXct(Time[1]),units="min")
  tmp1<-ElapsedTimeMin[-1]
  tmp2<-ElapsedTimeMin[-length(ElapsedTimeMin)]
  tmp3<-tmp1-tmp2
  tmp3<-c(0,tmp3)
  DiffTimeMin<-tmp3
  
  Distance_mm<-results$Distance*p$mmPerPixel
  IsInteracting<-Distance_mm<=interaction.cutoff.mm
  results<-data.frame(results,Distance_mm,Time,ElapsedTimeMin,DiffTimeMin,IsInteracting)

  cat("*******\n")
  print(frequencies)
  cat("\n")
  cat(paste("Total frames interacting:",sum(results$IsInteracting),"of",length(results$IsInteracting),"frames\n"))

  cat(paste("Total time interacting (min):",sum(results$DiffTimeMin[results$IsInteracting]),"of",sum(results$DiffTimeMin),"minutes\n"))
 
  list(Frequencies=frequencies,Results=results)
}


InteractionCounterData<-function(parameters,dirname="Data",interaction.cutoff.mm=8){
  datadir<-paste("./",dirname,"/",sep="")
  files <- list.files(path=datadir,pattern = "*CountingData_[0-9].*\\.csv")    
  if(length(files)<1) {
    cat("No counting files found.")
    flush.console()      
  }
  files<-paste(datadir,files,sep="")
  
  theData<-rbindlist(lapply(files, function(x){read.csv(x, header=TRUE)}),idcol="Rep")
  #theData$Frame <-theData$Frame - theData$Frame[1] +1 
  theResults<-GetInteractionResults(theData,parameters,interaction.cutoff.mm)


  results<-list(Data=theData,Frequencies=theResults$Frequencies,Results=theResults$Results)
  
  outfile<-paste(datadir,"InteractionResults.csv",sep="")
  write.csv(results$Results,file=outfile,row.names=FALSE)
  tmp<-data.frame(results$Frequencies)
  names(tmp)<-c("FrameCounts")
  outfile<-paste(datadir,"InteractionFreq.csv",sep="")
  write.csv(tmp,file=outfile)
  results
}

GetBinnedInteractionTime<-function(results,binsize.min=10){

  et<-as.numeric(results$Results$ElapsedTimeMin)

  y<-seq(min(et),max(et),by=binsize.min)

  tmpMatrix<-cbind(y[-length(y)],y[-1])
  intervals<-cut(y+0.000001,y,include.lowest=TRUE,dig.lab=8)
  intervals<-intervals[-length(intervals)]
  midpoint<-(tmpMatrix[,1]+tmpMatrix[,2])/2
  intDuration<-rep(NA,nrow(tmpMatrix))
  percDuration<-rep(NA,nrow(tmpMatrix))
  result<-data.frame(intervals,tmpMatrix[,2]-tmpMatrix[,1],midpoint,tmpMatrix,intDuration,percDuration)
  names(result)<-c("Interval","Duration","MidPoint","Start","End","InteractionTime","PercentageInteraction")
  for(i in 1:nrow(result)){
    indexer<-(results$Results$ElapsedTimeMin > result$Start[i]) &
        (results$Results$ElapsedTimeMin <= result$End[i]) &
        (results$Results$IsInteracting == TRUE)
    tmpData<-subset(results$Results,indexer)
    result$InteractionTime[i]<-sum(tmpData$DiffTimeMin)
    result$PercentageInteraction[i]<-result$InteractionTime[i]/result$Duration[i]
  }
  result
}

UpdateDistanceCutoff<-function(results,newcutoff.mm){
  results$Results$IsInteracting<-results$Results$Distance_mm<=newcutoff.mm
  results
  }
