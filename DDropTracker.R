require(stringr)

DDropTracker.ProcessDDropTracker<-function(tracker){
  if (!is.null(tracker$ExpDesign)) {
    a <- "ObjectID" %in% colnames(tracker$ExpDesign)
    b <- "TrackingRegion" %in% colnames(tracker$ExpDesign)
    c <- "Treatment" %in% colnames(tracker$ExpDesign)
    d <- "Run" %in% colnames(tracker$ExpDesign)
    e <- "Fly" %in% colnames(tracker$ExpDesign)
    f <- c(a, b, c, d,e)
    if (sum(f) < 5) {
      stop(
        "Experimental design file requires Run, ObjectID,TrackingRegion,Fly, and Treatments columns."
      )
    }
  }
  class(tracker)<-c("DDropTracker",class(tracker))
  tracker
}

Summarize.DDropTracker<-function(tracker,range=c(0,0),ShowPlot=FALSE){  
  rd<-Tracker.GetRawData(tracker,range)  
  
  perc.Sleeping<-sum(rd$Sleeping)/length(rd$Sleeping)
  perc.Walking<-sum(rd$Walking)/length(rd$Walking)
  perc.MicroMoving<-sum(rd$MicroMoving)/length(rd$MicroMoving)
  perc.Resting<-sum(rd$Resting)/length(rd$Resting)
  
  firstTimeSeen<-rd$Minutes[1]*60
  firstUps<-Tracker.GetFirstTimeAboveYPos(tracker,c(0.25,0.5,0.75,0.9))
  totalYdist<-Tracker.GetTotalYDist(tracker,range)
  totalUpdist<-Tracker.GetTotalUpDist(tracker,range)

  avgspeed<-mean(rd$Speed, na.rm=TRUE)
  
  tmp<-sort(rd$Speed)
  tmp<-tmp[(length(tmp)*0.9):length(tmp)]
  
  avgTop10speed<-mean(tmp, na.rm=TRUE)
  totaldist<-rd$TotalDistance[length(rd$TotalDistance)]*tracker$Parameters$mmPerPixel
  
  total.min<-rd$Minutes[nrow(rd)]-rd$Minutes[1]
  total.sec<-total.min*60
  if(is.null(tracker$ExpDesign)){
    results<-data.frame(tracker$ID,total.sec,firstTimeSeen,firstUps[1],firstUps[2],firstUps[3],firstUps[4],totalYdist,totalUpdist,avgspeed,avgTop10speed,totaldist,perc.Sleeping,perc.Walking,perc.MicroMoving,perc.Resting,range[1],range[2])
    names(results)<-c("ObjectID","TrackingRegion","ObsSeconds","SecFirstSeen","SecTo25","SecTo50", "SecTo75", "SecTo90","TotalYDist","TotalUpDist","AvgSpeed","AvgTop10Speed","TotalDist_mm","PercSleeping","PercWalking","PercMicroMoving","PercResting","StartMin","EndMin")
  }
  else{
    results<-data.frame(tracker$ID,tracker$ExpDesign$Fly,tracker$ExpDesign$Treatment,total.sec,firstTimeSeen,firstUps[1],firstUps[2],firstUps[3],firstUps[4],totalYdist,totalUpdist,avgspeed,avgTop10speed,totaldist,perc.Sleeping,perc.Walking,perc.MicroMoving,perc.Resting,range[1],range[2])
    names(results)<-c("ObjectID","TrackingRegion","Fly","Treatment","ObsSeconds","SecFirstSeen","SecTo25","SecTo50", "SecTo75", "SecTo90","TotalYDist","TotalUpDist","AvgSpeed","AvgTop10Speed","TotalDist_mm","PercSleeping","PercWalking","PercMicroMoving","PercResting","StartMin","EndMin")
  }
  results
}
  

GetAveragedPerFlyResults<-function(results){
  colstoget<-c("ObsSeconds","SecFirstSeen","SecTo25","SecTo50","SecTo75","SecTo90","TotalYDist","TotalUpDist",
               "AvgSpeed","AvgTop10Speed","TotalDist_mm")
  fly<-results$Fly
  data<-results[,colstoget]
  results2<-aggregate(data,list(Fly=fly),FUN=mean,na.rm=TRUE)
  Treatment<-rep(NA,nrow(results2))
  for(i in 1:length(Treatment)){
    tmp<-results$Treatment[results$Fly==results2$Fly[i]]
    Treatment[i]<-tmp[1]
  }
  results2<-data.frame(Treatment,results2)
  results2
}

## Result is in seconds
Tracker.GetFirstTimeAboveYPos<-function(tracker,percentclimb){
  result<-NA
  precentclimb<-percentclimb/100 # To get decimal equivalent
  ## Remember that higher is more positive for Y position
  ## because we inverted the RelY values when the data were 
  ## read.
  
  roiHeight<-tracker$ROI[2]
  maxY<-roiHeight/2
  minY <- -1.0 * roiHeight/2
  threshold<-(roiHeight*percentclimb) - (roiHeight/2)
  
  if(length(percentclimb)==1) {
    tmp<-tracker$RawData
    tmp<-tmp[tmp$RelY>threshold,"Minutes"]
    if(length(tmp)>0){
      result<-tmp[1]
    }
    else {
      result<-NA
    }
  }
  else {
    result<-rep(-1,length(percentclimb))
    for(i in 1:length(percentclimb)){
      tmp<-tracker$RawData
      tmp<-tmp[tmp$RelY>threshold[i],"Minutes"]
      if(length(tmp)>0) {
        result[i]<-tmp[1]
      }
      else {
        result[i]<-NA
      }
    }
  }
  result*60
}

Tracker.GetFirstRelYPos<-function(tracker){
  tracker$RawData$RelY[1]
}

Tracker.GetTotalYDist<-function(tracker,time=c(0,0)){
  rd<-Tracker.GetRawData(tracker,time)
  y1<-rd$RelY[-1]
  y2<-rd$RelY[-length(rd$RelY)]
  delta.y<-sum(abs(y1-y2))
  delta.y*tracker$Parameters$mmPerPixel  
}

Tracker.GetTotalUpDist<-function(tracker,time=c(0,0)){
  rd<-Tracker.GetRawData(tracker,time)
  y1<-rd$RelY[-1]
  y2<-rd$RelY[-length(rd$RelY)]
  delta.y<-y1-y2
  delta.y[delta.y<0]<-0
  tmp<-sum(delta.y)
  tmp*tracker$Parameters$mmPerPixel
}

## Functions that just catch misapplied higher functions
FinalPI.DDropTracker<-function(tracker){
  cat("This function not available for this type of tracker")
}
CumulativePI.DDropTracker<-function(tracker){
  cat("This function not available for this type of tracker")
}
GetPIData.DDropTracker<-function(tracker,range=c(0,0)){
  cat("This function not available for this type of tracker")
}
PIPlots.DDropTracker<-function(tracker,range=c(0,0)){
  cat("This function not available for this type of tracker")
}
TimeDependentPIPlots.DDropTracker<-function(tracker,range=c(0,0)){
  cat("This function not available for this type of tracker")
}


Summarize.All.DDropArenas<-function(){
  objects<-ls(pos=1)
  arenas<-objects[grep("ARENA*",objects)]
  if(length(arenas)>0){
  for(i in arenas){
    print(i)
    tmp<-get(i)
    tmp.result<-Summarize(tmp,ShowPlot=FALSE)
    Arena<-rep(tmp$Name)
    tmp.result<-data.frame(Arena,tmp.result)
    if(exists("result",inherits = FALSE)==TRUE){
      result<-rbind(result,tmp.result)       
    }
    else {
      result<-tmp.result     
    }
  }
  }
  else {
    result<-NA
  }
  if("Fly" %in% colnames(result)){
    results2<-GetAveragedPerFlyResults(result)
  }
  else {
    results2<-NULL
  }
  result<-list(PerRun=result,PerFly=results2)
  result
}

ZeroDDropResults<-function(results){
  new.results<-results$PerRun
  colsToZero<-c("SecFirstSeen","SecTo25","SecTo50","SecTo75","SecTo90")
  new.results[,colsToZero]<-new.results[,colsToZero] - new.results[,"SecFirstSeen"]
  if("Fly" %in% colnames(results)){
    results2<-GetAveragedPerFlyResults(results)
  }
  else {
    results2<-NULL
  }
  new.results<-list(PerRun=new.results,PerFly=results2)
  new.results
}

Plot.DDropTracker<-function(tracker,range=c(0,0)){
  PlotY(tracker,range)
}

Plot.All.DDropArenas<-function(){
  objects<-ls(pos=1)
  arenas<-objects[grep("ARENA*",objects)]
  if(length(arenas)>0){
    for(i in arenas){
      tmp<-get(i)
      print(paste("Plotting ",tmp$Name))
      PlotY(tmp)
    }
  }
}