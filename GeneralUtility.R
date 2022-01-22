
GetRegions<-function(data){
  if("Region" %in% colnames(data)){
    tmp<-unique(data$Region)
    r<-tmp[tmp!="None"]
  }
  else {
    r<-NA
  }
  r
}

GetRuns.Tracker<-function(tracker){
  data<-Tracker.GetRawData(tracker)
  tmp<-rle(as.character(data$Region))
  tmp<-data.frame(tmp$lengths,tmp$values)
  names(tmp)<-c("RunDurationFrames","Region")
  RunDurationMin<-tmp$RunDurationFrames/tracker$Parameters$FPS/60
  CumRunDurMin<-cumsum(RunDurationMin)
  CumRunDurMin<-c(0,CumRunDurMin)
  CumRunDurMin<-CumRunDurMin[-length(CumRunDurMin)]
  tmp<-data.frame(tmp,RunDurationMin,CumRunDurMin)
  tmp
}

GetFirstRegionDuration.Tracker<-function(tracker,time_min){
  ## This function returns the duration info
  ## for the first duration >= time_min
  tmp<-GetRuns.Tracker(tracker)
  tmp2<-tmp[tmp$Region!="None",]
  tmp2<-tmp2[tmp2$RunDurationMin>=time_min,]
  tmp2[1,]
}

TrimArena.Arena<-function(arena, matingtime_min, duration_min){
  for(i in arena$Trackers){
    tmp<-paste("Tracker_",i,sep="")
    t<-Arena.GetTracker(arena,i)
    tmp2 <- GetFirstRegionDuration.Tracker(t, matingtime_min)
    tmp2 <- tmp2[, 4]
    tmp3<-tmp2+duration_min
    t$RawData<-subset(t$RawData,t$RawData$Minutes>tmp2 & t$RawData$Minutes<=tmp3)
    if(nrow(t$RawData)<1){
      mess<-paste("**Warning! Tracker ",i,"has no remaining data**\n")
      cat(mess)
    }
    if("TwoChoiceTracker" %in% class(t)){
      t$PIData<-subset(t$PIData,t$PIData$Minutes>tmp2 & t$PIData$Minutes<=tmp3)
    }
    arena[[tmp]]<-t
  }
  arena
}

DeleteTracker.Arena<-function(arena, trackerID){  
  tmp2<-paste("Tracker_",trackerID,sep="")  
  arena[[tmp2]]<-NULL
  arena$Trackers<-arena$Trackers[arena$Trackers!=trackerID]  
  arena
}


ReportDurations.Arena<-function(arena){
  result<-data.frame(matrix(c(0,0,0),nrow=1))
  names(result)<-c("Tracker","StartTime","Duration")  
  index<-1
    for(j in arena$Trackers){
      t<-Arena.GetTracker(arena,j)
      tmp<-t$RawData
      if(nrow(tmp)<2) {
        start<-NA
        duration<-NA
      }
      else {
        start<-tmp$Minutes[1]
        duration<-tmp$Minutes[length(tmp$Minutes)]-start
      }
      result[index,]<-c(j,start,duration)
      index<-index+1
    }  
  result
}

ZeroDDropResults<-function(results){
  new.results<-results
  colsToZero<-c("SecFirstSeen","SecTo25","SecTo50","SecTo75","SecTo90")
  new.results[,colsToZero]<-new.results[,colsToZero] - new.results[,"SecFirstSeen"]
}













## Type specific functions
Summarize<-function(tracker, ...) UseMethod("Summarize",tracker)
FinalPI<-function(tracker, ...) UseMethod("FinalPI",tracker)
CumulativePI<-function(tracker, ...) UseMethod("CumulativePI",tracker)
PIPlots<-function(tracker, ...) UseMethod("PIPlots",tracker)
TimeDependentPIPlots<-function(tracker, ...) UseMethod("TimeDependentPIPlots",tracker)
Summarize<-function(tracker, ...) UseMethod("Summarize",tracker)
PlotXY<-function(tracker, ...) UseMethod("PlotXY",tracker)
PlotX<-function(tracker, ...) UseMethod("PlotX",tracker)
PlotY<-function(tracker, ...) UseMethod("PlotY",tracker)
GetPIData<-function(tracker, ...) UseMethod("GetPIData",tracker)
AnalyzeTransitions<-function(tracker, ...) UseMethod("AnalyzeTransitions",tracker)
SmoothTransitions<-function(tracker, ...) UseMethod("SmoothTransitions",tracker)