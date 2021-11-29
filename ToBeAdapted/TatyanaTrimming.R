
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
  data<-Tracker.GetRawData(tracker)
  tmp<-rle(as.character(data$Region))
  tmp<-data.frame(tmp$lengths,tmp$values)
  names(tmp)<-c("RunDurationFrames","Region")
  RunDurationMin<-tmp$RunDurationFrames/tracker$Parameters$FPS/60
  CumRunDurMin<-cumsum(RunDurationMin)
  CumRunDurMin<-c(0,CumRunDurMin)
  CumRunDurMin<-CumRunDurMin[-length(CumRunDurMin)]
  tmp<-data.frame(tmp,RunDurationMin,CumRunDurMin)
  tmp2<-tmp[tmp$Region!="None",]
  tmp2<-tmp2[tmp2$RunDurationMin>=time_min,]
  tmp2[1,4]
}

TrimChamber.Chamber<-function(chamber, matingtime_min, duration_min){
  for(i in chamber$Trackers){
    tmp<-paste("Tracker_",i,sep="")
    t<-Chamber.GetTracker(chamber,i)
    tmp2<-GetFirstRegionDuration.Tracker(t,matingtime_min)
    tmp3<-tmp2+duration_min
    t$RawData<-subset(t$RawData,t$RawData$Minutes>tmp2 & t$RawData$Minutes<=tmp3)
    t$PIData<-subset(t$PIData,t$PIData$Minutes>tmp2 & t$PIData$Minutes<=tmp3)
    chamber[[tmp]]<-t
  }
  chamber
}

TrimExperiment.Experiment<-function(experiment,matingtime_min,duration_min){
  for(i in experiment$Chambers){
    tmp<-paste("Chamber",i,sep="")
    tmp2<-Experiment.GetChamber(experiment,i)
    experiment[[tmp]]<-TrimChamber.Chamber(tmp2,matingtime_min,duration_min)
  }
  experiment
}

ReportDurations.Experiment<-function(experiment){
  result<-data.frame(matrix(c(0,0,0,0),nrow=1))
  names(result)<-c("Chamber","Tracker","StartTime","Duration")
  index<-1
  for(i in experiment$Chambers){
    chamber<-Experiment.GetChamber(experiment,i)
    for(j in chamber$Trackers){
      t<-Chamber.GetTracker(chamber,j)
      tmp<-t$RawData
      if(nrow(tmp)<2) {
        start<-NA
        duration<-NA
      }
      else {
        start<-tmp$Minutes[1]
        duration<-tmp$Minutes[length(tmp$Minutes)]-start
      }
      result[index,]<-c(i,j,start,duration)
      index<-index+1
    }
  }
  result
}

DeleteTracker.Experiment<-function(chamber, tracker, experiment){
  tmp<-paste("Chamber",chamber,sep="")
  tmp2<-paste("Tracker_",tracker,sep="")
  chamber<-Experiment.GetChamber(experiment,chamber)
  chamber[[tmp2]]<-NULL
  chamber$Trackers<-chamber$Trackers[chamber$Trackers!=tracker]
  experiment[[tmp]]<-chamber
  experiment
}