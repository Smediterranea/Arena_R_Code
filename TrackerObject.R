source("ParametersClass.R")
source("TwoChoiceTracker.R")
source("XChoiceTracker.R")
source("GeneralUtility.R")
require(ggplot2)
require(markovchain)
require(Gmisc)


TrackerClass.RawDataFrame<-function(id,parameters,data,roisize,theCountingROI,expDesign){
  if (!is.numeric(id))
    stop("invalid arguments") 
  
  st<-paste("ARENA_T",id,sep="")
  
  tmp<-data
  tmp<-subset(tmp,tmp$ObjectID==id)
  tmp<-droplevels(tmp)
  
  if(is.na(parameters$FPS)){
    ## Transform the mSec so that the first observation is 0
    tmp$MSec<-tmp$MSec - tmp$MSec[1]
    Minutes<-tmp$MSec/(1000*60)
  }
  else {    
    min = 1.0/(parameters$FPS*60)
    Minutes <- seq(from=0,by=min,length.out=length(tmp$MSec))
  }
  
  tmp<-data.frame(tmp,Minutes)
  tmp$Region<-factor(tmp$Region)
  tmp$DateQuality<-factor(tmp$DataQuality)

  if(!is.null(expDesign)){
    expDesign=subset(expDesign,expDesign$ID==id)
  }

  data=list(ID=id,ROI=roisize,CountingROI=theCountingROI,Parameters=parameters,RawData=tmp,ExpDesign=expDesign)
  class(data)="Tracker"
  
  ## The class is done, now can add default operations to it
  ## before returning.
  data<-Tracker.Calculate.SpeedsAndFeeds(data)
  data<-Tracker.Calculate.MovementTypes(data)
  data<-Tracker.Calculate.Sleep(data)
  ##if(length(GetRegions(data$RawData))==2) {
  ##  data<-TwoChoiceTracker.ProcessTwoChoiceTracker(data)
  ##}
  if(p$TType=="TwoChoiceTracker"){
    data<-TwoChoiceTracker.ProcessTwoChoiceTracker(data)
  }
  else if(p$TType=="XChoiceTracker"){
    data<-XChoiceTracker.ProcessXTracker(data)
  }
  if(nrow(tmp)>0)
    assign(st,data,pos=1)  
  data
}

Tracker.Calculate.MovementTypes<-function(tracker){
  tnames<-c(names(tracker$RawData),"Walking","MicroMoving","Resting")
  new.data.frame<-data.frame(tracker$RawData[1,],c(TRUE),c(TRUE),c(TRUE)) # Temp holder
  names(new.data.frame)<-tnames
  tdata<-tracker$RawData
  micro <-tdata$ModifiedSpeed_mm_s > tracker$Parameters$MicroMove.mm.sec[1] & tdata$ModifiedSpeed_mm_s < tracker$Parameters$MicroMove.mm.sec[2]
  walking <-tdata$ModifiedSpeed_mm_s >  tracker$Parameters$Walking.mm.sec
  rest<-tdata$ModifiedSpeed_mm_s < tracker$Parameters$MicroMove.mm.sec[1]
  
  tdata<-data.frame(tdata,walking,micro,rest)
  names(tdata)<-tnames
  new.data.frame<-rbind(new.data.frame,tdata)
  
  new.data.frame<-new.data.frame[-1,]
  tracker$RawData<-new.data.frame   
  tracker
}

Tracker.Calculate.SpeedsAndFeeds<-function(tracker){
  tnames<-c(names(tracker$RawData),"Xpos_mm","Ypos_mm","DeltaX_mm","DeltaY_mm","Dist_mm","Speed_mm_s","ModifiedSpeed_mm_s")
  new.data.frame<-data.frame(tracker$RawData[1,],c(1),c(1),c(1),c(1),c(1),c(1),c(1)) # Temp holder
  names(new.data.frame)<-tnames
  tdata<-tracker$RawData
  tmp<-length(tdata$X) 
  if(tmp>0) {
    x1<-tdata$X[1:(tmp-1)]
    x2<-tdata$X[2:tmp]
    y1<-tdata$Y[1:(tmp-1)]
    y2<-tdata$Y[2:tmp]
    min1<-tdata$Minutes[1:(tmp-1)]
    min2<-tdata$Minutes[2:tmp]
    delta.sec<-c(0,(min2-min1)*60)
    delta.x.mm<-c(0,x2-x1)*tracker$Parameters$mmPerPixel
    delta.y.mm<-c(0,y2-y1)*tracker$Parameters$mmPerPixel
    dist.mm<-sqrt(delta.x.mm*delta.x.mm + delta.y.mm*delta.y.mm)
    
    speed<-dist.mm/delta.sec
    speed[1]<-0
    
    ## For more complex speed transformations, add a function here
    if(tracker$Parameters$Smooth.Speed.Data){
      ttt<-ksmooth(tdata$Minutes,speed)$y    
      modifiedSpeed_mm_s<-ttt
      
    }
    else {
      modifiedSpeed_mm_s<-speed  
    }
    
    modifiedSpeed_mm_s[is.na(modifiedSpeed_mm_s)]<-0
    xpos_mm<-tdata$RelX*tracker$Parameters$mmPerPixel
    ypos_mm<-tdata$RelY*tracker$Parameters$mmPerPixel
    tdata<-data.frame(tdata,xpos_mm,ypos_mm,delta.x.mm,delta.y.mm,dist.mm,speed,modifiedSpeed_mm_s)
    names(tdata)<-tnames
    new.data.frame<-rbind(new.data.frame,tdata)
    
    new.data.frame<-new.data.frame[-1,]
    tracker$RawData<-new.data.frame    
  }
  tracker
}

## Note that this functions requires that Speeds and Feeds and Movement Types
## are calculated first!

## Calculating sleep is a hard and maybe ill-defined problem.
## for now I will assume sleeping is moving between 0 and the lowest
## micromovements per sec for 5min in a row.
## Note that a consistently slow moving fly will therefore be considered
## sleeping even though it might slowly traverse the entire arena!

## This basically means a fly that is resting for 5min or more is sleeping. 
Tracker.Calculate.Sleep<-function(tracker){
  tnames<-c(names(tracker$RawData),"Sleeping")
  t1<-tracker$RawData
  p<-tracker$Parameters
  
  ## This won't work well with higher frame rates. Error will 
  ## blow up in the conversion of distance/frame to distance/sec.
  theRuns<-rle(t1$Resting)
  cumMinRuns<-t1$Minutes[cumsum(theRuns$lengths)]
  RunDurationMin<-cumMinRuns-c(0,cumMinRuns[-length(cumMinRuns)])
  LongEnoughRuns<-RunDurationMin>p$Sleep.Threshold.Min
  LongEnoughSleepRuns<-LongEnoughRuns & as.logical(theRuns$values)
  sleep<-rep(LongEnoughSleepRuns,theRuns$lengths)
  
  t1<-data.frame(t1,sleep)
  names(t1)<-tnames
    
  #Sleep trumps everything else
  tracker$RawData$Walking[tracker$RawData$Sleeping]<-FALSE
  tracker$RawData$Resting[tracker$RawData$Sleeping]<-FALSE
  tracker$RawData$MicroMoving[tracker$RawData$Sleeping]<-FALSE
    
  
  tracker
}

CleanTrackers<-function(){
  tmp<-ls(pattern="Out_[0123456789]*_T[0123456789]*",pos=1)
  rm(list=tmp,pos=1)
  tmp<-ls(pattern="ARENA_?",pos=1)
  rm(list=tmp,pos=1)
}

Tracker.ChangeParameterObject<-function(tracker,newP) {
  p<-tracker$Parameters
  fsleep.flag<-FALSE
  ferror.flag<-FALSE
  sthreshold.flag<-FALSE
  ttype.flag<-FALSE
  pixel.flag<-FALSE
  walking.flag<-FALSE
  micromove.flag<-FALSE
  
  tmp.O<-options()
  options(warn=-1)
  tracker$Parameters<-newP
  ## Change only those that are listed
  if(p$Filter.Sleep!=newP$Filter.Sleep) {    
    flseep.flag<-TRUE
  }
  if(p$Filter.Tracker.Error!=newP$Filter.Tracker.Error) {    
    ferror.flag<-TRUE
  }
  if(p$Sleep.Threshold.Distance.mm!=newP$Sleep.Threshold.Distance.mm) {    
    sthreshold.flag<-TRUE
  }
  if(p$Sleep.Threshold.Min!=newP$Sleep.Threshold.Min) {    
    sthreshold.flag<-TRUE
  }
  if(sum(p$MicroMove.mm.sec!=newP$MicroMove.mm.sec)>0) {    
    micromove.flag<-TRUE
  }
  
  if(p$Walking.mm.sec != newP$Walking.mm.sec) {    
    walking.flag<-TRUE
  }
  if(p$TType!=newP$TType) {    
    ttype.flag<-TRUE
  }
  
  ## Now update the stats needed
  if(fsleep.flag==TRUE) {
    tracker<-Tracker.Calculate.Sleep(tracker)
  }
  else if(ferror.flag==TRUE){
    tracker<-Tracker.Calculate.SpeedsAndFeeds(tracker)
    tracker<-Tracker.Calculate.MovementTypes(tracker)
    tracker<-Tracker.Calculate.Sleep(tracker)
  }
  else if(sthreshold.flag==TRUE) {
    tracker<-Tracker.Calculate.Sleep(tracker)
  }
  else if(sum(c(micromove.flag,walking.flag))>0){
    tracker<-Tracker.Calculate.MovementTypes(tracker)
    tracker<-Tracker.Calculate.Sleep(tracker)
  }
  else if(ttype.flag==TRUE){
    cat("Do something here")
  }
  options(tmp.O)
  tracker
}

Tracker.GetType<-function(tracker){
  tracker$Parameters$TType
}

GetMeanXPositions.Tracker<-function(tracker,range=c(0,0)){
  rd<-Tracker.GetRawData(tracker,range)
  Walking<-mean(rd$Xpos_mm[rd$Walking])
  MicroMoving<-mean(rd$Xpos_mm[rd$MicroMoving])
  Resting<-mean(rd$Xpos_mm[rd$Resting])
  Sleeping<-mean(rd$Xpos_mm[rd$Sleeping])
  Total<-mean(rd$Xpos_mm)
  
  tmp<-data.frame(tracker$ID,Walking,MicroMoving,Resting,Sleeping,Total)
  names(tmp)<-c("ID","Walking","MicroMoving","Resting","Sleeping","Total")
  tmp
}

Summarize.Tracker<-function(tracker,range=c(0,0),ShowPlot=TRUE){
  rd<-Tracker.GetRawData(tracker,range)  
  
  ## Now get the summary on the rest
  total.min<-rd$Minutes[nrow(rd)]-rd$Minutes[1]
  total.dist<-(rd$TotalDistance[nrow(rd)]-rd$TotalDistance[1])*tracker$Parameters$mmPerPixel  
  perc.Sleeping<-sum(rd$Sleeping)/length(rd$Sleeping)
  perc.Walking<-sum(rd$Walking)/length(rd$Walking)
  perc.MicroMoving<-sum(rd$MicroMoving)/length(rd$MicroMoving)
  perc.Resting<-sum(rd$Resting)/length(rd$Resting)
  
  avg.speed<-mean(rd$ModifiedSpeed_mm_s)
  
  regions<-levels(rd$Region)  
  r.tmp<-matrix(rep(-1,length(regions)),nrow=1)
  for(i in 1:length(r.tmp)){
    r.tmp[1,i]<-sum(rd$Region==regions[i])
  }
  
  results<-data.frame(tracker$ID,total.min,total.dist,perc.Sleeping,perc.Walking,perc.MicroMoving,perc.Resting,avg.speed,range[1],range[2],r.tmp)
  names(results)<-c("ID","ObsMinutes","TotalDist_mm","PercSleeping","PercWalking","PercMicroMoving","PercResting","AvgSpeed","StartMin","EndMin",regions)
  
  if(ShowPlot){
    tmp<-data.frame(c(results$PercWalking,results$PercMicroMoving,results$PercResting,results$PercSleeping),rep("one",4), factor(c("Walking","MicroMoving","Resting","Sleeping")))
    names(tmp)<-c("a","b","Movement")
    print(qplot(x=b,y=a,data=tmp, fill=(Movement)) + geom_bar(stat="identity")+ xlab("Treatment") + ylab("Percentage")) 
  }
  
  results
}

PlotXY.Tracker<-function(tracker,range=c(0,0),ShowQuality=FALSE,PointSize=0.75){
  rd<-Tracker.GetRawData(tracker,range)
  
  xlim<-c(min(rd$RelX),max(rd$RelX))
  ylim<-c(min(rd$RelY),max(rd$RelY))
  ylim2<-c(max(rd$RelY),min(rd$RelY))
  
  
  if(ShowQuality==FALSE){
    tmp2<-rep("Moving",length(rd$RelX))
    tmp2[rd$Sleeping]<-"Sleeping"
    tmp2[rd$Resting]<-"Resting"
    tmp2[rd$MicroMoving]<-"Micromoving"
  }
  else {
    tmp2<-rep("HighQuality",length(rd$RelX))
    tmp2[rd$DataQuality!="High"]<-"LowQuality"
  }
  Movement<-factor(tmp2)
  xlims<-c(tracker$ROI[1]/-2,tracker$ROI[1]/2)*tracker$Parameters$mmPerPixel
  ylims<-c(tracker$ROI[2]/-2,tracker$ROI[2]/2)*tracker$Parameters$mmPerPixel
  x<-ggplot(rd,aes(Xpos_mm,Ypos_mm,color=Movement)) + 
    geom_point() + 
    coord_fixed() + 
    ggtitle(paste("Chamber:",tracker$Chamber," Tracker:",tracker$ID,sep="")) +
    xlab("XPos (mm)") + ylab("YPos (mm)") + xlim(xlims) +
    ylim(ylims)
  print(x)
}

PlotX.Tracker<-function(tracker,range = c(0,0)){  
  rd<-Tracker.GetRawData(tracker,range)
  tmp2<-rep("Moving",length(rd$X))
  tmp2[rd$Sleeping]<-"Sleeping"
  tmp2[rd$Resting]<-"Resting"
  tmp2[rd$MicroMoving]<-"Micromoving"
  
  Movement<-factor(tmp2)
  ylims<-c(tracker$ROI[1]/-2,tracker$ROI[1]/2)*tracker$Parameters$mmPerPixel
  print(ggplot(rd, aes(Minutes, Xpos_mm),
         xlab="Minutes", ylab="XPos (mm)") + ggtitle(paste(" ID:",tracker$ID,sep="")) + 
    geom_rect(aes(xmin = Minutes, xmax = dplyr::lead(Minutes,default=0), ymin = -Inf, ymax = Inf, fill = factor(Indicator)), show.legend=F)+  
    scale_fill_manual(values = alpha(c("gray", "red"), .07)) +
    geom_line(aes(group=1,color=Movement),size=2) + ylim(ylims))
              
}

PlotY.Tracker<-function(tracker,range = c(0,0)){    
  rd<-Tracker.GetRawData(tracker,range)
    rd<-Tracker.GetRawData(tracker,range)
    tmp2<-rep("Moving",length(rd$X))
    tmp2[rd$Sleeping]<-"Sleeping"
    tmp2[rd$Resting]<-"Resting"
    tmp2[rd$MicroMoving]<-"Micromoving"
    Movement<-factor(tmp2)
    ylims<-c(tracker$ROI[2]/-2,tracker$ROI[2]/2)*tracker$Parameters$mmPerPixel
    print(ggplot(rd, aes(Minutes, Ypos_mm),
            xlab="Minutes", ylab="YPos (mm)") + ggtitle(paste(" ID:",tracker$ID,sep="")) + 
            geom_rect(aes(xmin = Minutes, xmax = dplyr::lead(Minutes,default=0), ymin = -Inf, ymax = Inf, fill = factor(Indicator)), show.legend=F)+  
            scale_fill_manual(values = alpha(c("gray", "red"), .07)) +
            geom_line(aes(group=1,color=Movement),size=2) + ylim(ylims))
}

Tracker.BarPlotRegions<-function(tracker,include.none=FALSE,range=c(0,0)){
  tmp<-Summarize(tracker,range)
  ## remove noon-region columns
  camera<-tmp[1,1]
  id<-tmp[1,2]
  tmp<-tmp[1,c(-1,-2,-3,-4,-5,-6,-7,-8,-9)]
  name<-names(tmp)[tmp>0]
  tmp<-tmp[tmp>0]
  if(include.none==FALSE){
    tmp<-tmp[name!="None"]
    name<-name[name!="None"]    
  }
  if(length(tmp>0)){
    barplot(tmp,names.arg=name,main =paste(" ID:",tracker$ID,sep=""),ylab="Frames")
  }
  else {
    tmp<-Summarize(tracker,range)
    barplot(tmp$None,names.arg="None")
  }
}


AnalyzeTransitions.Tracker<-function(tracker,range=c(0,0),ShowPlot=TRUE){
  require(markovchain)
  data<-Tracker.GetRawData(tracker,range)
  
  tmp<-rle(as.character(data$Region))
  cumMinRuns<-data$Minutes[cumsum(tmp$lengths)]
  RunDurationMin<-cumMinRuns-c(0,cumMinRuns[-length(cumMinRuns)])
  tmp<-data.frame(tmp$lengths,tmp$values)
  names(tmp)<-c("RunDurationFrames","Region")
  
  tmp<-data.frame(tmp,RunDurationMin)
  result<-list(Runs=tmp)
  
  mc<-markovchainFit(data$Region)
  result$TMatrix<-mc
  if(ShowPlot==TRUE){
  gg<-result$Runs
  x<-ggplot(gg,aes(RunDurationMin,color=Region, Fill=Region)) +
    geom_bar() + scale_x_log10() + facet_wrap(~Region) + xlab("Duration (min)") + 
    ylab("Count") + ggtitle(paste(" Tracker:",tracker$ID,sep=""))
  print(x)
  transitionPlot(mc$estimate@transitionMatrix,new_page = TRUE)
  }
  result
}

SmoothTransitions.Tracker<-function(tracker,minRun=1){
  loop<-1
  while(loop==1){
    loop<-0
    ## First, are there runs of length minRun or less and bounded by the same region
    tmp<-AnalyzeTransitions.Tracker(tracker,ShowPlot=FALSE)
    tmp<-tmp$Runs
    EndIndex<-cumsum(tmp$RunDurationFrames)
    startIndex<-c(0,EndIndex[-length(EndIndex)])
    
    regionMatrix<-data.frame(tmp,startIndex,EndIndex,c(NA,as.character(tmp$Region[-length(tmp$Region)])),c(as.character(tmp$Region[-1]),NA))
    names(regionMatrix)<-c("RunDuration","Region","RunDurationMin","StartIndex","EndIndex","RegionBefore","RegionAfter")
    fixIndicator<-(tmp$RunDurationFrames<=minRun) & (regionMatrix$RegionBefore == regionMatrix$RegionAfter)
    regionMatrix<-data.frame(regionMatrix,fixIndicator)
    names(regionMatrix)<-c("RunDuration","Region","RunDurationMin","StartIndex","EndIndex","RegionBefore","RegionAfter","Indicator")
    
    if(sum(regionMatrix$Indicator)>0) {
      loop<-1
      for(i in 1:(nrow(regionMatrix))){
        if(regionMatrix$Indicator[i]==TRUE){
          therange<-regionMatrix$StartIndex[i]:regionMatrix$EndIndex[i]
          tracker$RawData$Region[therange]<-regionMatrix$RegionBefore[i]
        }
      }
    }
  }
  tracker
}

GetQuartileXPositions.Tracker<-function(tracker,quartile,range=c(0,0)){
  if(quartile<1 || quartile>4)
    stop("Bad quartile parameter!")
  rd<-Tracker.GetRawData(tracker,range)
  Walking<-quantile(rd$Xpos_mm[rd$Walking])
  MicroMoving<-quantile(rd$Xpos_mm[rd$MicroMoving])
  Resting<-quantile(rd$Xpos_mm[rd$Resting])
  Sleeping<-quantile(rd$Xpos_mm[rd$Sleeping])
  Total<-quantile(rd$Xpos_mm)
  
  if(quartile>-1 && quartile<5) {
    Walking<-Walking[quartile+1]
    MicroMoving<-MicroMoving[quartile+1]
    Resting<-Resting[quartile+1]
    Sleeping<-Sleeping[quartile+1]
    Total<-Total[quartile+1]
  }
  else {
    Walking<-NA
    MicroMoving<-NA
    Resting<-NA
    Sleeping<-NA
    Total<-NA
  }
  tmp<-data.frame(tracker$ID,Walking,MicroMoving,Resting,Sleeping,Total)
  names(tmp)<-c("ID","Walking","MicroMoving","Resting","Sleeping","Total")
  tmp
}

## Functions that just catch misapplied higher functions
FinalPI.Tracker<-function(tracker){
  cat("This function not available for this type of tracker")
}
CumulativePI.Tracker<-function(tracker){
  cat("This function not available for this type of tracker")
}
GetPIData.Tracker<-function(tracker,range=c(0,0)){
  cat("This function not available for this type of tracker")
}
PIPlots.Tracker<-function(tracker,range=c(0,0)){
  cat("This function not available for this type of tracker")
}
TimeDependentPIPlots.Tracker<-function(tracker,range=c(0,0)){
  cat("This function not available for this type of tracker")
}
## Utility functions
Tracker.GetRawData<-function(tracker,range=c(0,0)){
  rd <-tracker$RawData
  if(sum(range)!=0) {    
    rd<- rd[(rd$Minutes>range[1]) & (rd$Minutes<range[2]),]
  }
  ## Filter out unwanted data
  if(tracker$Parameters$Filter.Sleep==TRUE)
    rd<-rd[rd$Sleeping==0,]
  if(tracker$Parameters$Filter.Tracker.Error==1)
    rd<-rd[rd$DataQuality=="High",]
  rd
}


Tracker.LastSampleData<-function(tracker){
  tmp<-Tracker.GetRawData(tracker)
  nr<-nrow(tmp)
  tmp[nr,]
}
Tracker.FirstSampleData<-function(tracker){
  tmp<-Tracker.GetRawData(tracker)
  tmp[1,]
}
