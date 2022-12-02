
## Note, high density and PI plots require ImageMagik to be installed for 
## simpler PDF outputs.
## Get it here: https://imagemagick.org/script/download.php#windows
## Also required are the following packages: ggplot2, markovchain, Gmisc, 
##  data.table, reshape2, readxl, tibble, stringr, readr

rm(list=ls())


source("ArenaObject.R")

## Also source the functions at the end of this file before running the following
## code.

## Always put this here to remove any old variables
CleanTrackers()

## First make a parameter class
## You can define a generic tracker
p<-ParametersClass.ArenaCounter()

## If you are analyzing a movie, then you need to specify the FPS
## that was used when the movie was recorded!
## If your data were collected with a live (i.e., FLIR) camera,
## then FPS should remain NA because the interframe time is 
## saved in the output file by DTrack.
p<-Parameters.SetParameter(p,FPS=NA)
arena<-ArenaClass(p,dirname="GeneralTrackingData")

arena<-Process.Arena(arena)


center.radius.mm<-10
Summarize.EvieCounter(arena,center.radius.mm)



####################################################################
## Source the functions below here before running the above code.

## TotalDistance
Summarize.EvieCounter<-function(arena,center.radius.mm=5){
  trackers<-arena$Trackers
  results<-data.frame(matrix(rep(NA,8),nrow=2))
  names(results)<-c("Region","TotalDistance_mm","IsInCenter","TotalFrames")
  if(nrow(trackers)>0){
    for(i in 1:nrow(trackers)){
      t<-paste("Tracker_",trackers[i,1],sep="")
      cc<-arena[[t]]
      results[i,1]<-t
      results[i,2]<-cc$RawData$TotalDistance_mm[length(cc$RawData$TotalDistance_mm)]
      results[i,3]<-sum(cc$RawData$CenterDistance_mm<=center.radius.mm)/nrow(cc$RawData)
      results[i,4]<-nrow(cc$RawData)
    }
  }
  results
}




Process.Arena<-function(arena){
  trackers<-arena$Trackers
  if(nrow(trackers)>0){
    for(i in 1:nrow(trackers)){
      t<-paste("Tracker_",trackers[i,1],sep="")
      cc<-arena[[t]]
      cc<-add.distances(cc)
      arena[[t]]<-cc
    }
  }
  arena
}
    
add.distances<-function(counter,radius_mm){
  tmp<-counter$RawData[,c("RelX","RelY")]
  CenterDistance<-apply(tmp,1,get.distance.to.center.pixels)
  CenterDistance_mm<-CenterDistance*counter$Parameters$mmPerPixel
  tmp2<-tmp
  pos<-cbind(tmp[-nrow(tmp),],tmp2[-1,])
  distances<-apply(pos,1,get.distance)
  TotalDistance<-c(0,cumsum(distances))
  TotalDistance_mm<-TotalDistance*counter$Parameters$mmPerPixel
  counter$RawData<-data.frame(counter$RawData,TotalDistance,TotalDistance_mm,CenterDistance,CenterDistance_mm)
  counter
}

get.distance.to.center.pixels<-function(pos){
  sqrt(pos[1]*pos[1]+pos[2]*pos[2])
}

get.distance<-function(r){
  delta.x<-r[1]-r[3]
  delta.y<-r[2]-r[4]
  dist<-sqrt(delta.x*delta.x+delta.y*delta.y)
  dist
}