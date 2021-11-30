
rm(list=ls())
source("ArenaObject.R")

CleanTrackers() 
p<-ParametersClass.TwoChoiceTracker()
arena<-ArenaClass(p)

PlotX(arena)


unique(arena$Tracker_3$RawData$Region)


t1<-arena$Tracker_4$RawData

sleep.possible<-t1$Dist_mm<p$Sleep.Threshold.Distance.mm
theRuns<-rle(as.character(sleep.possible))
cumMinRuns<-t1$Minutes[cumsum(theRuns$lengths)]
RunDurationMin<-cumMinRuns-c(0,cumMinRuns[-length(cumMinRuns)])
LongEnoughRuns<-RunDurationMin>p$Sleep.Threshold.Min
LongEnoughSleepRuns<-LongEnoughRuns & as.logical(theRuns$values)
sleep<-rep(LongEnoughSleepRuns,theRuns$lengths)











data<-
require(readxl)
  
cdir<-"./Data/"
file<-list.files(cdir,pattern="*.xlsx")
if(length(file)>1)
  stop("Only allowed one experiment file in the directory")
file<-paste(cdir,file,sep="")
test<-read_excel(file,sheet="ROI")


