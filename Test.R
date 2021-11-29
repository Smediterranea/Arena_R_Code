
rm(list=ls())
source("ArenaObject.R")

CleanTrackers() 
p<-ParametersClass.TwoChoiceTracker()
arena<-ArenaClass(p)

PlotX(arena$Tracker_2)


unique(arena$Tracker_3$RawData$Region)






data<-
require(readxl)
  
cdir<-"./Data/"
file<-list.files(cdir,pattern="*.xlsx")
if(length(file)>1)
  stop("Only allowed one experiment file in the directory")
file<-paste(cdir,file,sep="")
test<-read_excel(file,sheet="ROI")


