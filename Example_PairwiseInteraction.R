rm(list=ls())
## Do one of the following two
source("ArenaObject.R")

## Always put this here to remove any old variables
CleanTrackers()

## First make a parameter class
## You can define a generic tracker
## You must exactly two counting regions in the experiment (not including "None")
Interaction.Distance.mm<-8
p<-ParametersClass.PairwiseInteractionCounter(Interaction.Distance.mm)
## saved in the output file by DTrack.
p<-Parameters.SetParameter(p,FPS=NA)

## The next value is for the old CCD cameras
## mm.per.pixel<-0.2156
## The next value is for the new CCD camera setup
## mm.per.pixel<-0.132
## The next value is roughly good for the Arenas
# mm.per.pixel<-0.0.056
p<-Parameters.SetParameter(p,mmPerPixel=0.132)

dirname<-"InteractionData"
arena<-ArenaClass(p,dirname)

data.summary<-Summarize(arena)
## If you want to look at data for only a subset of the experiment, you can
## pass a range (in minutes)
Summarize(arena,range=c(0,10))

PlotX(arena, range=c(0,10))

## You can write the data to a file
write.csv(
  summary,
  file = paste(dirname, "/DataSummary.csv", sep = ""),
  row.names = FALSE
)

#or Copy to a clipboard to enter directly into Excel
write.table(data.summary,"clipboard",sep="\t",row.names=FALSE)
