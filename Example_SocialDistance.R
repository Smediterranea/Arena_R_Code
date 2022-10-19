
###################################
## Run this part first
## If no errors, then move on....
rm(list=ls())
## Do one of the following two
source("ArenaObject.R")

## Always put this here to remove any old variables
CleanTrackers()

## First make a parameter class
## You can define a generic tracker
## You must exactly two counting regions in the experiment (not including "None")
Interacting.Entities<-5
p<-ParametersClass.SocialDistanceCounter(Interacting.Entities)
## saved in the output file by DTrack.
p<-Parameters.SetParameter(p,FPS=NA)

## The next value is for the old CCD cameras
## mm.per.pixel<-0.2156
## The next value is for the new CCD camera setup
## mm.per.pixel<-0.131
## The next value is roughly good for the Arenas
# mm.per.pixel<-0.0.056
p<-Parameters.SetParameter(p,mmPerPixel=0.131)

dirname<-"SocialDistanceData"
arena<-ArenaClass(p,dirname)

###################################

###################################
## Run this part second then, copy 
## folder (now with results)
## back to its original location
data.summary<-Summarize(arena)
## If you want to look at data for only a subset of the experiment, you can
## pass a range (in minutes)
##Summarize(arena,range=c(0,10))



## You can write the data to a file
write.csv(
  data.summary,
  file = paste(dirname, "/DataSummary.csv", sep = ""),
  row.names = FALSE
)

#or Copy to a clipboard to enter directly into Excel
#write.table(data.summary,"clipboard",sep="\t",row.names=FALSE)
file = paste(dirname, "/RESULTS", sep = "")
save.image(file)

## There is also a simple default plot.
Plot(arena)
###################################