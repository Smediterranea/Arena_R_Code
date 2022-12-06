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
Interaction.Distance.mm<-c(2,4,6,8,10,12)
p<-ParametersClass.PairwiseInteractionTracker(Interaction.Distance.mm[4])
## saved in the output file by DTrack.
p<-Parameters.SetParameter(p,FPS=10)

## The next value is for the old CCD cameras
## mm.per.pixel<-0.2156
## The next value is for the new CCD camera setup
## mm.per.pixel<-0.131
## The next value is roughly good for the Arenas
# mm.per.pixel<-0.0.056
##p<-Parameters.SetParameter(p,mmPerPixel=0.056)
mm.per.pixel<-0.184

#dirname<-"InteractionData"
#arena<-ArenaClass(p,dirname)

dirname<-"InteractionData"
age<-59
run<-3
mm.per.pixel<-0.184

for(i in Interaction.Distance.mm){
  print(i)
  p2<-ParametersClass.PairwiseInteractionTracker(i)
  p2<-Parameters.SetParameter(p2,mmPerPixel=mm.per.pixel)
  p2<-Parameters.SetParameter(p2,FPS=10)
  arena<-ArenaClass(p2,dirname)
  data.summary<-Summarize(arena)
  IDist<-rep(i,nrow(data.summary))
  Age<-rep(age,nrow(data.summary))
  Rep<-rep(run,nrow(data.summary))
  data.summary<-data.frame(Age,Rep,IDist,data.summary)
  if(i==Interaction.Distance.mm[1]){
    results<-data.summary  
  }
  else {
    results<-rbind(results,data.summary)
  }
}


#or Copy to a clipboard to enter directly into Excel
write.table(results,"clipboard",sep="\t",row.names=FALSE)

## To output the Arena data in a form that we can send to Ali do the following
#OutputAliData.PairwiseInteractionTrackerArena(arena,dirname)


