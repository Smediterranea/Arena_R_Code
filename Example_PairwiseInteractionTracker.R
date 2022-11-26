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
p<-Parameters.SetParameter(p,mmPerPixel=0.056)


dirname<-"InteractionData"
arena<-ArenaClass(p,dirname)

Summarize(arena)


## To output the Arena data in a form that we can send to Ali do the following
OutputAliData.PairwiseInteractionTrackerArena(arena,dirname)


