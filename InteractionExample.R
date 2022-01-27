require(ggplot2)

source("ParametersClass.R")
source("InteractionCounter.R")


##***********
datadir<-"Run1"
distance.for.interaction.mm <-8
binsize.in.min<-5
##***********

## Run these next four functions to get the data saved to files in data directory.

##***********
p<-ParametersClass.InteractionCounter()
##***********

##***********
##*Run this section if you want all results regardless of counting region.
interaction.results<-InteractionCounterData(p,datadir,distance.for.interaction.mm)

binned.interaction.results<-GetBinnedInteractionTime(interaction.results,binsize.min=binsize.in.min)
write.csv(binned.interaction.results,file=paste(datadir,"/BinnedResults.csv",sep=""),row.names=FALSE)
##***********

##***********
## Run This section if you want separate results for "Left" and "Right" regions.
## If you want to specify analysis for only one counting region
interaction.results.left<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "Left")
interaction.results.right<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "Right")


binned.interaction.results.left<-GetBinnedInteractionTime(interaction.results.left,binsize.min=binsize.in.min)
write.csv(binned.interaction.results.left,file=paste(datadir,"/BinnedResultsLeft.csv",sep=""),row.names=FALSE)

binned.interaction.results.right<-GetBinnedInteractionTime(interaction.results.right,binsize.min=binsize.in.min)
write.csv(binned.interaction.results.right,file=paste(datadir,"/BinnedResultsRight.csv",sep=""),row.names=FALSE)

##***********

##***********
## Run the following three lines if all you want to change is the interaction distance.
newcutoff.mm<-20
interaction.results<-UpdateDistanceCutoff(interaction.results,newcutoff.mm)
binned.interaction.results<-GetBinnedInteractionTime(interaction.results,binsize.min=5)
##***********

##***********
##*With all regions combined
## Plots
ggplot(interaction.results$Results, aes(x=as.numeric(ElapsedTimeMin),y=as.numeric(IsInteracting))) + geom_step()

ggplot(interaction.results$Results, aes(x=as.numeric(ElapsedTimeMin),y=as.numeric(Distance))) + geom_step()

ggplot(binned.interaction.results, aes(x=MidPoint,y=PercentageInteraction)) + geom_step()
##***********


##***********
##*With Right and Left Regions separate
## Left Plots
ggplot(interaction.results.left$Results, aes(x=as.numeric(ElapsedTimeMin),y=as.numeric(IsInteracting))) + geom_step()

ggplot(interaction.results.left$Results, aes(x=as.numeric(ElapsedTimeMin),y=as.numeric(Distance))) + geom_step()

ggplot(binned.interaction.results.left, aes(x=MidPoint,y=PercentageInteraction)) + geom_step()
## Right Plots
ggplot(interaction.results.right$Results, aes(x=as.numeric(ElapsedTimeMin),y=as.numeric(IsInteracting))) + geom_step()

ggplot(interaction.results.right$Results, aes(x=as.numeric(ElapsedTimeMin),y=as.numeric(Distance))) + geom_step()

ggplot(binned.interaction.results.right, aes(x=MidPoint,y=PercentageInteraction)) + geom_step()

##***********
