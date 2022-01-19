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
interaction.results<-InteractionCounterData(p,datadir,distance.for.interaction.mm)

binned.interaction.results<-GetBinnedInteractionTime(interaction.results,binsize.min=binsize.in.min)
write.csv(binned.interaction.results,file=paste(datadir,"/BinnedResults.csv",sep=""),row.names=FALSE)
##***********

##***********
## Run the following three lines if all you want to change is the interaction distance.
newcutoff.mm<-20
interaction.results<-UpdateDistanceCutoff(interaction.results,newcutoff.mm)
binned.interaction.results<-GetBinnedInteractionTime(interaction.results,binsize.min=5)
##***********

##***********
## Plots
ggplot(interaction.results$Results, aes(x=as.numeric(ElapsedTimeMin),y=as.numeric(IsInteracting))) + geom_step()

ggplot(interaction.results$Results, aes(x=as.numeric(ElapsedTimeMin),y=as.numeric(Distance))) + geom_step()

ggplot(binned.interaction.results, aes(x=MidPoint,y=PercentageInteraction)) + geom_step()
##***********
