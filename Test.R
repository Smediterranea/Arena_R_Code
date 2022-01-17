require(ggplot2)

datadir<-"Data/Run1"
distance.for.interaction.mm <-8

p<-ParametersClass.InteractionCounter()
interaction.results<-InteractionCounterData(p,datadir,distance.for.interaction.mm)
binned.interaction.results<-GetBinnedInteractionTime(interaction.results,binsize.min=5)
write.csv(binned.interaction.results,file=paste(datadir,"/BinnedResults.csv",sep=""),row.names=FALSE)

## Other functions of use
newcutoff.mm<-10
interaction.results<-UpdateDistanceCutoff(interaction.results,newcutoff.mm)
binned.interaction.results<-GetBinnedInteractionTime(interaction.results,binsize.min=5)

## Plots
ggplot(results$Results, aes(x=as.numeric(ElapsedTimeMin),y=as.numeric(IsInteracting))) + geom_step()

ggplot(binned.interaction.results, aes(x=MidPoint,y=PercentageInteraction)) + geom_step()

