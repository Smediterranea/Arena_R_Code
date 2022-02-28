require(ggplot2)

source("ParametersClass.R")
source("InteractionCounter.R")


##***********
datadir<-"Data"
distance.for.interaction.mm <-8
binsize.in.min<-5
##***********
##***********
##* For movies with old cameras

##***********
p<-ParametersClass.InteractionCounter()
p<-Parameters.SetParameter(p,mmPerPixel=0.2156)
p<-Parameters.SetParameter(p,FPS=10)
##***********

## Run these next four functions to get the data saved to files in data directory.
interaction.results.c0<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_0")
binned.interaction.results.c0 <-
  GetBinnedInteractionTime(interaction.results.c0, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.left,
  file = paste(datadir, "/BinnedResultsC0.csv", sep = ""),
  row.names = FALSE
)

interaction.results.c1<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_1")
binned.interaction.results.c1 <-
  GetBinnedInteractionTime(interaction.results.c1, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.left,
  file = paste(datadir, "/BinnedResultsC1.csv", sep = ""),
  row.names = FALSE
)

interaction.results.c2<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_2")
binned.interaction.results.c2 <-
  GetBinnedInteractionTime(interaction.results.c2, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.left,
  file = paste(datadir, "/BinnedResultsC2.csv", sep = ""),
  row.names = FALSE
)
interaction.results.c3<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_3")
binned.interaction.results.c3 <-
  GetBinnedInteractionTime(interaction.results.c3, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.left,
  file = paste(datadir, "/BinnedResultsC3.csv", sep = ""),
  row.names = FALSE
)
interaction.results.c4<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_4")
binned.interaction.results.c4 <-
  GetBinnedInteractionTime(interaction.results.c4, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.left,
  file = paste(datadir, "/BinnedResultsC4.csv", sep = ""),
  row.names = FALSE
)
interaction.results.c5<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_5")
binned.interaction.results.c5 <-
  GetBinnedInteractionTime(interaction.results.c5, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.left,
  file = paste(datadir, "/BinnedResultsC5.csv", sep = ""),
  row.names = FALSE
)
interaction.results.c6<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_6")
binned.interaction.results.c6 <-
  GetBinnedInteractionTime(interaction.results.c6, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.left,
  file = paste(datadir, "/BinnedResultsC6.csv", sep = ""),
  row.names = FALSE
)


