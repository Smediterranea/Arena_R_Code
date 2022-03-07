require(ggplot2)

source("ParametersClass.R")
source("InteractionCounter.R")


##***********
datadir<-"Data"
distance.for.interaction.mm <-8
binsize.in.min<-10
##***********
##***********
##* For movies with old cameras

##***********
p<-ParametersClass.InteractionCounter()
p<-Parameters.SetParameter(p,mmPerPixel=0.2156)
p<-Parameters.SetParameter(p,FPS=10)
##***********

## Run these next four functions to get the data saved to files in data directory.
interaction.results.t0<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_0")
binned.interaction.results.t0 <-
  GetBinnedInteractionTime(interaction.results.t0, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.t0,
  file = paste(datadir, "/BinnedResultsT0.csv", sep = ""),
  row.names = FALSE
)

interaction.results.t1<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_1")
binned.interaction.results.t1 <-
  GetBinnedInteractionTime(interaction.results.t1, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.t1,
  file = paste(datadir, "/BinnedResultsT1.csv", sep = ""),
  row.names = FALSE
)

interaction.results.t2<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_2")
binned.interaction.results.t2 <-
  GetBinnedInteractionTime(interaction.results.t2, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.t2,
  file = paste(datadir, "/BinnedResultsT2.csv", sep = ""),
  row.names = FALSE
)
interaction.results.t3<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_3")
binned.interaction.results.t3 <-
  GetBinnedInteractionTime(interaction.results.t3, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.t3,
  file = paste(datadir, "/BinnedResultsT3.csv", sep = ""),
  row.names = FALSE
)
interaction.results.t4<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_4")
binned.interaction.results.t4 <-
  GetBinnedInteractionTime(interaction.results.t4, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.t4,
  file = paste(datadir, "/BinnedResultsT4.csv", sep = ""),
  row.names = FALSE
)
interaction.results.t5<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_5")
binned.interaction.results.t5 <-
  GetBinnedInteractionTime(interaction.results.t5, binsize.min = binsize.in.min)
write.csv(
  binned.interaction.results.t5,
  file = paste(datadir, "/BinnedResultsT5.csv", sep = ""),
  row.names = FALSE
)

file = paste(datadir, "/RESULTS", sep = "")
save.image(file)



