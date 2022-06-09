require(ggplot2)

source("ParametersClass.R")
source("InteractionCounter.R")

## This is meant to be used for analysis of movie files (not live recording)
## with FPS=10 frames per sec during recording. It also assumes six tracking regions T_0 ... T_5
##***********
datadir<-"Movie1Part1"
distance.for.interaction.mm <-8
binsize.in.min<-10
##***********
##***********
##* For movies with old cameras

##***********
p<-ParametersClass.InteractionCounter()
## The next value is for the old CCD cameras
#p<-Parameters.SetParameter(p,mmPerPixel=0.2156)
## The next value is for the new CCD camera setup
p<-Parameters.SetParameter(p,mmPerPixel=0.132)
## The next value is roughly good for the Arenas
##p<-Parameters.SetParameter(p,mmPerPixel=0.056)
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


AllChamber.Frequencies<-rbind(interaction.results.t0$Frequencies,interaction.results.t1$Frequencies,
                              interaction.results.t2$Frequencies,interaction.results.t3$Frequencies,
                              interaction.results.t4$Frequencies,interaction.results.t5$Frequencies)

tmp.0<-sum(interaction.results.t0$Results$IsInteracting)/length(interaction.results.t0$Results$IsInteracting)
tmp.1<-sum(interaction.results.t1$Results$IsInteracting)/length(interaction.results.t1$Results$IsInteracting)
tmp.2<-sum(interaction.results.t2$Results$IsInteracting)/length(interaction.results.t2$Results$IsInteracting)
tmp.3<-sum(interaction.results.t3$Results$IsInteracting)/length(interaction.results.t3$Results$IsInteracting)
tmp.4<-sum(interaction.results.t4$Results$IsInteracting)/length(interaction.results.t4$Results$IsInteracting)
tmp.5<-sum(interaction.results.t5$Results$IsInteracting)/length(interaction.results.t5$Results$IsInteracting)

Int.Frequency<-c(tmp.0,tmp.1,tmp.2,tmp.3,tmp.4,tmp.5)
rm("tmp.0","tmp.1","tmp.2","tmp.3","tmp.4","tmp.5")
Chamber<-c("T_0","T_1","T_2","T_3","T_4","T_5")
AllChamber.Frequencies<-data.frame(Chamber,AllChamber.Frequencies,Int.Frequency)
write.csv(
  AllChamber.Frequencies,
  file = paste(datadir, "/AllFrequencies.csv", sep = ""),
  row.names = FALSE
)


file = paste(datadir, "/RESULTS", sep = "")
save.image(file)





