require(ggplot2)

source("ParametersClass.R")
source("InteractionCounter.R")


##***********
datadir<-"Data"
distance.for.interaction.mm <-8
binsize.in.min<-5
##***********

## Run these next four functions to get the data saved to files in data directory.

##***********
p<-ParametersClass.InteractionCounter()
p<-Parameters.SetParameter(p,FPS=10)
##***********

interaction.results.c1<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_1")
interaction.results.c2<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_2")
interaction.results.c3<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_3")
interaction.results.c4<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_4")
interaction.results.c5<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_5")
interaction.results.c6<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_6")
interaction.results.c7<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_7")
interaction.results.c8<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_8")
interaction.results.c9<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_9")
interaction.results.c10<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_10")
interaction.results.c11<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_11")
interaction.results.c12<-InteractionCounterData(p,datadir,distance.for.interaction.mm,counting.region = "C_12")