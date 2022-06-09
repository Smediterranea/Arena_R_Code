require(ggplot2)

source("ParametersClass.R")
source("InteractionCounter.R")

## This is meant to be used for analysis of movie files (not live recording)
## with FPS=10 frames per sec during recording. It also assumes six tracking regions T_0 ... T_5
##***********
datadir<-"InteractionData"
distance.for.interaction.mm <-8
binsize.in.min<-10
FPS=10

## The next value is for the old CCD cameras
mm.per.pixel<-0.2156
## The next value is for the new CCD camera setup
#mm.per.pixel<-0.132
## The next value is roughly good for the Arenas
#mm.per.pixel<-0.0.056

AnalyzeSixWellPairwise(datadir, distance.for.interaction.mm, binsize.in.min,mm.per.pixel,FPS)