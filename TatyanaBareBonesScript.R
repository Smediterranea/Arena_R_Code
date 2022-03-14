
## Note, high density and PI plots require ImageMagik to be installed for 
## simpler PDF outputs.
## Get it here: https://imagemagick.org/script/download.php#windows
## Also required are the following packages: ggplot2, markovchain, Gmisc, 
##  data.table, reshape2, readxl, tibble, stringr, readr


rm(list=ls())
## Do one of the following two
source("ArenaObject.R")

## Always put this here to remove any old variables
CleanTrackers()

## If you have exactly two counting regions in the experiment (not including "None")
p<-ParametersClass.TwoChoiceTracker()

## If you are analyzing a movie, then you need to specify the FPS
## that was used when the movie was recorded!
## If your data were collected with a live (i.e., FLIR) camera,
## then FPS should remain NA because the interframe time is 
## saved in the output file by DTrack.
p<-Parameters.SetParameter(p,FPS=2)
p<-Parameters.SetParameter(p,mmPerPixel=0.2156)

## Check out the parameters and ensure that they are appropriate.
## Notably, make sure the mmPerPixel value is correct for your setup.
## Note that sleep is not currently implemented.

## change parameters as you need
##p<-SetParameter(p,Filter.Sleep=1)
##p<-SetParameter(p,Filter.Tracker.Error=`1)

## Place your tracking data and the experiment file in a subdirectory.
## If you name it something other than 'Data' you need to supply it as
## a parameter to the ArenaClass function
## ArenaClass<-function(parameters,dirname="Data")

datadir<-"TrackingData"

arena<-ArenaClass(p,dirname=datadir)

## Basic movement information and region summaries can be obtained from
summary.data<-Summarize(arena)
write.csv(summary.data,file=paste(datadir,"/SummaryResults.csv",sep=""),row.names=FALSE)


## If you want to trim the data then run these commands
minutes_in_region<-5/60
subsequent_observable_duration_min<-30

arena.trimmed<-Trim.Arena(arena,minutes_in_region,subsequent_observable_duration_min)
ReportDuration.Arena(arena.trimmed)
summary.data.trimmed<-Summarize(arena.trimmed)
write.csv(summary.data.trimmed,file=paste(datadir,"/SummaryResultsTrimmed.csv",sep=""),row.names=FALSE)


##PlotXY(arena)
##PIPlots(arena)
#TimeDependentPIPlots(Arena.GetTracker(arena,"T1_0"))
#TimeDependentPIPlots(Arena.GetTracker(arena,"T2_0"))
#TimeDependentPIPlots(Arena.GetTracker(arena,"T3_0"))
#TimeDependentPIPlots(Arena.GetTracker(arena,"T4_0"))
#TimeDependentPIPlots(Arena.GetTracker(arena,"T5_0"))
#TimeDependentPIPlots(Arena.GetTracker(arena,"T6_0"))


##PlotXY(arena.trimmed)
##PIPlots(arena.trimmed)
#TimeDependentPIPlots(Arena.GetTracker(arena.trimmed,"T1_0"))
#TimeDependentPIPlots(Arena.GetTracker(arena.trimmed,"T2_0"))
#TimeDependentPIPlots(Arena.GetTracker(arena.trimmed,"T3_0"))
#TimeDependentPIPlots(Arena.GetTracker(arena.trimmed,"T4_0"))
#TimeDependentPIPlots(Arena.GetTracker(arena.trimmed,"T5_0"))
#TimeDependentPIPlots(Arena.GetTracker(arena.trimmed,"T6_0"))



