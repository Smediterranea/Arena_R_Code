
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

## First make a parameter class
## You can define a generic tracker
## You must exactly two counting regions in the experiment (not including "None")
p<-ParametersClass.TwoChoiceTracker()

## If you are analyzing a movie, then you need to specify the FPS
## that was used when the movie was recorded!
## If your data were collected with a live (i.e., FLIR) camera,
## then FPS should remain NA because the interframe time is 
## saved in the output file by DTrack.
p<-Parameters.SetParameter(p,FPS=2)

## you should also set the mmPixel for the camera used, if you know it.
## This is for the small, older CCD cams
p<-Parameters.SetParameter(p,mmPerPixel=0.2156)

## This is for a blackfly on the arena
##p<-Parameters.SetParameter(p,mmPerPixel=0.056)

## This directory needs the data files, which are required
## to end in Data_#.csv where # is a number, the original excel experiment file
## from DTrack, and and ExpDesign.csv file with four columns (and order)
## TrackingRegion, ObjectID, CountingRegion, Treatment
dirname<-"TwoChoiceTrackingData"
arena<-ArenaClass(p,dirname)

## Get the basic movement data and treatment frame counts
## Also outputs some simple barplots of movement.
results<-Summarize(arena)
write.csv(results,file=paste(dirname,"/Results.csv",sep=""),row.names=FALSE)

## Plot the relevant data.  Plots will be output to PDF, not the ImageMagik requirement above.
##PlotX(arena)
##PlotY(arena)
PlotXY(arena)
PIPlots(arena)

## Additional plots and outputs are available for individual trackers, such as
TimeDependentPIPlots.TwoChoiceTracker(arena$Tracker_T6_0)

## See Scott for more details.


