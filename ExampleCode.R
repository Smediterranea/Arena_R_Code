
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
p<-ParametersClass()

## Or one that summarizes the distribution across the X axis.
p<-ParametersClass.XChoiceTracker()

## If you have exactly two counting regions in the experiment (not including "None")
p<-ParametersClass.TwoChoiceTracker()

## If you are analyzing a movie, then you need to specify the FPS
## that was used when the movie was recorded!
## If your data were collected with a live (i.e., FLIR) camera,
## then FPS should remain NA because the interframe time is 
## saved in the output file by DTrack.
p<-Parameters.SetParameter(p,FPS=NA)

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

arena<-ArenaClass(p,dirname="GeneralTrackingData")

## Basic movement information and region summaries can be obtained from
Summarize(arena)
## By default a summary output pdf files is produces as well (this can be turned off).

## To extract a specific chamber from the experiment
tracker<-Arena.GetTracker(arena,3)

## To plot position information use PlotX for either 
## the arena or a specific tracker. If you pass an arena object
## the output is sent to a pdf file by default (can be changed)
## but if you send a tracker it is not.
PlotXY(arena)
PlotX(Arena.GetTracker(arena,1))
PlotXY(Arena.GetTracker(arena,1))

## Note that, for now, the only difference between an XChoiceTracker 
## and a generic tracker is the X Plot output.  For the Choice it outputs
## a density plot, while for the generic it outputs a plot showing position as a function
## of time.  You can force each one by calling the individual functions.

PlotX.XChoiceTracker(Arena.GetTracker(arena,1))
PlotX.Tracker(Arena.GetTracker(arena,1))

## For position plots, the code will extract the size of the tracking region
## from the excel experiment file and use it as the bounds of the plots.

## you can also focus your plots/anaysis by providing a range function (in minutes).
Summarize(arena,range=c(30,40))
PlotX.Tracker(Arena.GetTracker(arena,2),range=c(30,40))

## If you have a two choice experiment there are some additional plots and data.
## Remember that often Arena plots are output to pdf by default, while Tracker
## plots are not.
PIPlots(arena)
PIPlots(Arena.GetTracker(arena,4))
TimeDependentPIPlots(Arena.GetTracker(arena,"T0_1"))

