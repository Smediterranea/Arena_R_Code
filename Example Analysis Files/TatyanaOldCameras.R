## Note, high density and PI plots require ImageMagik to be installed for 
## simpler PDF outputs.
## Get it here: https://imagemagick.org/script/download.php#windows
## Also required are the following packages: ggplot2, markovchain, Gmisc, 
##  data.table, reshape2, readxl, tibble

rm(list=ls())
## Do one of the following two
source("ArenaObject.R")



## Always put this here to remove any old variables
CleanTrackers()

## First make a parameter class
p<-ParametersClass.TwoChoiceTracker()
p<-Parameters.SetParameter(p,FPS=2)


## Should also set the mmPerPixel parameter for the setup!
#p<-Parameters.SetParameter(p,mmPerPixel=0.056)

## change parameters as you need
##p<-SetParameter(p,Filter.Sleep=1)
##p<-SetParameter(p,Filter.Tracker.Error=`1)

arena<-ArenaClass(p,dirname="Data")

## Then just do the entire analysis in one shot
## For plots, red points indicate tracking issues, green dots indicate potentially sleeping flies


Summarize(arena)

## If you want to trim the data to start at first time in region and subsequent duration
minutes_in_region<-5/60
subsequent_observable_duration_minutes<-30 

arena2<-Trim.Arena(arena,minutes_in_region,subsequent_observable_duration_minutes)

## Now get a printout of the actual time observed for each tracker
ReportDurations.Arena(arena2)

## There are problems with trackers 2 and 3, which didn't seem to ever mate.  You can remove them if needed with

arena3<-DeleteTracker.Arena(arena2,2)
arena3<-DeleteTracker.Arena(arena3,3)

ReportDurations.Arena(arena3)

## These all seem okay, with 30min of observations each.
## Now you can just get the summary data.

Summarize(arena3)

###############################
## This bit will help you print out runs.
################################
tracker_num<-5

## To pull a specific tracker out from the chamber
tracker<-Arena.GetTracker(arena,tracker_num)

## If you want to see the runs in a specific tracker, call the GetRuns.Tracker
## function, which takes a tracker object as an argument

GetRuns.Tracker(tracker)
################################