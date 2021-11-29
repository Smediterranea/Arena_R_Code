rm(list=ls())

## Always put this here to remove any old variables
CleanTrackers()

## First make a parameter class
p<-ParametersClass.XChoiceTracker()
p<-Parameters.SetParameter(p,FPS=2)

fn<-"ExpDesignXChoice.csv"

chambers<-c(2,3,4,5,6,7)
exp<-ExperimentClass.Camera(chambers,p,fn)

summary.results<-Summarize(exp)
means.results<-SummarizeXMeans(exp)

