
## Note, high density and PI plots require ImageMagik to be installed for 
## simpler PDF outputs.
## Get it here: https://imagemagick.org/script/download.php#windows
## Also required are the following packages: ggplot2, markovchain, Gmisc, 
##  data.table, reshape2, readxl, tibble, stringr, readr, plyr, dplyr

rm(list=ls())

require(readr)
source("ParametersClass.R")
source("TrackerObject.R")
source("ArenaObject.R")
source("GeneralUtility.R")

## In the output directory make sure to include the Experiment .xlxs file as well
## as the tacking csv files for each run. The original xlxs file is used to define
## the lanes. An experimental design file is optional (ExpDesign.csv) but helps
## simply downstream organization.
dirname<-"DDropData"
parameters<-ParametersClass.DDrop()

ReadDDropFiles(parameters,dirname)

## This results object will have data for each run as well as average for each
## fly.
results<-Summarize.All.DDropArenas()
outputfile<- paste("./",dirname,"/DDropResultsPerRun.csv",sep="")
write.csv(results$PerRun,file=outputfile,row.names=FALSE)
outputfile<- paste("./",dirname,"/DDropResultsPerFly.csv",sep="")
write.csv(results$PerFly,file=outputfile,row.names=FALSE)

## Or write to clipboard to paste into excel
## But this will CBind the two components of the list.
write.table(results$PerRun,"clipboard",sep="\t",row.names=FALSE)

## or

write.table(results$PerFly,"clipboard",sep="\t",row.names=FALSE)

## Isolate the data only after the flies are first seen above
## the basement mask.
zeroed.results<-ZeroDDropResults(results)
outputfile<- paste("./",dirname,"/ZeroedDDropResultsPerRun.csv",sep="")
write.csv(zeroed.results$PerRun,file=outputfile,row.names=FALSE)
outputfile<- paste("./",dirname,"/ZeroedDDropResultsPerFly.csv",sep="")
write.csv(zeroed.results$PerFly,file=outputfile,row.names=FALSE)


## Plots can be useful as well
PlotY(ARENA1)

## This takes some time but will output pdf files. You need Imagemagik as noted above.
Plot.All.DDropArenas() 

tdata<-results$PerFly
vars<-names(tdata)
for(i in 3:ncol(tdata)){
  cat("\n")
  cat(paste("***",vars[i],"***",sep=""))
  tmp<-aov(tdata[,i]~tdata$Treatment)
  print(summary(tmp))
  cat("\n")
}