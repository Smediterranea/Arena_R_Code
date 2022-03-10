
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
## the lanes.
dirname<-"DDropData"
parameters<-ParametersClass.DDrop()

ReadDDropFiles(parameters,dirname)
results<-Summarize.All.DDropArenas()
outputfile<- paste("./",dirname,"/DDropResults.csv",sep="")
write.csv(results,file=outputfile,row.names=FALSE)

## Or write to clipboard to paste into excel
write.table(results,"clipboard",sep="\t",row.names=FALSE)

## Isolate the data only after the flies are first seen above
## the basement mask.
zeroed.results<-ZeroDDropResults(results)
write.table(results,"clipboard",sep="\t",row.names=FALSE)

## Plots can be useful as well
PlotY(ARENA1)
Plot.All.DDropArenas() ## This takes some time but will output pdf files