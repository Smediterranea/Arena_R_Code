rm(list=ls())
source("DDropBatchFunctions.R")
## In the output directory make sure to include the Experiment .xlxs file as well
## as the tacking csv files for each run. The original xlxs file is used to define
## the lanes. An experimental design file is optional (ExpDesign.csv) but helps
## simply downstream organization.
dirname<-"DDropData"
p<-ParametersClass.DDrop()

## Set this to the total time observed for each run
p<-Parameters.SetParameter(p,ObservationTime.sec=15)
## Set this to the divisions to analyze y distance moved
p<-Parameters.SetParameter(p,DDropDivision.sec=3)
## Set this to proper conversion for the camera
## Pletcher lab = 0.087
p<-Parameters.SetParameter(p,mmPerPixel=0.087)

RunDDropBatch(dirname,p)