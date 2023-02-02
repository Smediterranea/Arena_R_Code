require(readr)
source("ParametersClass.R")
source("TrackerObject.R")
source("ArenaObject.R")
source("GeneralUtility.R")

RunDDropBatch<-function(dirname,p){
  thefolders<-list.dirs(dirname)
  thefolders<-thefolders[-1]
  for(f in thefolders){
    print(paste("Running DDrop analysis on folder: ",f,sep=""))
    TheActualDDropAnalysis(f, p)
  }
}

TheActualDDropAnalysis<-function(f,p){
  ReadDDropFiles(p,f)
  
  ## This results object will have data for each run as well as average for each
  ## fly.
  results<-Summarize.All.DDropArenas()
  outputfile<- paste("./",f,"/DDropResultsPerRun.csv",sep="")
  write.csv(results$PerRun,file=outputfile,row.names=FALSE)
  outputfile<- paste("./",f,"/DDropResultsPerFly.csv",sep="")
  write.csv(results$PerFly,file=outputfile,row.names=FALSE)
  ## Plots can be useful as well
  PlotY(ARENA1)
}
