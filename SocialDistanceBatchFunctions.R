source("ArenaObject.R")


RunSocialDistanceBatch<-function(dirname, interacting.entities=10){
  thefolders<-list.dirs(dirname)
  thefolders<-thefolders[-1]
  for(f in thefolders){
    print(paste("Running analysis on folder: ",f,sep=""))
    TheActualSDAnalysis(f, interacting.entities)
  }
}


TheActualSDAnalysis<-function(dirname, interacting.entities=10){

  ## Do one of the following two
  
  ## Always put this here to remove any old variables
  CleanTrackers()
  
  ## First make a parameter class
  ## You can define a generic tracker
  ## You must exactly two counting regions in the experiment (not including "None")
  Interacting.Entities<-interacting.entities
  p<-ParametersClass.SocialDistanceCounter(Interacting.Entities)
  ## saved in the output file by DTrack.
  p<-Parameters.SetParameter(p,FPS=NA)
  
  ## The next value is for the old CCD cameras
  ## mm.per.pixel<-0.2156
  ## The next value is for the new CCD camera setup
  ## mm.per.pixel<-0.132
  ## The next value is roughly good for the Arenas
  # mm.per.pixel<-0.0.056
  p<-Parameters.SetParameter(p,mmPerPixel=0.132)
  
  dirname<-dirname
  print(dirname)
  arena<-ArenaClass(p,dirname)
  
  ###################################
  
  ###################################
  ## Run this part second then, copy 
  ## folder (now with results)
  ## back to its original location
  data.summary<-Summarize(arena)
  ## If you want to look at data for only a subset of the experiment, you can
  ## pass a range (in minutes)
  ##Summarize(arena,range=c(0,10))
  
  
  
  ## You can write the data to a file
  write.csv(
    data.summary,
    file = paste(dirname, "/DataSummary.csv", sep = ""),
    row.names = FALSE
  )
  
  #or Copy to a clipboard to enter directly into Excel
  #write.table(data.summary,"clipboard",sep="\t",row.names=FALSE)
  file = paste(dirname, "/RESULTS", sep = "")
  save.image(file)
  
  ## There is also a simple default plot.
  Plot(arena)
  ###################################
  
}

