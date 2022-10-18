source("ArenaObject.R")


TheActualStuff<-function(dirname,mmPerPixel,fps){
  ###################################
  ## Run this part first
  ## If no errors, then move on....

  ## Always put this here to remove any old variables
  CleanTrackers()
  
  ## First make a parameter class
  ## You can define a generic tracker
  ## You must exactly two counting regions in the experiment (not including "None")
  Interaction.Distance.mm<-c(2,4,6,8,10,12)
  p<-ParametersClass.PairwiseInteractionCounter(Interaction.Distance.mm[1])
  ## saved in the output file by DTrack.
  p<-Parameters.SetParameter(p,FPS=fps)
  
  ## The next value is for the old CCD cameras
  ## mm.per.pixel<-0.2156
  ## The next value is for the new CCD camera setup
  ## mm.per.pixel<-0.132
  ## The next value is roughly good for the Arenas
  # mm.per.pixel<-0.0.056
  p<-Parameters.SetParameter(p,mmPerPixel=mmPerPixel)

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
  
  if(length(Interaction.Distance.mm)>1){
    for(i in 2:length(Interaction.Distance.mm)){
      arena<-UpdateDistanceCutoff.Arena(arena,Interaction.Distance.mm[i])  
      tmp<-Summarize(arena)
      data.summary<-rbind(data.summary,tmp)
    }
  }
  
  Plot(arena)
  
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
  ###################################
}



RunBatchAnalysis<-function(parentFolder,mmPerPixel,fps){
  thefolders<-list.dirs(parentFolder)
  thefolders<-thefolders[-1]
  for(f in thefolders){
    print(paste("Analyzing folder:",f,sep=""))
    TheActualStuff(f,mmPerPixel,fps)
  }
}