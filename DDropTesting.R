rm(list=ls())

require(readr)
source("ParametersClass.R")
source("TrackerObject.R")
source("ArenaObject.R")
source("GeneralUtility.R")

dirname<-"DDropData"
parameters<-ParametersClass.DDrop()

DDropClass<-function(parameters,dirname="Data"){
  datadir<-paste("./",dirname,"/",sep="")
  files <- list.files(path=datadir,pattern = "*TrackingData_[0-9]*.csv")    
  if(length(files)<1) {
    cat("No tracking files found.")
    flush.console()      
  }
  files<-paste(datadir,files,sep="")
  for(f in files){
    tmp<-substring(f,3,nchar(f))
    runnumber<-readr::parse_number(tmp)
    Load.DDrop.Object(parameters,f,dirname,runnumber)
  }
}

Load.DDrop.Object<-function(parameters,filename,dirname,runNumber){
  datadir<-paste("./",dirname,"/",sep="")
  theData<-read.csv(filename, header=TRUE)
  
  ## Just for DDrop, the RELY position needs to be inverted (for plotting, etc)
  theData$RelY<-theData$RelY*(-1.0)
  
  trackers<-unique(theData$Name)
  
  ## Get the tracking ROI and the Counting ROI
  ## This reg expression tries to avoid temporary files that begin with '~'
  file <- list.files(datadir, pattern = "^[^~].*xlsx")
  if(length(file)>1)
    stop("Only allowed one experiment file in the directory")
  if(length(file)<1)
    stop("Original experiment file is missing")
  file<-paste(datadir,file,sep="")
  roi <- read_excel(file, sheet = "ROI")
  roi<-subset(roi,roi$Name!="AutoGenerateAntiMask")
  
  ## Look for experimental design file
  files <- list.files(path = datadir, pattern = "ExpDesign.csv") 
  if (length(files) < 1) {
    expDesign <- NULL
  }
  else {
    files <- paste(datadir, files, sep = "")    
    expDesign <- read.csv(files[1])    
  }
  
  arenaName<-paste("Arena",runNumber,sep="")
  DDrop <- list(Name = arenaName, Trackers = trackers, ROI = roi, ExpDesign=expDesign, DataDir=dirname)
  
  if(length(trackers)>0){
    for(i in trackers){
      nm<-paste("Tracker",i,sep="_")
      roinm<-i
      theROI<-c(roi$Width[roi$Name==roinm],roi$Height[roi$Name==roinm])
      theCountingROI<-roi$Name[roi$Type=="Counting"]
      if(length(theCountingROI)<1)
        theCountingROI<-"None"
      tmp<-TrackerClass.RawDataFrame(i,parameters,theData,theROI,theCountingROI,expDesign)
      DDrop<-c(DDrop,setNames(list(nm=tmp),nm))
    }
  }
  class(DDrop)="Arena"
  st<-paste("ARENA",runNumber,sep="")
  assign(st,DDrop,pos=1)  
  DDrop
}