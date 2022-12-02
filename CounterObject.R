source("ParametersClass.R")
source("PairwiseInteractionCounter.R")
source("SocialDistanceCounter.R")

CounterClass.RawDataFrame <-
  function(id,
           parameters,
           data,
           roisize,
           theCountingROI,
           expDesign) {
    tmp <- data
    tmp <-
      subset(tmp,tmp$TrackingRegion == id)
    tmp <- droplevels(tmp)
    
    if (is.na(parameters$FPS)) {
      ## Transform the mSec so that the first observation is 0
      ## Well, maybe not.  Definitely not for DDrop, probably
      ## not for other applications.  I'll comment it out for now.
      ##tmp$MSec<-tmp$MSec - tmp$MSec[1]
      Minutes <- tmp$MSec / (1000 * 60)
    }
    else {
      min.per.frame = 1.0 / (parameters$FPS * 60)
      Minutes <- tmp$Frame*min.per.frame
    }
    
    tmp <- data.frame(tmp, Minutes)
    tmp$CountingRegion <- factor(tmp$CountingRegion)
    tmp$DataQuality <- factor(tmp$DataQuality)
    
    if (!is.null(expDesign)) {
      expDesign<-subset(expDesign,expDesign$TrackingRegion == id)      
    }
    
    name<-paste(id)
    data = list(
      ID = id,
      Name=name,
      ROI = roisize,
      CountingROI = theCountingROI,
      Parameters = parameters,
      RawData = tmp,
      ExpDesign = expDesign
    )
    class(data) = "Counter"
    if (parameters$TType == "PairwiseInteractionCounter") {
      data <- PairwiseInteractionCounter.ProcessPairwiseInteractionCounter(data)
    }
    else if (parameters$TType == "SocialDistanceCounter") {
      data <- SocialDistanceCounter.ProcessSocialDistanceCounter(data)
    }
    else if(parameters$TType == "Counter"){
      
    }
    else{
      stop("Improper tracker type!")
    }
    
    data
  }


Counter.GetRawData <- function(counter, range = c(0, 0)) {
  rd <- counter$RawData
  if (sum(range) != 0) {
    rd <- rd[(rd$Minutes > range[1]) & (rd$Minutes < range[2]), ]
  }
  ## Filter out unwanted data
  if (tracker$Parameters$Filter.Sleep == TRUE) {
    rd <- rd[rd$Sleeping == 0, ]
  }
  if (tracker$Parameters$Filter.Tracker.Error == 1) {
    rd <- rd[rd$DataQuality == "High", ]
  }
  rd
}