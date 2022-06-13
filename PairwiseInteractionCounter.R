require(data.table)
require(reshape2)
require(readxl)
require(tibble)
require(plyr)
require(dplyr)
require(tidyr)

PairwiseInteractionCounter.ProcessPairwiseInteractionCounter<-function(counter){
  if (!is.null(counter$ExpDesign)) {
    a <- "TrackingRegion" %in% colnames(tracker$ExpDesign)
    b <- "Treatment" %in% colnames(tracker$ExpDesign)
    f <- c(a, b)
    if (sum(f) < 2) {
      stop(
        "Experimental design file requires TrackingRegion and Treatments columns."
      )
    }
  }
  counter <- PairwiseInteractionCounter.SetInteractionData(counter)
  class(counter) <- c("PairwiseInteractionCounter", class(counter))
  counter
}


## Public Functions ##
PairwiseInteractionCounter.SetInteractionData<-function(counter) {
  theData<-counter$RawData
  p<-counter$Parameters
  counts <-
    theData %>% group_by(Frame) %>% summarise(Objects = sum(NObjects))
  missings <-
    length(which(!(seq(
      min(counts$Frame), max(counts$Frame)
    ) %in% counts$Frame)))
  ones <- sum(counts$Objects == 1)
  twos <- sum(counts$Objects == 2)
  more <- sum(counts$Objects > 2)
  
  frequencies <- c(missings, ones, twos, more)
  names(frequencies) <- c("Zero", "One", "Two", "More")
  
  frameIndex <- subset(counts, Objects == 2)$Frame
  distances <- rep(NA, length(frameIndex))
  ElapsedTimeMin<-distances
  DiffTimeMin<-distances
  IsInteracting<-rep(FALSE,length(DiffTimeMin))
  
  results <- data.frame(frameIndex, distances,distances,ElapsedTimeMin,DiffTimeMin,IsInteracting)
  names(results) <- c("Frame", "Distance","Distance_mm","ElapsedTimeMin","DiffTimeMin","IsInteracting")
  
  for (i in 1:length(results$Distance)) {
    if (i %% 2000 == 0)
      print(paste("Rep", i, "of", length(results$Distance)))
    tmp <- subset(theData, Frame == results$Frame[i])
    ## Drop spurious blobs
    tmp<-subset(tmp,BlobType!="None")
    if (nrow(tmp) != 2 && nrow(tmp) != 1){
      print(tmp)
      stop("Oops1")
      
    }
    results$ElapsedTimeMin[i]=tmp$Minutes
    if(i==1){
      results$DiffTimeMin[i]=results$ElapsedTimeMin[i]
    }
    else {
      results$DiffTimeMin[i]=results$ElapsedTimeMin[i]-results$ElapsedTimeMin[i-1]
    }
    x <- tmp$RelX
    y <- tmp$RelY
    if (length(x) == 2) {
      diffx <- x[1] - x[2]
      diffy <- y[1] - y[2]
      d <- sqrt(diffx * diffx + diffy * diffy)
      results$Distance[i] <- d
      results$Distance_mm[i] <- d*p$mmPerPixel
      if(results$Distance_mm[i]<=p$Interaction.Distance.mm){
        results$IsInteracting[i]=TRUE
      }
           
    }
    else {
      results$Distance[i] <- 0
      results$Distance_mm[i] <- 0
      results$IsInteracting[i]<-TRUE
    }
  }
  
  counter$InteractionData<-list(Frequencies = frequencies, Results = results)
  counter
}

InteractionCounter.GetRawData <- function(counter, range = c(0, 0)) {
  rd <- counter$RawData
  if (sum(range) != 0) {
    rd <- rd[(rd$Minutes > range[1]) & (rd$Minutes < range[2]), ]
  }
  rd
}
InteractionCounter.GetInteractionData <- function(counter, range = c(0, 0)) {
  rd <- counter$InteractionData$Results
  if (sum(range) != 0) {
    rd <- rd[(rd$Minutes > range[1]) & (rd$Minutes < range[2]), ]
  }
  rd
}

Summarize.PairwiseInteractionCounter<-function(counter,range=c(0,0),ShowPlot=TRUE){
  rd<-InteractionCounter.GetInteractionData(counter,range) 
  
  interacting<-rd[counter$InteractionData$Results$IsInteracting==TRUE,]
  notinteracting<-rd[rd$IsInteracting==FALSE,]
  total.time<-sum(rd$DiffTimeMin) 
  time.interacting<-sum(interacting$DiffTimeMin)
  time.notinteracting<-sum(notinteracting$DiffTimeMin) 
  
  ff<-c(counter$InteractionData$Frequencies)
  print(ff)
  results<-data.frame(counter$ID,total.time,time.interacting,time.notinteracting,time.interacting/total.time,ff[1],ff[2],ff[3],ff[4],ff[3]/(sum(ff)),
                      range[1],range[2])
  names(results)<-c("TrackingRegion","ObsMinutes","MinutesInteracting","MinutesNotInteracting","PercentInteraction","Zero","One","Two","More","PercTwo","StartMin","EndMin")
  rownames(results)<-1:nrow(results)
  results
}

PlotXY.PairwiseInteractionCounter<-function(counter,range = c(0, 0)){
  print("HI there!!")
  
}


GetBinnedInteractionTime <- function(results, binsize.min = 10) {
  et <- as.numeric(results$Results$ElapsedTimeMin)
  
  y <- seq(min(et), max(et), by = binsize.min)
  
  tmpMatrix <- cbind(y[-length(y)], y[-1])
  intervals <- cut(y + 0.000001,
                   y,
                   include.lowest = TRUE,
                   dig.lab = 8)
  intervals <- intervals[-length(intervals)]
  midpoint <- (tmpMatrix[, 1] + tmpMatrix[, 2]) / 2
  intDuration <- rep(NA, nrow(tmpMatrix))
  percDuration <- rep(NA, nrow(tmpMatrix))
  result <-
    data.frame(intervals,
               tmpMatrix[, 2] - tmpMatrix[, 1],
               midpoint,
               tmpMatrix,
               intDuration,
               percDuration)
  names(result) <-
    c(
      "Interval",
      "Duration",
      "MidPoint",
      "Start",
      "End",
      "InteractionTime",
      "PercentageInteraction"
    )
  for (i in 1:nrow(result)) {
    indexer <- (results$Results$ElapsedTimeMin > result$Start[i]) &
      (results$Results$ElapsedTimeMin <= result$End[i]) &
      (results$Results$IsInteracting == TRUE)
    tmpData <- subset(results$Results, indexer)
    result$InteractionTime[i] <- sum(tmpData$DiffTimeMin)
    result$PercentageInteraction[i] <-
      result$InteractionTime[i] / result$Duration[i]
  }
  result
}

UpdateDistanceCutoff <- function(results, newcutoff.mm) {
  results$Results$IsInteracting <-
    results$Results$Distance_mm <= newcutoff.mm
  results
}
