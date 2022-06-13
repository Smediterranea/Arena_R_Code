source("GeneralUtility.R")


require(data.table)
require(reshape2)
require(readxl)
require(tibble)
require(plyr)
require(dplyr)
require(tidyr)


## Public Functions ##
GetInteractionResults <- function(ic, p, interaction.cutoff.mm = 8) {
  counts <-
    ic %>% group_by(Frame) %>% summarise(Objects = sum(NObjects))
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
  times <- distances
  msec <- times
  
  results <- data.frame(frameIndex, distances)
  names(results) <- c("Frame", "Distance")
  ##TMP
  #results<-results[1:20,]
  #times<-times[1:10]
  #msec<-msec[1:20]
  #####
  
  for (i in 1:length(results$Distance)) {
    if (i %% 1000 == 0)
      print(paste("Rep", i, "of", length(results$Distance)))
    tmp <- subset(ic, Frame == results$Frame[i])
    ## Drop spurious blobs
    tmp<-subset(tmp,BlobType!="None")
    if (nrow(tmp) != 2 && nrow(tmp) != 1){
      print(tmp)
      stop("Oops1")
      
    }
    x <- tmp$RelX
    y <- tmp$RelY
    if (length(x) == 2) {
      diffx <- x[1] - x[2]
      diffy <- y[1] - y[2]
      d <- sqrt(diffx * diffx + diffy * diffy)
      results$Distance[i] <- d
      times[i] <- tmp$Time[1]
      msec[i] <- tmp$Millisec[1]
    }
    else {
      results$Distance[i] <- 0
      times[i] <- tmp$Time
      msec[i] <- tmp$Millisec
    }
    
  }
  if(is.na(p$FPS)){
    Time <- strptime(times, "%m/%d/%Y %H:%M:%S")
    tmp <- msec / 1000
    
    Time$sec <- Time$sec + tmp
    
    ElapsedTimeMin <-
      difftime(as.POSIXct(Time), as.POSIXct(Time[1]), units = "min")
    tmp1 <- ElapsedTimeMin[-1]
    tmp2 <- ElapsedTimeMin[-length(ElapsedTimeMin)]
    tmp3 <- tmp1 - tmp2
    tmp3 <- c(0, tmp3)
    DiffTimeMin <- tmp3
  }
  else {
    Time<-results$Frame/p$FPS
    Time<-Time/60
    ElapsedTimeMin <-Time-Time[1]
    #print(ElapsedTime)
    tmp3 <- c(0, diff(Time))
    DiffTimeMin <- tmp3
  }
  
  Distance_mm <- results$Distance * p$mmPerPixel
  IsInteracting <- Distance_mm <= interaction.cutoff.mm
  
  results <-
    data.frame(results,
               Distance_mm,
               Time,
               ElapsedTimeMin,
               DiffTimeMin,
               IsInteracting)
  cat("*******\n")
  print(frequencies)
  cat("\n")
  cat(paste(
    "Total frames interacting:",
    sum(results$IsInteracting),
    "of",
    length(results$IsInteracting),
    "frames\n"
  ))
  
  cat(paste(
    "Total time interacting (min):",
    sum(results$DiffTimeMin[results$IsInteracting]),
    "of",
    sum(results$DiffTimeMin),
    "minutes\n"
  ))
  
  list(Frequencies = frequencies, Results = results)
}


InteractionCounterData <-
  function(parameters,
           dirname = "Data",
           interaction.cutoff.mm = 8,
           tracking.region = "all") {
    datadir <- paste("./", dirname, "/", sep = "")
    files <-
      list.files(path = datadir, pattern = "*Data_[0-9].*\\.csv")
    if (length(files) < 1) {
      cat("No counting files found.")
      flush.console()
      return(FALSE)
    }
    files <- paste(datadir, files, sep = "")
    
    theData <-
      rbindlist(lapply(files, function(x) {
        read.csv(x, header = TRUE)
      }), idcol = "Rep")
    
    if(!("TrackingRegion" %in% names(theData))){
      names(theData)[names(theData) == "Region"] <- "CountingRegion"
      names(theData)[names(theData) == "Name"] <- "TrackingRegion"
    }
    
    if (tracking.region != "all") {
      theData <- subset(theData, theData$TrackingRegion == tracking.region)
      outfilename<-paste("InteractionResults_",tracking.region,".csv",sep="")
      outfilename2<-paste("InteractionFreq_",tracking.region,".csv",sep="")
    }
    else{
      outfilename<-"InteractionResults.csv"
      outfilename2<-"InteractionFreq.csv"
    }
    #theData$Frame <-theData$Frame - theData$Frame[1] +1
    theResults <-
      GetInteractionResults(theData, parameters, interaction.cutoff.mm)
    
    
    results <-
      list(
        Data = theData,
        Frequencies = theResults$Frequencies,
        Results = theResults$Results
      )
    
    outfile <- paste(datadir, outfilename, sep = "")
    write.csv(results$Results, file = outfile, row.names = FALSE)
    tmp <- data.frame(results$Frequencies)
    names(tmp) <- c("FrameCounts")
    outfile <- paste(datadir, outfilename2, sep = "")
    write.csv(tmp, file = outfile)
    results
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

AnalyzeSixWellPairwise<-function(datadir="Data", distance.for.interaction.mm = 8, binsize.in.min=10,mmPerPixel=0.132,fps=10){
  p<-ParametersClass.InteractionCounter()
  p<-Parameters.SetParameter(p,mmPerPixel=mmPerPixel)
  p<-Parameters.SetParameter(p,FPS=fps)
  ## Run these next four functions to get the data saved to files in data directory.
  interaction.results.t0<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_0")
  binned.interaction.results.t0 <-
    GetBinnedInteractionTime(interaction.results.t0, binsize.min = binsize.in.min)
  write.csv(
    binned.interaction.results.t0,
    file = paste(datadir, "/BinnedResultsT0.csv", sep = ""),
    row.names = FALSE
  )
  
  interaction.results.t1<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_1")
  binned.interaction.results.t1 <-
    GetBinnedInteractionTime(interaction.results.t1, binsize.min = binsize.in.min)
  write.csv(
    binned.interaction.results.t1,
    file = paste(datadir, "/BinnedResultsT1.csv", sep = ""),
    row.names = FALSE
  )
  
  interaction.results.t2<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_2")
  binned.interaction.results.t2 <-
    GetBinnedInteractionTime(interaction.results.t2, binsize.min = binsize.in.min)
  write.csv(
    binned.interaction.results.t2,
    file = paste(datadir, "/BinnedResultsT2.csv", sep = ""),
    row.names = FALSE
  )
  interaction.results.t3<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_3")
  binned.interaction.results.t3 <-
    GetBinnedInteractionTime(interaction.results.t3, binsize.min = binsize.in.min)
  write.csv(
    binned.interaction.results.t3,
    file = paste(datadir, "/BinnedResultsT3.csv", sep = ""),
    row.names = FALSE
  )
  interaction.results.t4<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_4")
  binned.interaction.results.t4 <-
    GetBinnedInteractionTime(interaction.results.t4, binsize.min = binsize.in.min)
  write.csv(
    binned.interaction.results.t4,
    file = paste(datadir, "/BinnedResultsT4.csv", sep = ""),
    row.names = FALSE
  )
  interaction.results.t5<-InteractionCounterData(p,datadir,distance.for.interaction.mm,tracking.region = "T_5")
  binned.interaction.results.t5 <-
    GetBinnedInteractionTime(interaction.results.t5, binsize.min = binsize.in.min)
  write.csv(
    binned.interaction.results.t5,
    file = paste(datadir, "/BinnedResultsT5.csv", sep = ""),
    row.names = FALSE
  )
  
  
  AllChamber.Frequencies<-rbind(interaction.results.t0$Frequencies,interaction.results.t1$Frequencies,
                                interaction.results.t2$Frequencies,interaction.results.t3$Frequencies,
                                interaction.results.t4$Frequencies,interaction.results.t5$Frequencies)
  
  tmp.0<-sum(interaction.results.t0$Results$IsInteracting)/length(interaction.results.t0$Results$IsInteracting)
  tmp.1<-sum(interaction.results.t1$Results$IsInteracting)/length(interaction.results.t1$Results$IsInteracting)
  tmp.2<-sum(interaction.results.t2$Results$IsInteracting)/length(interaction.results.t2$Results$IsInteracting)
  tmp.3<-sum(interaction.results.t3$Results$IsInteracting)/length(interaction.results.t3$Results$IsInteracting)
  tmp.4<-sum(interaction.results.t4$Results$IsInteracting)/length(interaction.results.t4$Results$IsInteracting)
  tmp.5<-sum(interaction.results.t5$Results$IsInteracting)/length(interaction.results.t5$Results$IsInteracting)
  
  Int.Frequency<-c(tmp.0,tmp.1,tmp.2,tmp.3,tmp.4,tmp.5)
  rm("tmp.0","tmp.1","tmp.2","tmp.3","tmp.4","tmp.5")
  Chamber<-c("T_0","T_1","T_2","T_3","T_4","T_5")
  AllChamber.Frequencies<-data.frame(Chamber,AllChamber.Frequencies,Int.Frequency)
  write.csv(
    AllChamber.Frequencies,
    file = paste(datadir, "/AllFrequencies.csv", sep = ""),
    row.names = FALSE
  )
  
  
  file = paste(datadir, "/RESULTS", sep = "")
  
}