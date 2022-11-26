source("ParametersClass.R")
source("TwoChoiceTracker.R")
source("XChoiceTracker.R")
source("PairwiseInteractionTracker.R")
source("DDropTracker.R")
source("GeneralUtility.R")
require(ggplot2)
require(markovchain)
require(Gmisc)


TrackerClass.RawDataFrame <-
  function(id,
           parameters,
           data,
           roisize,
           theCountingROI,
           expDesign) {
    tmp <- data
    tmp <-
      subset(tmp,
             tmp$ObjectID == id$ObjectID & tmp$TrackingRegion == id$TrackingRegion)
    tmp <- droplevels(tmp)
    
    if (is.na(parameters$FPS)) {
      ## Transform the mSec so that the first observation is 0
      ## Well, maybe not.  Definitely not for DDrop, probably
      ## not for other applications.  I'll comment it out for now.
      ##tmp$MSec<-tmp$MSec - tmp$MSec[1]
      Minutes <- tmp$MSec / (1000 * 60)
    }
    else {
      min = 1.0 / (parameters$FPS * 60)
      Minutes <- seq(from = 0,
                     by = min,
                     length.out = length(tmp$MSec))
    }
    
    tmp <- data.frame(tmp, Minutes)
    tmp$CountingRegion <- factor(tmp$CountingRegion)
    tmp$DataQuality <- factor(tmp$DataQuality)
    
    if (!is.null(expDesign)) {
      expDesign<-subset(expDesign,expDesign$ObjectID == id$ObjectID & expDesign$TrackingRegion == id$TrackingRegion)      
    }
    name<-paste(id$TrackingRegion,"_",id$ObjectID,sep="")
    data = list(
      ID = id,
      Name=name,
      ROI = roisize,
      CountingROI = theCountingROI,
      Parameters = parameters,
      RawData = tmp,
      ExpDesign = expDesign
    )
    class(data) = "Tracker"
    if (parameters$TType == "TwoChoiceTracker") {
      data <- TwoChoiceTracker.ProcessTwoChoiceTracker(data)
    }
    else if (parameters$TType == "XChoiceTracker") {
      data <- XChoiceTracker.ProcessXTracker(data)
    }
    else if (parameters$TType == "DDropTracker") {
      data <- DDropTracker.ProcessDDropTracker(data)
    }
    else if (parameters$TType == "PairwiseInteractionTracker") {
      data <- PairwiseInteractionTracker.ProcessPairwiseInteractionTracker(data)
    }
    else{
      data<-Tracker.ProcessGeneralTracker(data)
    }
    
    ## The class is done, now can add default operations to it
    ## before returning.
    data <- Tracker.Calculate.SpeedsAndFeeds(data)
    data <- Tracker.Calculate.MovementTypes(data)
    data <- Tracker.Calculate.Sleep(data)
    
    data
  }

Tracker.ProcessGeneralTracker<-function(tracker){
  class(tracker) <- c("Tracker", class(tracker))
  tracker
}

Tracker.Calculate.MovementTypes <- function(tracker) {
  tnames <- c(names(tracker$RawData), "Walking", "MicroMoving", "Resting")
  new.data.frame <-
    data.frame(tracker$RawData[1, ], c(TRUE), c(TRUE), c(TRUE)) # Temp holder
  names(new.data.frame) <- tnames
  tdata <- tracker$RawData
  micro <-
    tdata$ModifiedSpeed_mm_s > tracker$Parameters$MicroMove.mm.sec[1] &
    tdata$ModifiedSpeed_mm_s < tracker$Parameters$MicroMove.mm.sec[2]
  walking <-
    tdata$ModifiedSpeed_mm_s >  tracker$Parameters$Walking.mm.sec
  rest <-
    tdata$ModifiedSpeed_mm_s < tracker$Parameters$MicroMove.mm.sec[1]
  
  tdata <- data.frame(tdata, walking, micro, rest)
  names(tdata) <- tnames
  new.data.frame <- rbind(new.data.frame, tdata)
  
  new.data.frame <- new.data.frame[-1, ]
  tracker$RawData <- new.data.frame
  tracker
}

Tracker.Calculate.SpeedsAndFeeds <- function(tracker) {
  tnames <-
    c(
      names(tracker$RawData),
      "Xpos_mm",
      "Ypos_mm",
      "DeltaX_mm",
      "DeltaY_mm",
      "Dist_mm",
      "Speed_mm_s",
      "ModifiedSpeed_mm_s"
    )
  new.data.frame <-
    data.frame(tracker$RawData[1, ], c(1), c(1), c(1), c(1), c(1), c(1), c(1)) # Temp holder
  names(new.data.frame) <- tnames
  tdata <- tracker$RawData
  tmp <- length(tdata$X)
  if (tmp > 0) {
    x1 <- tdata$X[1:(tmp - 1)]
    x2 <- tdata$X[2:tmp]
    y1 <- tdata$Y[1:(tmp - 1)]
    y2 <- tdata$Y[2:tmp]
    min1 <- tdata$Minutes[1:(tmp - 1)]
    min2 <- tdata$Minutes[2:tmp]
    delta.sec <- c(0, (min2 - min1) * 60)
    delta.x.mm <- c(0, x2 - x1) * tracker$Parameters$mmPerPixel
    delta.y.mm <- c(0, y2 - y1) * tracker$Parameters$mmPerPixel
    dist.mm <- sqrt(delta.x.mm * delta.x.mm + delta.y.mm * delta.y.mm)
    
    speed <- dist.mm / delta.sec
    speed[1] <- 0
    
    ## For more complex speed transformations, add a function here
    if (tracker$Parameters$Smooth.Speed.Data) {
      ttt <- ksmooth(tdata$Minutes, speed, x.points = tdata$Minutes)$y
      modifiedSpeed_mm_s <- ttt
      
    }
    else {
      modifiedSpeed_mm_s <- speed
    }
    
    modifiedSpeed_mm_s[is.na(modifiedSpeed_mm_s)] <- 0
    xpos_mm <- tdata$RelX * tracker$Parameters$mmPerPixel
    ypos_mm <- tdata$RelY * tracker$Parameters$mmPerPixel
    tdata <-
      data.frame(
        tdata,
        xpos_mm,
        ypos_mm,
        delta.x.mm,
        delta.y.mm,
        dist.mm,
        speed,
        modifiedSpeed_mm_s
      )
    names(tdata) <- tnames
    new.data.frame <- rbind(new.data.frame, tdata)
    
    new.data.frame <- new.data.frame[-1, ]
    tracker$RawData <- new.data.frame
  }
  tracker
}

## Note that this functions requires that Speeds and Feeds and Movement Types
## are calculated first!

## Calculating sleep is a hard and maybe ill-defined problem.
## for now I will assume sleeping is moving between 0 and the lowest
## micromovements per sec for 5min in a row.
## Note that a consistently slow moving fly will therefore be considered
## sleeping even though it might slowly traverse the entire arena!

## This basically means a fly that is resting for 5min or more is sleeping.
Tracker.Calculate.Sleep <- function(tracker) {
  tnames <- c(names(tracker$RawData), "Sleeping")
  t1 <- tracker$RawData
  p <- tracker$Parameters
  
  ## This won't work well with higher frame rates. Error will
  ## blow up in the conversion of distance/frame to distance/sec.
  theRuns <- rle(t1$Resting)
  cumMinRuns <- t1$Minutes[cumsum(theRuns$lengths)]
  RunDurationMin <- cumMinRuns - c(0, cumMinRuns[-length(cumMinRuns)])
  LongEnoughRuns <- RunDurationMin > p$Sleep.Threshold.Min
  LongEnoughSleepRuns <- LongEnoughRuns & as.logical(theRuns$values)
  sleep <- rep(LongEnoughSleepRuns, theRuns$lengths)
  
  t1 <- data.frame(t1, sleep)
  names(t1) <- tnames
  tracker$RawData <- t1
  
  #Sleep trumps everything else
  tracker$RawData$Walking[tracker$RawData$Sleeping] <- FALSE
  tracker$RawData$Resting[tracker$RawData$Sleeping] <- FALSE
  tracker$RawData$MicroMoving[tracker$RawData$Sleeping] <- FALSE
  
  
  tracker
}

CleanTrackers <- function() {
  tmp <- ls(pattern = "Out_[0123456789]*_T[0123456789]*", pos = 1)
  rm(list = tmp, pos = 1)
  tmp <- ls(pattern = "ARENA_?", pos = 1)
  rm(list = tmp, pos = 1)
}

Tracker.ChangeParameterObject <- function(tracker, newP) {
  p <- tracker$Parameters
  fsleep.flag <- FALSE
  ferror.flag <- FALSE
  sthreshold.flag <- FALSE
  ttype.flag <- FALSE
  pixel.flag <- FALSE
  walking.flag <- FALSE
  micromove.flag <- FALSE
  
  tmp.O <- options()
  options(warn = -1)
  tracker$Parameters <- newP
  ## Change only those that are listed
  if (p$Filter.Sleep != newP$Filter.Sleep) {
    flseep.flag <- TRUE
  }
  if (p$Filter.Tracker.Error != newP$Filter.Tracker.Error) {
    ferror.flag <- TRUE
  }
  if (p$Sleep.Threshold.Distance.mm != newP$Sleep.Threshold.Distance.mm) {
    sthreshold.flag <- TRUE
  }
  if (p$Sleep.Threshold.Min != newP$Sleep.Threshold.Min) {
    sthreshold.flag <- TRUE
  }
  if (sum(p$MicroMove.mm.sec != newP$MicroMove.mm.sec) > 0) {
    micromove.flag <- TRUE
  }
  
  if (p$Walking.mm.sec != newP$Walking.mm.sec) {
    walking.flag <- TRUE
  }
  if (p$TType != newP$TType) {
    ttype.flag <- TRUE
  }
  
  ## Now update the stats needed
  if (fsleep.flag == TRUE) {
    tracker <- Tracker.Calculate.Sleep(tracker)
  }
  else if (ferror.flag == TRUE) {
    tracker <- Tracker.Calculate.SpeedsAndFeeds(tracker)
    tracker <- Tracker.Calculate.MovementTypes(tracker)
    tracker <- Tracker.Calculate.Sleep(tracker)
  }
  else if (sthreshold.flag == TRUE) {
    tracker <- Tracker.Calculate.Sleep(tracker)
  }
  else if (sum(c(micromove.flag, walking.flag)) > 0) {
    tracker <- Tracker.Calculate.MovementTypes(tracker)
    tracker <- Tracker.Calculate.Sleep(tracker)
  }
  else if (ttype.flag == TRUE) {
    cat("Do something here")
  }
  options(tmp.O)
  tracker
}

Tracker.GetType <- function(tracker) {
  tracker$Parameters$TType
}

GetMeanXPositions.Tracker <- function(tracker, range = c(0, 0)) {
  rd <- Tracker.GetRawData(tracker, range)
  Walking <- mean(rd$Xpos_mm[rd$Walking])
  MicroMoving <- mean(rd$Xpos_mm[rd$MicroMoving])
  Resting <- mean(rd$Xpos_mm[rd$Resting])
  Sleeping <- mean(rd$Xpos_mm[rd$Sleeping])
  Total <- mean(rd$Xpos_mm)
  
  tmp <-
    data.frame(tracker$ID, Walking, MicroMoving, Resting, Sleeping, Total)
  names(tmp) <-
    c("ID",
      "TrackingRegion",
      "Walking",
      "MicroMoving",
      "Resting",
      "Sleeping",
      "Total")
  tmp
}

Summarize.Tracker <- function(tracker,
                              range = c(0, 0),
                              ShowPlot = TRUE) {
  rd <- Tracker.GetRawData(tracker, range)
  
  ## Now get the summary on the rest
  total.min <- rd$Minutes[nrow(rd)] - rd$Minutes[1]
  total.frames<-nrow(rd)
  total.dist <-
    (rd$TotalDistance[nrow(rd)] - rd$TotalDistance[1]) * tracker$Parameters$mmPerPixel
  perc.Sleeping <- sum(rd$Sleeping) / length(rd$Sleeping)
  perc.Walking <- sum(rd$Walking) / length(rd$Walking)
  perc.MicroMoving <- sum(rd$MicroMoving) / length(rd$MicroMoving)
  perc.Resting <- sum(rd$Resting) / length(rd$Resting)
  
  avg.speed <- mean(rd$ModifiedSpeed_mm_s)
  
  regions <- tracker$CountingROI
  r.tmp <- matrix(rep(-1, length(regions)), nrow = 1)
  for (i in 1:length(r.tmp)) {
    r.tmp[1, i] <- sum(rd$CountingRegion == regions[i])
  }
  
  results <-
    data.frame(
      tracker$ID,
      total.min,
      total.dist,
      perc.Sleeping,
      perc.Walking,
      perc.MicroMoving,
      perc.Resting,
      avg.speed,
      range[1],
      range[2],
      total.frames,
      r.tmp
    )
  names(results) <-
    c(
      "ObjectID",
      "TrackingRegion",
      "ObsMinutes",
      "TotalDist_mm",
      "PercSleeping",
      "PercWalking",
      "PercMicroMoving",
      "PercResting",
      "AvgSpeed",
      "StartMin",
      "EndMin",
      "TotalFrames",
      regions
    )
  
  if (ShowPlot) {
    tmp <-
      data.frame(
        c(
          results$PercWalking,
          results$PercMicroMoving,
          results$PercResting,
          results$PercSleeping
        ),
        rep("one", 4),
        factor(c(
          "Walking", "MicroMoving", "Resting", "Sleeping"
        ))
      )
    names(tmp) <- c("a", "b", "Movement")
    print(
      qplot(
        x = b,
        y = a,
        data = tmp,
        fill = (Movement)
      ) + geom_bar(stat = "identity") + xlab("Treatment") + ylab("Percentage")
    )
  }
  
  results
}

Plot.Tracker<-function(tracker,
                     range = c(0, 0),
                     ShowQuality = FALSE,
                     PointSize = 0.75){
  PlotXY.Tracker(tracker,range,ShowQuality,PointSize)
}

PlotXY.Tracker <-
  function(tracker,
           range = c(0, 0),
           ShowQuality = FALSE,
           PointSize = 0.75) {
    rd <- Tracker.GetRawData(tracker, range)
    
    xlim <- c(min(rd$RelX), max(rd$RelX))
    ylim <- c(min(rd$RelY), max(rd$RelY))
    ylim2 <- c(max(rd$RelY), min(rd$RelY))
    
    
    if (ShowQuality == FALSE) {
      tmp2 <- rep("Moving", length(rd$RelX))
      tmp2[rd$Sleeping] <- "Sleeping"
      tmp2[rd$Resting] <- "Resting"
      tmp2[rd$MicroMoving] <- "Micromoving"
    }
    else {
      tmp2 <- rep("HighQuality", length(rd$RelX))
      tmp2[rd$DataQuality != "High"] <- "LowQuality"
    }
    Movement <- factor(tmp2)
    xlims <-
      c(tracker$ROI[1] / -2, tracker$ROI[1] / 2) * tracker$Parameters$mmPerPixel
    ylims <-
      c(tracker$ROI[2] / -2, tracker$ROI[2] / 2) * tracker$Parameters$mmPerPixel
    x <- ggplot(rd, aes(Xpos_mm, Ypos_mm, color = Movement)) +
      geom_point() +
      coord_fixed() +
      ggtitle(paste("Tracker:", tracker$Name, sep =
                      "")) +
      xlab("XPos (mm)") + ylab("YPos (mm)") + xlim(xlims) +
      ylim(ylims)
    print(x)
  }

PlotX.Tracker <- function(tracker, range = c(0, 0)) {
  rd <- Tracker.GetRawData(tracker, range)
  tmp2 <- rep("Moving", length(rd$X))
  tmp2[rd$Sleeping] <- "Sleeping"
  tmp2[rd$Resting] <- "Resting"
  tmp2[rd$MicroMoving] <- "Micromoving"
  
  Movement <- factor(tmp2)
  ylims <-
    c(tracker$ROI[1] / -2, tracker$ROI[1] / 2) * tracker$Parameters$mmPerPixel
  print(
    ggplot(rd, aes(Minutes, Xpos_mm),
           xlab = "Minutes", ylab = "XPos (mm)") +  ggtitle(paste("Tracker:", tracker$Name, sep =
                                                                    "")) +
      geom_rect(
        aes(
          xmin = Minutes,
          xmax = dplyr::lead(Minutes, default = 0),
          ymin = -Inf,
          ymax = Inf,
          fill = factor(Indicator)
        ),
        show.legend = F
      ) +
      scale_fill_manual(values = alpha(c("gray", "red"), .07)) +
      geom_line(aes(group = 1, color = Movement), size = 2) + ylim(ylims)
  )
  
}

PlotY.Tracker <- function(tracker, range = c(0, 0)) {
  rd <- Tracker.GetRawData(tracker, range)
  rd <- Tracker.GetRawData(tracker, range)
  tmp2 <- rep("Moving", length(rd$X))
  tmp2[rd$Sleeping] <- "Sleeping"
  tmp2[rd$Resting] <- "Resting"
  tmp2[rd$MicroMoving] <- "Micromoving"
  Movement <- factor(tmp2)
  ylims <-
    c(tracker$ROI[2] / -2, tracker$ROI[2] / 2) * tracker$Parameters$mmPerPixel
  if(is.null(tracker$ExpDesign)){
    title<-paste("Tracker:", tracker$Name, sep ="")
  }
  else{
    title<-paste("Tracker: ", tracker$Name, "  Treatment: ",tracker$ExpDesign$Treatment[1],sep ="")
  }
  print(
    ggplot(rd, aes(Minutes, Ypos_mm),
           xlab = "Minutes", ylab = "YPos (mm)") +  ggtitle(title) +
      geom_rect(
        aes(
          xmin = Minutes,
          xmax = dplyr::lead(Minutes, default = 0),
          ymin = -Inf,
          ymax = Inf,
          fill = factor(Indicator)
        ),
        show.legend = F
      ) +
      scale_fill_manual(values = alpha(c("gray", "red"), .07)) +
      geom_line(aes(group = 1, color = Movement), size = 2) + ylim(ylims)
  )
}

Tracker.BarPlotRegions <- function(tracker, range = c(0, 0)) {
  tmp <- Summarize(tracker, range)
  rois <- rep(NA, 100)
  counter <- 1
  ## remove noon-region columns
  for (i in tracker$CountingROI) {
    if (i %in% names(tmp)) {
      rois[counter] <- i
      counter <- counter + 1
    }
  }
  rois <- rois[!is.na(rois)]
  
  if (length(rois) > 0) {
    results <- data.frame(matrix(rep(NA, 2 * length(rois)), ncol = 2))
    for (i in 1:length(rois)) {
      results[i, 1] <- rois[i]
      results[i, 2] <- tmp[, rois[i]]
    }
    barplot(
      results[, 2],
      names.arg = results[, 1],
      main = paste(
        " ID:",
        tracker$ID$TrackingRegion,
        "_",
        tracker$ID$ObjectID,
        sep = ""
      ),
      ylab = "Frames"
    )
  }
}


AnalyzeTransitions.Tracker <-
  function(tracker,
           range = c(0, 0),
           ShowPlot = TRUE) {
    require(markovchain)
    data <- Tracker.GetRawData(tracker, range)
    
    tmp <- rle(as.character(data$CountingRegion))
    cumMinRuns <- data$Minutes[cumsum(tmp$lengths)]
    RunDurationMin <- cumMinRuns - c(0, cumMinRuns[-length(cumMinRuns)])
    tmp <- data.frame(tmp$lengths, tmp$values)
    names(tmp) <- c("RunDurationFrames", "Region")
    
    tmp <- data.frame(tmp, RunDurationMin)
    result <- list(Runs = tmp)
    
    mc <- markovchainFit(data$CountingRegion)
    result$TMatrix <- mc
    if (ShowPlot == TRUE) {
      gg <- result$Runs
      x <- ggplot(gg, aes(RunDurationMin, color = Region, Fill = Region)) +
        geom_bar() + scale_x_log10() + facet_wrap( ~ Region) + xlab("Duration (min)") +
        ylab("Count") + ggtitle(paste(
          " Tracker:",
          tracker$ID$TrackingRegion,
          "_",
          tracker$ID$ObjectID,
          sep = ""
        ))
      print(x)
      transitionPlot(mc$estimate@transitionMatrix, new_page = TRUE)
    }
    result
  }

SmoothTransitions.Tracker <- function(tracker, minRun = 1) {
  loop <- 1
  while (loop == 1) {
    loop <- 0
    ## First, are there runs of length minRun or less and bounded by the same region
    tmp <- AnalyzeTransitions.Tracker(tracker, ShowPlot = FALSE)
    tmp <- tmp$Runs
    EndIndex <- cumsum(tmp$RunDurationFrames)
    startIndex <- c(0, EndIndex[-length(EndIndex)])
    
    regionMatrix <-
      data.frame(tmp,
                 startIndex,
                 EndIndex,
                 c(NA, as.character(tmp$Region[-length(tmp$Region)])),
                 c(as.character(tmp$Region[-1]), NA))
    names(regionMatrix) <-
      c(
        "RunDuration",
        "Region",
        "RunDurationMin",
        "StartIndex",
        "EndIndex",
        "RegionBefore",
        "RegionAfter"
      )
    fixIndicator <-
      (tmp$RunDurationFrames <= minRun) &
      (regionMatrix$RegionBefore == regionMatrix$RegionAfter)
    regionMatrix <- data.frame(regionMatrix, fixIndicator)
    names(regionMatrix) <-
      c(
        "RunDuration",
        "Region",
        "RunDurationMin",
        "StartIndex",
        "EndIndex",
        "RegionBefore",
        "RegionAfter",
        "Indicator"
      )
    
    if (sum(regionMatrix$Indicator) > 0) {
      loop <- 1
      for (i in 1:(nrow(regionMatrix))) {
        if (regionMatrix$Indicator[i] == TRUE) {
          therange <- regionMatrix$StartIndex[i]:regionMatrix$EndIndex[i]
          tracker$RawData$Region[therange] <-
            regionMatrix$RegionBefore[i]
        }
      }
    }
  }
  tracker
}

GetQuartileXPositions.Tracker <-
  function(tracker, quartile, range = c(0, 0)) {
    if (quartile < 1 || quartile > 4)
      stop("Bad quartile parameter!")
    rd <- Tracker.GetRawData(tracker, range)
    Walking <- quantile(rd$Xpos_mm[rd$Walking])
    MicroMoving <- quantile(rd$Xpos_mm[rd$MicroMoving])
    Resting <- quantile(rd$Xpos_mm[rd$Resting])
    Sleeping <- quantile(rd$Xpos_mm[rd$Sleeping])
    Total <- quantile(rd$Xpos_mm)
    
    if (quartile > -1 && quartile < 5) {
      Walking <- Walking[quartile + 1]
      MicroMoving <- MicroMoving[quartile + 1]
      Resting <- Resting[quartile + 1]
      Sleeping <- Sleeping[quartile + 1]
      Total <- Total[quartile + 1]
    }
    else {
      Walking <- NA
      MicroMoving <- NA
      Resting <- NA
      Sleeping <- NA
      Total <- NA
    }
    tmp <-
      data.frame(tracker$ID, Walking, MicroMoving, Resting, Sleeping, Total)
    names(tmp) <-
      c("ID",
        "TrackingRegion",
        "Walking",
        "MicroMoving",
        "Resting",
        "Sleeping",
        "Total")
    tmp
  }


ReportDuration.Tracker<-function(tracker){
  result<-data.frame(matrix(c(0,"None",0,0),nrow=1))
  names(result)<-c("ObjectID","TrackingRegion","StartTime","Duration")  
  index<-1
    t<-tracker
    tmp<-t$RawData
    if(nrow(tmp)<2) {
      start<-NA
      duration<-NA
    }
    else {
      start<-tmp$Minutes[1]
      duration<-tmp$Minutes[length(tmp$Minutes)]-start
    }
    result[index,]<-c(tracker$ID$ObjectID,tracker$ID$TrackingRegion,start,duration)
    index<-index+1
  result
}

GetRuns.Tracker<-function(tracker){
  data<-Tracker.GetRawData(tracker)
  tmp<-rle(as.character(data$CountingRegion))
  tmp<-data.frame(tmp$lengths,tmp$values)
  names(tmp)<-c("RunDurationFrames","CountingRegion")
  RunDurationMin<-tmp$RunDurationFrames/tracker$Parameters$FPS/60
  CumRunDurMin<-cumsum(RunDurationMin)
  StartTime<-c(0,CumRunDurMin)
  StartTime<-StartTime[-(length(StartTime))]
  EndTime<-CumRunDurMin
  tmp<-data.frame(tmp,RunDurationMin,StartTime,EndTime)
  tmp
}

GetFirstRegionDuration.Tracker<-function(tracker,time_min){
  ## This function returns the duration info
  ## for the first duration >= time_min
  tmp<-GetRuns.Tracker(tracker)
  tmp2<-tmp[tmp$CountingRegion!="None",]
  tmp2<-tmp2[tmp2$RunDurationMin>=time_min,]
  tmp2[1,]
}


## Functions that just catch misapplied higher functions
FinalPI.Tracker <- function(tracker) {
  cat("This function not available for this type of tracker")
}
CumulativePI.Tracker <- function(tracker) {
  cat("This function not available for this type of tracker")
}
GetPIData.Tracker <- function(tracker, range = c(0, 0)) {
  cat("This function not available for this type of tracker")
}
PIPlots.Tracker <- function(tracker, range = c(0, 0)) {
  cat("This function not available for this type of tracker")
}
TimeDependentPIPlots.Tracker <- function(tracker, range = c(0, 0)) {
  cat("This function not available for this type of tracker")
}
## Utility functions
Tracker.GetRawData <- function(tracker, range = c(0, 0)) {
  rd <- tracker$RawData
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



Tracker.LastSampleData <- function(tracker) {
  tmp <- Tracker.GetRawData(tracker)
  nr <- nrow(tmp)
  tmp[nr, ]
}
Tracker.FirstSampleData <- function(tracker) {
  tmp <- Tracker.GetRawData(tracker)
  tmp[1, ]
}
