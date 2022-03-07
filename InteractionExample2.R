require(ggplot2)

source("ParametersClass.R")
source("InteractionCounter.R")


##***********
distance.for.interaction.mm <- 8
binsize.in.min <- 30
p <- ParametersClass.InteractionCounter()
##***********

##***********
## Run this section of code to analyze all Data directories, assuming left/right chambers
dirs <- list.dirs(full.names = FALSE, recursive = FALSE)

for (i in dirs) {
  if (grepl("Data", i) == TRUE) {
    datadir <- i
    
    interaction.results.left <-
      InteractionCounterData(p, datadir, distance.for.interaction.mm, tracking.region = "Left")
    interaction.results.right <-
      InteractionCounterData(p, datadir, distance.for.interaction.mm, tracking.region = "Right")
    
    
    binned.interaction.results.left <-
      GetBinnedInteractionTime(interaction.results.left, binsize.min = binsize.in.min)
    write.csv(
      binned.interaction.results.left,
      file = paste(datadir, "/BinnedResultsLeft.csv", sep = ""),
      row.names = FALSE
    )
    
    binned.interaction.results.right <-
      GetBinnedInteractionTime(interaction.results.right, binsize.min = binsize.in.min)
    write.csv(
      binned.interaction.results.right,
      file = paste(datadir, "/BinnedResultsRight.csv", sep = ""),
      row.names = FALSE
    )
  }
}
##***********
## Run this section of code to analyze all Data directories, assuming single chambers
dirs <- list.dirs(full.names = FALSE, recursive = FALSE)
for (i in dirs) {
  if (grepl("Data", i) == TRUE) {
    datadir <- i
    
  }
  interaction.results <-
    InteractionCounterData(p, datadir, distance.for.interaction.mm)
  
  binned.interaction.results <-
    GetBinnedInteractionTime(interaction.results, binsize.min = binsize.in.min)
  write.csv(
    binned.interaction.results,
    file = paste(datadir, "/BinnedResults.csv", sep = ""),
    row.names = FALSE
  )
}