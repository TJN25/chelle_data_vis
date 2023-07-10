load("data/zeroBlasts.Rda")

plotSegment <- function(plotData, rowVal, colVal, byColour = T, stackColours = F, combineRows = F) {
  if(combineRows){
    dat <- plotData %>% filter(column == colVal) %>% group_by(blast.id, column, Colour) %>% summarise(column.weights = sum(Weight)) %>% dplyr::rename(Weight = column.weights)
  }else{
  dat <- plotData %>% filter(row == rowVal, column == colVal)
  }
  if(byColour){
    dat <- dat %>% mutate(use_colour = ifelse(Colour == "yellow", "#FFDB33", ifelse(Colour == "red", "#FF3333", "#339BFF")))
    if(stackColours){
  p <- ggplot() + 
    geom_bar(data = dat, aes(x = blast.id, y = Weight, group = Colour, show.legend = F),fill =  dat$use_colour, position = "stack", stat = "identity") +
    ylim(c(0,180)) + 
    theme_classic()
    }else{
      p <- ggplot() + 
        geom_bar(data = dat, aes(x = blast.id, y = Weight, group = Colour, show.legend = F),fill =  dat$use_colour, position = "dodge", stat = "identity") +
        ylim(c(0,120)) + 
        theme_classic()
    }
  }else{
    p <- ggplot() + 
      geom_bar(data = dat, aes(x = blast.id, y = Weight, show.legend = F), stat = "identity") +
      ylim(c(0,180)) + 
      theme_classic()
  }
  if(combineRows){
    p <- p + ylim(0, 540)
  }
  return(p)
}

plotAll <- function(plotData, stackColours = F){
  r1c1 <- plotSegment(plotData, rowVal = 1, colVal = 1, stackColours = stackColours)
  r1c2 <- plotSegment(plotData, rowVal = 1, colVal = 2, stackColours = stackColours)
  r1c3 <- plotSegment(plotData, rowVal = 1, colVal = 3, stackColours = stackColours)
  r2c1 <- plotSegment(plotData, rowVal = 2, colVal = 1, stackColours = stackColours)
  r2c2 <- plotSegment(plotData, rowVal = 2, colVal = 2, stackColours = stackColours)
  r2c3 <- plotSegment(plotData, rowVal = 2, colVal = 3, stackColours = stackColours)
  r3c1 <- plotSegment(plotData, rowVal = 3, colVal = 1, stackColours = stackColours)
  r3c2 <- plotSegment(plotData, rowVal = 3, colVal = 2, stackColours = stackColours)
  r3c3 <- plotSegment(plotData, rowVal = 3, colVal = 3, stackColours = stackColours)
  
  all.p <- ggarrange(r1c1, r1c2, r1c3, r2c1, r2c2, r2c3,  r3c1, r3c2, r3c3 + rremove("x.text"),
                     ncol = 3, nrow = 3)
  return(all.p)
}

plotCombined <- function(plotData){
  r1c1 <- plotSegment(plotData, rowVal = 1, colVal = 1, byColour = F)
  r1c2 <- plotSegment(plotData, rowVal = 1, colVal = 2, byColour = F)
  r1c3 <- plotSegment(plotData, rowVal = 1, colVal = 3, byColour = F)
  r2c1 <- plotSegment(plotData, rowVal = 2, colVal = 1, byColour = F)
  r2c2 <- plotSegment(plotData, rowVal = 2, colVal = 2, byColour = F)
  r2c3 <- plotSegment(plotData, rowVal = 2, colVal = 3, byColour = F)
  r3c1 <- plotSegment(plotData, rowVal = 3, colVal = 1, byColour = F)
  r3c2 <- plotSegment(plotData, rowVal = 3, colVal = 2, byColour = F)
  r3c3 <- plotSegment(plotData, rowVal = 3, colVal = 3, byColour = F)
  
  all.p <- ggarrange(r1c1, r1c2, r1c3, r2c1, r2c2, r2c3,  r3c1, r3c2, r3c3 + rremove("x.text"),
                     ncol = 3, nrow = 3)
  return(all.p)
}

plotRows <- function(plotData, stackColours = F){
  r1c1 <- plotSegment(plotData, rowVal = 1, colVal = 1, byColour = T, stackColours = stackColours, combineRows = T)
  r1c2 <- plotSegment(plotData, rowVal = 1, colVal = 2, byColour = T, stackColours = stackColours, combineRows = T)
  r1c3 <- plotSegment(plotData, rowVal = 1, colVal = 3, byColour = T, stackColours = stackColours, combineRows = T)

  
  all.p <- ggarrange(r1c1, r1c2, r1c3 + rremove("x.text"),
                     ncol = 3, nrow = 1)
  return(all.p)
}

generatePoints <- function(plotData, rowVal, colVal, blastVal, y.height, y.variance) {
  selectedData <- plotData %>% filter(blast_count == blastVal, row == rowVal, column == colVal)
  selectedData <- selectedData %>% group_by(row, column, Colour) %>% summarise(weight.all = sum(Weight))
  if(blastVal == 0){
    minRow <- 3 - rowVal + y.height - y.variance
    maxRow <- 3 - rowVal + y.height + y.variance
  }else{
    minRow <- 3.001 - rowVal
    maxRow <- 4 - rowVal
  
  }
  minCol <- colVal - 0.999
  maxCol <- colVal
  yellowCount <- ceiling(selectedData$weight.all[selectedData$Colour == "yellow"]) * 1
  blueCount <- ceiling(selectedData$weight.all[selectedData$Colour == "blue"]) * 1
  redCount <- ceiling(selectedData$weight.all[selectedData$Colour == "red"]) *  1
  dat <- data.frame(x = 0, y = 0 , colour = "white")
  if(yellowCount > 0){
  yellowData <- data.frame(x = runif(yellowCount, minCol, maxCol), 
                           y = runif(yellowCount, minRow, maxRow),
                           colour = "yellow")
  dat <- dat %>% rbind(yellowData)
  }
  if(redCount > 0){
  redData <- data.frame(x = runif(redCount, minCol, maxCol), 
                           y = runif(redCount, minRow, maxRow),
                           colour = "red")
  dat <- dat %>% rbind(redData)
  
  }
  if(blueCount > 0){
  blueData <- data.frame(x = runif(blueCount, minCol, maxCol), 
                           y = runif(blueCount, minRow, maxRow),
                           colour = "blue")
  dat <- dat %>% rbind(blueData)
  }
  dat <- dat %>% mutate(use_colour = ifelse(colour == "yellow", "#E79F00", ifelse(colour == "red", "#FF3333", ifelse(colour == "blue", "#339BFF", "#FFFFFF"))))
  dat$blast_count <- blastVal
  return(dat)
}

animateBlasts <- function(plotData, y.height, y.variance, hideUnchanged) {
allBlasts <- data.frame(x = 0, y = 0 , colour = "white", use_colour = "#FFFFFF", blast_count = -1, blast_duration = -1)

blastDurations <- sort(unique(plotData$blast_duration))

for(duration in blastDurations){
  selectedData <- plotData %>% filter(blast_duration == duration)
  blastCountValues <- sort(unique(selectedData$blast_count))
for(b in blastCountValues){
  
dat <- data.frame(x = 0, y = 0 , colour = "white", use_colour = "#FFFFFF", blast_count = -1, blast_duration = -1)
for(i in 1:3){
  for(j in 1:3){
    if(b == 0){
      tmp <- generatePoints(selectedData, rowVal = i, colVal = j, blastVal = b, y.height, y.variance)
      tmp$blast_duration <- duration
      dat <- dat %>% rbind(tmp)
    }else{
      tmp <- generatePoints(selectedData, rowVal = i, colVal = j, blastVal = b)
      tmp$blast_duration <- duration
      dat <- dat %>% rbind(tmp)
    }
    
  }
}
allBlasts <- allBlasts %>%  rbind(dat)
}
}

if(hideUnchanged){
  allBlasts <- allBlasts %>%  mutate(keep = ifelse(blast_count == 0, T, 
                                                   ifelse(colour == "yellow", ifelse(y > 2, F, T),
                                                          ifelse(colour == "blue", ifelse(y > 1, ifelse(y < 2, F, T), T), 
                                                                 ifelse(colour == "red", ifelse(y < 1, F, T), T)))))
  allBlasts <- allBlasts %>% filter(keep) %>% select(-keep)
}
return(allBlasts)
#+ transition_states(blast_count, wrap = F) +
 # shadow_mark()

}

pointsPlotPerBlast <- function(allBlasts, blastCountVal){
  dat <- allBlasts %>% filter(blast_count == blastCountVal)
  p <- ggplot() + 
    geom_point(data = dat, aes(x = x, y = y, group = colour), color = dat$use_colour) +
    ylim(c(0,3)) + 
    theme_bw()
  return(p)
}

plotBlastPoints <- function(allBlasts, allDurations = F){
  if(allDurations){
  blastDurations <- sort(unique(allBlasts$blast_duration))
  blastDurations <- blastDurations[blastDurations != -1]
  numDurations <- length(blastDurations)
  plotCounts <- allBlasts %>% select(blast_count, blast_duration) %>%  unique() %>% nrow()
  maxCounts <- max(allBlasts$blast_count)
  
  plotList <- list()
  counter <- 0
  for(b in blastDurations){
    selectedData <- allBlasts %>% filter(blast_duration == b)
    blastCounts <- sort(unique(selectedData$blast_count))
    print(blastCounts)
    for(counts in blastCounts){
      counter <- counter + 1
      pp <- pointsPlotPerBlast(selectedData, counts)
      plotList[[counter]] <- pp
    }
  }
  print(plotList)
  all.p <- ggarrange(plotlist = plotList, ncol = maxCounts + 1, nrow = numDurations)
  return(all.p)
  
  }else{
    
  if(max(allBlasts$blast_count) == 1){  
    p0 <- pointsPlotPerBlast(allBlasts, 0)
    p1 <- pointsPlotPerBlast(allBlasts, 1)
    all.p <- ggarrange(p0, p1 + rremove("x.text"),
                       ncol = 4, nrow = 1)
  }else{
    p0 <- pointsPlotPerBlast(allBlasts, 0)
    p1 <- pointsPlotPerBlast(allBlasts, 1)
    p2 <- pointsPlotPerBlast(allBlasts, 2)
    p3 <- pointsPlotPerBlast(allBlasts, 3)
    plotList <- list(p0, p1, p2, p3)
    all.p <- ggarrange(plotlist = plotList,
                       ncol = 4, nrow = 1)
  }
  
  
 
  return(all.p)
  }
  
}
