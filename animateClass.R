animateClass <- setClass("animateClass", representation(p = "list", y.height = "numeric", 
                                                        y.variance = "numeric", hideUnchanged = "character", 
                                                        simBlast = "data.frame", extraData = "data.frame", alphaVal = "numeric"),
                         prototype(hideUnchanged = "hide_unchanged", y.variance = 0.5, y.height = 0.5, alphaVal = 0.3), 
                         contains = "selectedDataClass")



setMethod("initialize", "animateClass", function(.Object, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object <- includeMissingPostions(.Object)
  .Object <- processData(.Object)
  .Object@simBlast <- simulateBlasts(.Object)
  .Object
})


setMethod("generatePlot", signature = c("animateClass"), 
          function(x){
            cat("ploting\n")
            p <- animateBlastPoints(x@simBlast)
            ll <- list()
            ll$p <- p
            x@p <- ll
            return(x)
          })

setMethod("processData", signature = c("animateClass"), 
          function(x){
            if(nrow(x@extraData) > 0){
              extraData <- transformData(x@extraData)
              extraData <- weightToProportion(extraData)
              x@extraData <- friendlyId(extraData)
            }
            if(nrow(x@blastData) > 0){
              plotData <- transformData(x@blastData)
              plotData <- weightToProportion(plotData)
              x@plotData <- friendlyId(plotData)
            }
            return(x)
          })

setMethod("includeMissingPostions", signature = c("animateClass"), 
          function(x){
            extraData <- data.frame(run = -1, blast_duration = -1, 
                                    blast_count = -1, yellow = 0, blue = 0, 
                                    red = 0, row = -1, column = -1, 
                                    id = "000", move_val = F)
            rowsVector <- c(1, 2, 3)
            colsVector <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
            for(runVal in x@runs){
              for(blastDuration in x@blast_durations){
                blast_counts <- c(0, x@blast_counts)
                for(blastCount in blast_counts){
                  for(m in 1:length(unique(x@plotData$move_val))){
                    moveText <- ifelse(m == 1, "", "moving")
                    moveLogical <- ifelse(m == 1, F, T)
                    idVal <- paste0(blastDuration, blastCount, runVal, moveText)
                    if(idVal %in% x@plotData$id){
                      next
                    }
                    df <- data.frame(run = runVal, blast_duration = blastDuration, 
                                     blast_count = blastCount, yellow = 0, blue = 0, 
                                     red = 0, row = rowsVector, column = colsVector, 
                                     id = idVal, move_val = moveLogical)
                    if(blastCount == 0){
                      df$yellow[df$row == 1] <- 60
                      df$blue[df$row == 2] <- 60
                      df$red[df$row == 3] <- 60
                    }
                    extraData <- extraData %>% rbind(df)
                  }
                }
              }
              
            }
            extraData <- extraData %>% filter(run > -1)
            x@extraData <- extraData
            return(x)
          })

setMethod("simulateBlasts", signature = c(x = "animateClass"), 
          function(x){
            plotData <- x@selectedData %>% rbind(x@extraData)
            y.height <- x@y.height
            y.variance <- x@y.variance
            hideUnchanged <- x@hideUnchanged
            alpha.val <- x@alphaVal
            allBlasts <- data.frame(x = 0, y = 0 , colour = "white", use_colour = "#FFFFFF", blast_count = -1, blast_duration = -1, move_val = F)
            
            blastDurations <- sort(unique(plotData$blast_duration))
            
            for(duration in blastDurations){
              selectedData <- plotData %>% filter(blast_duration == duration)
              blastCountValues <- sort(unique(selectedData$blast_count))
              for(b in blastCountValues){
                
                dat <- data.frame(x = 0, y = 0 , colour = "white", use_colour = "#FFFFFF", blast_count = -1, blast_duration = -1, move_val = F)
                for(i in 1:3){
                  for(j in 1:3){
                    for(m in 1:length(unique(selectedData$move_val))){
                      if(b == 0){
                        tmp <- generatePointsForAnimation(selectedData, rowVal = i, colVal = j, blastVal = b, y.height, y.variance, move_num = m)
                        tmp$blast_duration <- duration
                        dat <- dat %>% rbind(tmp)
                      }else{
                        tmp <- generatePointsForAnimation(selectedData, rowVal = i, colVal = j, blastVal = b, move_num = m)
                        tmp$blast_duration <- duration
                        dat <- dat %>% rbind(tmp)
                      }
                    }  
                    
                    
                  }
                }
                allBlasts <- allBlasts %>%  rbind(dat)
              }
            }
            allBlasts <- allBlasts %>% mutate(alphaVal = 1)
            if(hideUnchanged == "hide_unchanged"){
              allBlasts <- allBlasts %>%  mutate(keep = ifelse(blast_count == 0, T, 
                                                               ifelse(colour == "yellow", ifelse(y > 2, F, T),
                                                                      ifelse(colour == "blue", ifelse(y > 1, ifelse(y < 2, F, T), T), 
                                                                             ifelse(colour == "red", ifelse(y < 1, F, T), T)))))
              allBlasts <- allBlasts %>% filter(keep) %>% select(-keep)
            }else if(hideUnchanged == "grey_unchanged"){
              allBlasts <- allBlasts %>%  mutate(keep = ifelse(blast_count == 0, T, 
                                                               ifelse(colour == "yellow", ifelse(y > 2, F, T),
                                                                      ifelse(colour == "blue", ifelse(y > 1, ifelse(y < 2, F, T), T), 
                                                                             ifelse(colour == "red", ifelse(y < 1, F, T), T)))))
              allBlasts <- allBlasts %>% mutate(colour = ifelse(keep, colour, "grey"), use_colour = ifelse(keep, use_colour, "#d1d1d1"), alphaVal = ifelse(keep, 1, alpha.val)) %>% select(-keep)
            }else{
              allBlasts <- allBlasts %>%  mutate(keep = ifelse(blast_count == 0, T, 
                                                               ifelse(colour == "yellow", ifelse(y > 2, F, T),
                                                                      ifelse(colour == "blue", ifelse(y > 1, ifelse(y < 2, F, T), T), 
                                                                             ifelse(colour == "red", ifelse(y < 1, F, T), T)))))
              allBlasts <- allBlasts %>% mutate(alphaVal = ifelse(keep, 1, alpha.val)) %>% select(-keep)
            }
            return(allBlasts)
            
          })

setGeneric("generatePointsForAnimation", function(plotData, rowVal, colVal, blastVal, y.height, y.variance, move_num) standardGeneric("generatePointsForAnimation"))
setMethod("generatePointsForAnimation", signature = c(plotData = "data.frame"), 
          function(plotData, rowVal, colVal, blastVal, y.height = 0.5, y.variance = 0.45, move_num){
            set.seed(101)
            if(move_num == 1){
              moveVal = F
            }else{
              moveVal = T
            }
            selectedData <- plotData %>% filter(blast_count == blastVal, row == rowVal, column == colVal, move_val == moveVal)
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
            dat$move_val <- moveVal
            return(dat)
          })

setGeneric("animateBlastPoints", function(allBlasts) standardGeneric("animateBlastPoints"))
setMethod("animateBlastPoints", signature = c(allBlasts = "data.frame"), 
          function(allBlasts){
            allBlasts <- allBlasts %>% filter(blast_count > -1)
            blast0.11 <- allBlasts %>% filter(blast_count == 1, blast_duration == 0.1, move_val == F) %>% mutate(x_cat = 1, y_cat = "Duration of blasts (1 blast)")
            allBlasts <- allBlasts %>% mutate(x_cat = ifelse(blast_duration == 0.1, blast_count, ifelse(blast_count == 0, 0, blast_duration + 1)), 
                                              y_cat = ifelse(blast_duration == 0.1, ifelse(move_val, "Number of blasts (duration = 0.1s) moving", "Number of blasts (duration = 0.1s) static"), "Duration of blasts (1 blast)"))
            allBlasts$y_cat <- factor(allBlasts$y_cat, levels = c("Number of blasts (duration = 0.1s) static", 
                                                                  "Number of blasts (duration = 0.1s) moving",
                                                                  "Duration of blasts (1 blast)"))
            allBlasts <- allBlasts %>% rbind(blast0.11)
            p <- ggplot(data = allBlasts) + 
              geom_point(aes(x = x, y = y, group = colour), color = allBlasts$use_colour, alpha = allBlasts$alphaVal) +
              facet_grid(y_cat ~ x_cat, labeller = labeller(x_cat = countLabel)) +
              theme_bw()
            return(p)
            
          })


load("tmp/mainData.Rda")
animateData <- new(Class = "animateClass", blastData = blastData)
animateData@simBlast <- simulateBlasts(animateData)

animateData <- generatePlot(animateData)
animateData@p


dat <- animateData@selectedData %>% rbind(animateData@extraData)


dat <- dat %>% filter(blast_duration == 0.1, move_val == F)


