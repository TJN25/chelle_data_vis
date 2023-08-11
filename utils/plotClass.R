
# Create class ------------------------------------------------------------


allDataClass <- setClass("allDataClass", 
                         slots = list(blast_durations="numeric", blast_counts="numeric", 
                                      runs="numeric", blastData="data.frame", plotData = "data.frame"))

selectedDataClass <- setClass("selectedDataClass", representation(selectedData = "data.frame", blast_duration="numeric", moving = "logical", 
                                                                  blast_count="numeric", run="numeric", 
                                                                  combine_replicates = "logical") ,
                              prototype (combine_replicates=F),
                              contains = "allDataClass")

allBarPlotsClass <- setClass("allBarPlotsClass", representation(p = "list", stack_colours = "logical"), 
                             prototype(stack_colours = T),
                             contains = "selectedDataClass")

progressionPlotClass <- setClass("progressionPlotClass", representation(p = "list", y.height = "numeric", 
                                                                        y.variance = "numeric", hideUnchanged = "character", 
                                                                        simBlast = "data.frame", extraData = "data.frame", alphaVal = "numeric", 
                                                                        bead_weight = "numeric",
                                                                        colours_prog = "character"),
                                 prototype(hideUnchanged = "hide_unchanged", y.variance = 0.5, y.height = 0.5, alphaVal = 0.3, bead_weight = 0.2, colours_prog = c("yellow", "blue", "red")), 
                                 contains = "selectedDataClass")

# allDataClass functions -----------------------------------------------------------

#check_dataframe <-function(object){
#  if(nrow(object@blastData) == 0)return("Blast Data must be provided")
#  TRUE
#}

setMethod("initialize", "allDataClass", function(.Object, ...) {
  .Object <- callNextMethod(.Object, ...)
  if(nrow(.Object@blastData) > 0){
    .Object <- assignColumnTypes(.Object)
    .Object <- getValuesLists(.Object)
    .Object <- processData(.Object)
  }
  .Object
  
})



setGeneric("assignColumnTypes", function(x) standardGeneric("assignColumnTypes"))
setMethod("assignColumnTypes", signature = c("allDataClass"), 
          function(x){
            x@blastData$blast_duration <- as.numeric(x@blastData$blast_duration)
            x@blastData$blast_count <-  as.numeric(x@blastData$blast_count)
            x@blastData$run <-  as.numeric(x@blastData$run)
            return(x)
          })

setGeneric("getValuesLists", function(x) standardGeneric("getValuesLists"))
setMethod("getValuesLists", signature = c("allDataClass"), 
          function(x){
            x@blast_durations <-  sort(unique(x@blastData$blast_duration))
            x@blast_counts <-  sort(unique(x@blastData$blast_count))
            x@runs <-  sort(unique(x@blastData$run))
            return(x)
          })

setGeneric("processData", function(x) standardGeneric("processData"))
setMethod("processData", signature = c("allDataClass"), 
          function(x){
            plotData <- transformData(x@blastData)
            plotData <- weightToProportion(plotData)
            x@plotData <- friendlyId(plotData)
            return(x)
          })
setMethod("processData", signature = c("progressionPlotClass"), 
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

setGeneric("transformData", function(blastData) standardGeneric("transformData"))
setMethod("transformData", signature = c(blastData = "data.frame"), 
          function(blastData){
            plotData <- blastData %>% pivot_longer(cols = c("yellow", "blue", "red")) %>% dplyr::rename(Colour = name, Weight = value)
            return(plotData)
          })

setGeneric("weightToProportion", function(plotData) standardGeneric("weightToProportion"))
setMethod("weightToProportion", signature = c(plotData = "data.frame"), 
          function(plotData){
            weightTotals <- plotData %>% group_by(id, Colour) %>% summarise(weights = sum(Weight)) %>% mutate(tmp.id = paste0(id, Colour)) %>%  ungroup()%>% select(weights, tmp.id)
            plotData <- plotData %>%  mutate(tmp.id = paste0(id, Colour))
            plotData <- plotData %>% full_join(weightTotals, by = "tmp.id") %>% select(-tmp.id)
            plotData <- plotData %>% mutate(proportion = Weight/weights)
            return(plotData)
          })

setGeneric("friendlyId", function(plotData) standardGeneric("friendlyId"))
setMethod("friendlyId", signature = c(plotData = "data.frame"), 
          function(plotData){
            plotData <- plotData %>% mutate(blast.id = paste("Replicate:", run, ", No. blasts: ", 
                                                             blast_count, ", Duration: ", blast_duration, ifelse(move_val, ", moved", ", static")))
            return(plotData)
          })

# selectedDataClass functions -----------------------------------------------------------


setMethod("initialize", "selectedDataClass", function(.Object, ...) {
  .Object <- callNextMethod(.Object, ...)
  if(length(.Object@blast_duration) == 0) .Object@blast_duration <- .Object@blast_durations
  if(length(.Object@blast_count) == 0) .Object@blast_count <- .Object@blast_counts
  if(length(.Object@run) == 0) .Object@run <- .Object@runs
  if(length(.Object@moving) == 0) .Object@moving <- c(T, F)
  .Object <- selectData(.Object)
  .Object
})

setGeneric("selectData", function(x) standardGeneric("selectData"))
setMethod("selectData", signature = c(x = "selectedDataClass"), 
          function(x){
            blastCounts <- x@blast_count
            blastLengths <- x@blast_duration
            runVals <- x@run
            combineReplicates <- x@combine_replicates
            moveVals <- x@moving
            dat <- x@plotData
            if(combineReplicates){
              dat <- dat[dat$blast_count %in% blastCounts,]
              dat <- dat[dat$blast_duration %in% blastLengths,]
              dat <- dat[dat$move_val %in% moveVals,]
              dat <- dat %>% group_by(blast_duration, blast_count, row, move_val, column, Colour) %>% 
                summarise(combined.weight = sum(Weight)) %>% 
                dplyr::rename(Weight = combined.weight) %>% 
                mutate(blast.id = paste("No. blasts: ", 
                                        blast_count, ", Duration: ", 
                                        blast_duration, ifelse(move_val, ", moving", ", static")))
              
            }else{
              dat <- dat[dat$run %in% runVals,]
              dat <- dat[dat$blast_count %in% blastCounts,]
              dat <- dat[dat$blast_duration %in% blastLengths,]
              dat <- dat[dat$move_val %in% moveVals,]
            }
            
            x@selectedData <- dat
            return(x)
          })

# plotsClass functions -----------------------------------------------------------



setMethod("initialize", "allBarPlotsClass", function(.Object, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object
})

setMethod("initialize", "progressionPlotClass", function(.Object, ...) {
  .Object <- callNextMethod(.Object, ...)
  .Object <- includeMissingPostions(.Object)
  .Object <- processData(.Object)
  .Object@simBlast <- simulateBlasts(.Object)
  .Object
})


setGeneric("updateValues", function(x, ...) standardGeneric("updateValues"))
setMethod("updateValues", signature = c("allBarPlotsClass"), 
          function(x, ...){
            args <- as.list( sys.call() )
            args <- args[c(3:length(args))]
            argNames <- names(args)
            for(i in 1:length(args)){
              slotType <- typeof(slot(x, argNames[i]))
              if(slotType == "logical"){
                slot(x, argNames[i], check = TRUE) <- as.logical(as.character(args[[i]]))
              }else{
                val <- as.character(args[[i]])
                val <- val[2:length(val)]
                slot(x, argNames[i], check = TRUE) <- as.numeric(val)
              }
            }
            x <- selectData(x)
            return(x)
          })



setGeneric("generatePlot", function(x) standardGeneric("generatePlot"))
setMethod("generatePlot", signature = c("allBarPlotsClass"), 
          function(x){
            p <- plotData(x)
            ll <- list()
            ll$p <- p
            x@p <- ll
            return(x)
          })

setMethod("generatePlot", signature = c("progressionPlotClass"), 
          function(x){
            cat("ploting\n")
            p <- plotBlastPoints(x@simBlast)
            ll <- list()
            ll$p <- p
            x@p <- ll
            return(x)
          })


# Extra functions ---------------------------------------------------------


setGeneric("plotData", function(x) standardGeneric("plotData"))
setMethod("plotData", signature = c("allBarPlotsClass"), 
          function(x){
            plotData <- x@selectedData  %>% mutate(use_colour = ifelse(Colour == "yellow", "#FFDB33", ifelse(Colour == "red", "#FF3333", "#339BFF")))
            if(x@stack_colours){
              stackColours = "stack"
            }else{
              stackColours = "dodge"
            }
            if(x@combine_replicates){
              plotData <- plotData %>% arrange(move_val, blast_duration, blast_count)
            }else{
              plotData <- plotData %>% arrange(move_val, blast_duration, blast_count, run)
            }
            plotData$blast.id <- factor(plotData$blast.id, levels = unique(plotData$blast.id))
            p <- ggplot(data = plotData) + 
              geom_bar(aes(x = blast.id, y = Weight, group = Colour),fill =  plotData$use_colour, position = stackColours, stat = "identity") +
              facet_grid(row ~ column, labeller = labeller(row = rowLabel,
                                                           column = columnLabel))
            p <- p + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                           panel.background = element_rect(fill = "white", colour = "grey50"))
            return(p)
          })

setGeneric("includeMissingPostions", function(x) standardGeneric("includeMissingPostions"))
setMethod("includeMissingPostions", signature = c("progressionPlotClass"), 
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

setGeneric("simulateBlasts", function(x) standardGeneric("simulateBlasts"))
setMethod("simulateBlasts", signature = c(x = "progressionPlotClass"), 
          function(object){
            set.seed(101)
            hideUnchanged <- object@hideUnchanged
            alpha.val <- object@alphaVal
            y.height <- object@y.height
            y.variance <- object@y.variance
            bead_weight <- object@bead_weight
            colours_prog <- object@colours_prog
            
            selectedData <- object@selectedData %>% mutate(value = ceiling(Weight * 1/bead_weight)) %>% 
              uncount(value) %>% select(run, blast_duration, blast_count, row, column, move_val, Colour) 
            selectedData <- selectedData %>% 
              mutate(x = runif(nrow(selectedData ), 0.001, 0.999),
                     y = runif(nrow(selectedData ), 0.001, 0.999))
            
            extraBlasts <- object@extraData %>% mutate(value = ceiling(Weight* 1/bead_weight)) %>% 
              uncount(value) %>% select(run, blast_duration, blast_count, row, column, move_val, Colour) 
            extraBlasts <- extraBlasts %>% 
              mutate(x = runif(nrow(extraBlasts), 0.001, 0.999),
                     y = runif(nrow(extraBlasts), y.height - y.variance, y.height + y.variance))
            
            
            
            allBlasts <- selectedData %>% rbind(extraBlasts) %>% mutate(y = y + 3 - row,
                                              x = x + column - 1) %>% 
              mutate(use_colour = ifelse(Colour == "yellow", "#E79F00", ifelse(Colour == "red", "#FF3333", ifelse(Colour == "blue", "#339BFF", "#FFFFFF")))) %>% 
              dplyr::rename(colour = Colour) %>% 
              select(x, y, colour, use_colour, blast_count, blast_duration, move_val)
            
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
            allBlasts <- allBlasts %>% mutate(bead_weight = bead_weight + 0.5)
            
            allBlasts <- allBlasts %>% filter(colour %in% colours_prog)
            
            return(allBlasts)
            
          })

setGeneric("plotBlastPoints", function(allBlasts) standardGeneric("plotBlastPoints"))
setMethod("plotBlastPoints", signature = c(allBlasts = "data.frame"), 
          function(allBlasts){
            bead_weight <- allBlasts$bead_weight[1]
            allBlasts <- allBlasts %>% filter(blast_count > -1)
            blast0.11 <- allBlasts %>% filter(blast_count == 1, blast_duration == 0.1, move_val == F) %>% mutate(x_cat = 1, y_cat = "Duration of blasts (1 blast)")
            allBlasts <- allBlasts %>% mutate(x_cat = ifelse(blast_duration == 0.1, blast_count, ifelse(blast_count == 0, 0, blast_duration + 1)), 
                                              y_cat = ifelse(blast_duration == 0.1, ifelse(move_val, "Number of blasts (duration = 0.1s) moving", "Number of blasts (duration = 0.1s) static"), "Duration of blasts (1 blast)"))
            allBlasts$y_cat <- factor(allBlasts$y_cat, levels = c("Number of blasts (duration = 0.1s) static", 
                                                                  "Number of blasts (duration = 0.1s) moving",
                                                                  "Duration of blasts (1 blast)"))
            allBlasts <- allBlasts %>% rbind(blast0.11)
            allBlasts <- allBlasts %>% mutate(keep.2 = ifelse(blast_duration == 2 & blast_count == 0, F, ifelse(blast_duration == 1 & move_val, F, T))) %>% filter(keep.2) %>% select(-keep.2)
            p <- ggplot(data = allBlasts) + 
              geom_point(aes(x = x, y = y, group = colour), size = bead_weight, color = allBlasts$use_colour, alpha = allBlasts$alphaVal) +
              facet_grid(y_cat ~ x_cat, labeller = labeller(x_cat = countLabel)) +
              theme_bw()
            return(p)
            
          })



