# utiils ------------------------------------------------------------------

##Take input data and return a data frame

createNewDataFrame <- function(input) {

  df <- data.frame(run = input$replicate,
             blast_duration = input$blast_length,
             blast_count = input$blast_count, 
             yellow = c(input$r1_c1_input_yellow, input$r1_c2_input_yellow, input$r1_c3_input_yellow,
                        input$r2_c1_input_yellow, input$r2_c2_input_yellow, input$r2_c3_input_yellow, 
                        input$r3_c1_input_yellow, input$r3_c2_input_yellow, input$r3_c3_input_yellow),
             blue = c(input$r1_c1_input_blue, input$r1_c2_input_blue, input$r1_c3_input_blue,
                      input$r2_c1_input_blue, input$r2_c2_input_blue, input$r2_c3_input_blue, 
                      input$r3_c1_input_blue, input$r3_c2_input_blue, input$r3_c3_input_blue),
             red = c(input$r1_c1_input_red, input$r1_c2_input_red, input$r1_c3_input_red,
                     input$r2_c1_input_red, input$r2_c2_input_red, input$r2_c3_input_red, 
                     input$r3_c1_input_red, input$r3_c2_input_red, input$r3_c3_input_red),
             row = c(1, 1, 1,
                     2, 2, 2,
                     3, 3, 3),
             column = c(1, 2, 3, 1, 2, 3, 1, 2, 3)
  )
  df <- df %>% mutate(id = paste0(blast_duration, blast_count, run))
  return(df)
}

transformData <- function(blastData) {
  plotData <- blastData %>% pivot_longer(cols = c("yellow", "blue", "red")) %>% dplyr::rename(Colour = name, Weight = value)
  plotData$run <- as.numeric(plotData$run)
  plotData$blast_duration <- as.numeric(plotData$blast_duration)
  plotData$blast_count <- as.numeric(plotData$blast_count)
  return(plotData)
}

weightToProportion <- function(plotData) {
  weightTotals <- plotData %>% group_by(id, Colour) %>% summarise(weights = sum(Weight)) %>% mutate(tmp.id = paste0(id, Colour)) %>%  ungroup()%>% select(weights, tmp.id)
  plotData <- plotData %>%  mutate(tmp.id = paste0(id, Colour))
  plotData <- plotData %>% full_join(weightTotals, by = "tmp.id") %>% select(-tmp.id)
  plotData <- plotData %>% mutate(proportion = Weight/weights)
}

friendlyId <- function(plotData){
  plotData <- plotData %>% mutate(blast.id = paste("Replicate:", run, ", No. blasts: ", blast_count, ", Duration: ", blast_duration))
}

selectData <- function(plotData, runVals, blastCounts, blastLengths, combineReplicates =F){
  dat <- plotData
  if(combineReplicates){
    dat <- dat[dat$blast_count %in% blastCounts,]
    dat <- dat[dat$blast_duration %in% blastLengths,]
    dat <- dat %>% group_by(blast_duration, blast_count, row, column, Colour) %>% 
      summarise(combined.weight = sum(Weight)) %>% 
      dplyr::rename(Weight = combined.weight) %>% 
      mutate(blast.id = paste("No. blasts: ", blast_count, ", Duration: ", blast_duration))
      
  }else{
  dat <- dat[dat$run %in% runVals,]
  dat <- dat[dat$blast_count %in% blastCounts,]
    print(unique(dat$blast_duration))
    print(blastLengths)
  dat <- dat[dat$blast_duration %in% blastLengths,]
  }
  return(dat)
}