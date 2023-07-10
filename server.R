options(warn = -1)

server <- function(input, output, session) {


# Input selectors ---------------------------------------------------------

  output$replicate_plot_input <- renderUI({
    selectInput(inputId = 'replicate_plot', label = "Run", choices = inputVariables()$run, selected = inputVariables()$run[1], multiple = T)
  })
  
  output$blast_count_plot_input <- renderUI({
    selectInput(inputId = 'blast_count_plot', label = "Number of blasts", choices = inputVariables()$blast_count, selected = inputVariables()$blast_count[1], multiple = T)
  })
  
  output$blast_duration_plot_input <- renderUI({
    selectInput(inputId = 'blast_length_plot', label = "Duration of blasts", choices = inputVariables()$blast_duration, selected = inputVariables()$blast_duration[1], multiple = T)
  })
  
  output$replicate_plot_combined_input <- renderUI({
    selectInput(inputId = 'replicate_plot_combined', label = "Run", choices = inputVariables()$run, selected = inputVariables()$run[1], multiple = T)
  })
  
  output$blast_count_plot_combined_input <- renderUI({
    selectInput(inputId = 'blast_count_plot_combined', label = "Number of blasts", choices = inputVariables()$blast_count, selected = inputVariables()$blast_count[1], multiple = T)
  })
  
  output$blast_duration_plot_combined_input <- renderUI({
    selectInput(inputId = 'blast_length_plot_combined', label = "Duration of blasts", choices = inputVariables()$blast_duration, selected = inputVariables()$blast_duration[1], multiple = T)
  })
  
  output$blast_length_plot_progresion_input <- renderUI({
    selectInput(inputId = 'blast_length_plot_progresion', label = "Blast length (seconds)", choices = inputVariables()$blast_duration, selected = inputVariables()$blast_duration, multiple = T)
  })
  
# Data input --------------------------------------------------------------
  setBackgroudColourCustom()
  dataInputReactive <- reactiveValues()
  dataInputReactive$blastData <- blastData
  dataInputReactive$metaData <- metaData
  dataInputReactive$message <- ""
  output$data_input_help_message <- renderText({
    dataInputReactive$message
  })
  
  
  observeEvent(input$save_data, {
    dataInputDF <- createNewDataFrame(input)
    myData <- dataInputClass(blastDuration = as.numeric(input$blast_length), blastCount = as.numeric(input$blast_count), 
                             replicateNum = as.numeric(input$replicate), checkData = dataInputReactive$metaData, 
                             inputDataFrame = dataInputDF, mainData = dataInputReactive$blastData)
    myData <- checkMetaDataValid(myData)
    myData <- saveData(myData)
    myData <- saveMetaData(myData)
    dataInputReactive$blastData <- myData@mainData
    dataInputReactive$metaData <- myData@checkData
    if(printDataMessage(myData) == "Please check your data"){
      shinyalert("Check your data!", "Data not saved", type = "error")
    }else{
      shinyalert("Success!", "Data saved", type = "success")
    }
    dataInputReactive$message <- printDataMessage(myData)
    
  })



# Plots -------------------------------------------------------------------

  plotData <- reactive({
    plotData <- transformData(dataInputReactive$blastData)
    plotData <- weightToProportion(plotData)
    plotData <- friendlyId(plotData)
    plotData
  })
  inputVariables <- reactive({
    ll  <- list()
    ll$run <- sort(unique(plotData()$run))
    ll$blast_count <- sort(unique(plotData()$blast_count))
    ll$blast_duration <- sort(unique(plotData()$blast_duration))
    ll
  } 
  )
  
output$all_bar_plots <- renderPlot({
  plotData <- plotData()
  selectedData <- selectData(plotData, as.numeric(input$replicate_plot), as.numeric(input$blast_count_plot), as.numeric(input$blast_length_plot), input$combine_replicates_plot)
  plotAll(selectedData, stackColours = input$stack_colours_plot)
  
})

output$columns_and_rows_plot <- renderPlot({
  plotData <- plotData()
  selectedData <- selectData(plotData, as.numeric(input$replicate_plot_combined), as.numeric(input$blast_count_plot_combined), as.numeric(input$blast_length_plot_combined), input$combine_replicates_plot_column)
  plotRows(selectedData, stackColours = input$stack_colours_plot_column)
  
})

output$progression_plot_all <- renderPlot({
  plotData <- plotData()
  plotData <- plotData %>% rbind(zeroBlasts)
  selectedData <- selectData(plotData, runVals = inputVariables()$run, blastCounts = c(0, inputVariables()$blast_count), blastLengths = as.numeric(input$blast_length_plot_progresion))
  allBlasts <- animateBlasts(selectedData, input$y.height, input$y.variance, input$hide_unchanged_data)
  plotBlastPoints(allBlasts, allDurations = T)
})

output$progression_plot_output <- renderUI({
  plotOutput(outputId = "progression_plot_all", height = paste0(input$progression_plot_height, "px"))
})

output$progression_plot <- renderPlot({
  plotData <- plotData()
  plotData <- plotData %>% rbind(zeroBlasts)
  selectedData <- selectData(plotData, runVals = c(1,2), blastCounts = c(0,1,2,3) , blastLengths = as.numeric(0.1))
  allBlasts <- animateBlasts(selectedData, input$y.height, input$y.variance, input$hide_unchanged_data)
  plotBlastPoints(allBlasts)
  
  
})

output$progression_plot_1 <- renderPlot({
  plotData <- plotData()
  plotData <- plotData %>% rbind(zeroBlasts)
  selectedData <- selectData(plotData, runVals = c(1,2), blastCounts = c(0,1) , blastLengths = as.numeric(1))
  allBlasts <- animateBlasts(selectedData, input$y.height, input$y.variance, input$hide_unchanged_data)
  plotBlastPoints(allBlasts)
  
  
}) 

output$segment_text <- renderText({
  textValx <- input$segments_click$x
  textValy <- input$segments_click$y
  if(is.null(textValx)){
    textVal <- ""
  }else{
    textVal <- paste("x:", textValx, ", y:", textValy)
  }
  textVal
})

output$column_text <- renderText({
  textValx <- input$columns_click$x
  textValy <- input$columns_click$y
  if(is.null(textValx)){
    textVal <- ""
  }else{
    textVal <- paste("x:", textValx, ", y:", textValy)
  }
  textVal
})

output$progression_0.1_text <- renderText({
  textValx <- input$click_0.1$x
  textValy <- input$click_0.1$y
  if(is.null(textValx)){
    textVal <- ""
  }else{
    textVal <- paste("x:", textValx, ", y:", textValy)
  }
  textVal
})

output$progression_1_text <- renderText({
  textValx <- input$click_1$x
  textValy <- input$click_1$y
  if(is.null(textValx)){
    textVal <- ""
  }else{
    textVal <- paste("x:", textValx, ", y:", textValy)
  }
  textVal
})





# View data ---------------------------------------------------------------
output$blastDataTable <- renderDT({
  datatable(dataInputReactive$blastData, options = list(
    columnDefs = list(list(className = 'dt-center', targets = 5)),
    pageLength = 30
  ))
  
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("blastData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dataInputReactive$blastData, file, row.names = FALSE)
    }
  )


# About tab ---------------------------------------------------------------

output$random_image  <- renderUI({
  # Return a list
  randVal <- floor(runif(n = 1, min = 1, max = 6.999))
  tags$img(src =paste0(randVal, ".jpg"))

})
  
} 






