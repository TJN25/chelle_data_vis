options(warn = -1)

server <- function(input, output, session) {


# Input selectors ---------------------------------------------------------

  output$replicate_plot_input <- renderUI({
    selectInput(inputId = 'replicate_plot', label = "Run", choices = inputVariables()$run, selected = inputVariables()$run[1], multiple = T)
  })
  
  output$replicate_plot_progression_input <- renderUI({
    selectInput(inputId = 'replicate_plot_progression', label = "Run", choices = inputVariables()$run, selected = inputVariables()$run, multiple = T)
  })
  
  output$blast_count_plot_input <- renderUI({
    selectInput(inputId = 'blast_count_plot', label = "Number of blasts", choices = inputVariables()$blast_count, selected = inputVariables()$blast_count[1], multiple = T)
  })
  
  output$move_plot_input <- renderUI({
    selectInput(inputId = 'move_plot', label = "Moving?", choices = inputVariables()$move_val, selected = inputVariables()$move_val[1], multiple = T)
  })
  
  output$alpha_val_input <- renderUI({
    if(input$hide_unchanged_data_radio != "hide_unchanged"){
      sliderInput(inputId = "alpha_val_progression", label = "Opacity", min = 0, max = 1, value = 0.3, step = 0.1)
    }
  })
  
  output$blast_duration_plot_input <- renderUI({
    selectInput(inputId = 'blast_length_plot', label = "Duration of blasts", choices = barPlotData()@blast_durations, selected = inputVariables()$blast_duration[1], multiple = T)
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
                             replicateNum = as.numeric(input$replicate), moveVal = as.logical(input$move_value), checkData = dataInputReactive$metaData, 
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

  barPlotData <- reactive({
    barPlotData <- new(Class = "allBarPlotsClass", blastData = blastData)
    barPlotData
  })

  progPlotData <- reactive({
    progPlotData <- new(Class = "progressionPlotClass", blastData = blastData)
    progPlotData
  })
  
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
    ll$move_val <- sort(unique(plotData()$move_val))
    ll
  } 
  )
  
output$all_bar_plots <- renderPlot({
  barPlotData <- barPlotData()
  barPlotData@moving <- as.logical(input$move_plot)
  barPlotData@stack_colours <- input$stack_colours_plot
  barPlotData@blast_duration <- as.numeric(input$blast_length_plot)
  barPlotData@blast_count <- as.numeric(input$blast_count_plot)
  barPlotData@run <- as.numeric(input$replicate_plot)
  barPlotData@combine_replicates <- input$combine_replicates_plot
  barPlotData <- selectData(barPlotData)
  barPlotData <- generatePlot(barPlotData)
  barPlotData@p
})

output$columns_and_rows_plot <- renderPlot({
  plotData <- plotData()
  selectedData <- selectData(plotData, as.numeric(input$replicate_plot_combined), as.numeric(input$blast_count_plot_combined), as.numeric(input$blast_length_plot_combined), input$combine_replicates_plot_column)
  plotRows(selectedData, stackColours = input$stack_colours_plot_column)
  
})

output$progression_plot_all <- renderPlot({
  progPlotData <- progPlotData()
  progPlotData@y.height <- input$y.height
  progPlotData@y.variance <- input$y.variance
  progPlotData@hideUnchanged <- input$hide_unchanged_data_radio
  if(input$hide_unchanged_data_radio != "hide_unchanged"){
  progPlotData@alphaVal <- input$alpha_val_progression
  }
  progPlotData@run <- as.numeric(input$replicate_plot_progression)
  progPlotData <- selectData(progPlotData)
  progPlotData@simBlast <- simulateBlasts(progPlotData)
  progPlotData <- generatePlot(progPlotData)
  progPlotData@p 
})

output$progression_plot_output <- renderUI({
  plotOutput(outputId = "progression_plot_all", height = paste0(input$progression_plot_height, "px"))
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






