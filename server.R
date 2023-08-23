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
  
  output$blast_count_table_input <- renderUI({
    simBlast <- progInit()@simBlast
    selectInput(inputId = 'blast_count_table', label = "Blast count", choices = unique(simBlast$blast_count), selected = unique(simBlast$blast_count[simBlast$blast_count > 0]), multiple = T)
  })
  
  output$blast_duration_table_input <- renderUI({
    simBlast <- progInit()@simBlast
    selectInput(inputId = 'blast_duration_table', label = "Blast length (seconds)", choices = unique(simBlast$blast_duration), selected = unique(simBlast$blast_duration), multiple = T)
  })
  
  output$run_table_input <- renderUI({
    simBlast <- progInit()@simBlast
    selectInput(inputId = 'run_table', label = "Run", choices = unique(simBlast$run), selected = unique(simBlast$run), multiple = T)
  })
  
  output$move_val_table_input <- renderUI({
    simBlast <- progInit()@simBlast
    selectInput(inputId = 'move_val_table', label = "Moving?", choices = unique(simBlast$move_val), selected = unique(simBlast$move_val), multiple = T)
  })
  
  output$row_val_table_input <- renderUI({
    simBlast <- progInit()@simBlast
    selectInput(inputId = 'row_val_table', label = "Rows", choices = unique(ceiling(simBlast$x)), selected = unique(ceiling(simBlast$x)), multiple = T)
  })
  
  output$col_val_table_input <- renderUI({
    simBlast <- progInit()@simBlast
    selectInput(inputId = 'col_val_table', label = "Columns", choices = unique(ceiling(simBlast$y)), selected = unique(ceiling(simBlast$y)), multiple = T)
  })
  
  output$group_by_table_input <- renderUI({
    simBlast <- progInit()@simBlast
    selectInput(inputId = 'group_by_table', label = "Group by", choices = c("blast_count", "blast_duration", "move_val", "run", "colour", "X", "Y"), selected = c("blast_count", "blast_duration", "move_val"), multiple = T)
  })
  
  output$prog_table_x_input <- renderUI({
    progSummary <- progSummary()@progSummary
    selectInput(inputId = 'prog_table_x', label = "X-axis", choices = colnames(progSummary), selected = "blast_count", multiple = F)
  })
  
  output$prog_table_y_input <- renderUI({
    progSummary <- progSummary()@progSummary
    selectInput(inputId = 'prog_table_y', label = "Y-axis", choices = colnames(progSummary), selected = "num.of.beads.shown", multiple = F)
  })
  
  output$colour_by_prog_input <- renderUI({
    progSummary <- progSummary()@progSummary
    selectInput(inputId = 'colour_by_prog', label = "Colour by: ", choices = c("-", colnames(progSummary)), selected = "-", multiple = F)
  })

  output$shape_by_prog_input <- renderUI({
    progSummary <- progSummary()@progSummary
    selectInput(inputId = 'shape_by_prog', label = "Shape by: ", choices = c("-", colnames(progSummary)), selected = "-", multiple = F)
  })
  
    
  output$show_trend_options <- renderUI({
    fluidRow(
      column(width = 2, checkboxInput(inputId = "show_trend_line", label = "Show trend line", 
                                      value = T)),
      column(width = 2, checkboxInput(inputId = "show_confidence_interval", label = "Show confidence interval", 
                                      value = F)),
      column(width = 2, sliderInput(inputId = "point_size_prog", label = "Point size", min = 0.1, max = 5, value = 2, step = 0.1))
    )
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

  barPlotInit <- reactive({
    barPlotInit <- new(Class = "allBarPlotsClass", blastData = blastData)
    barPlotInit
  })
  
  barPlotData <- reactive({
    barPlotData <- barPlotInit()
    barPlotData@moving <- as.logical(input$move_plot)
    barPlotData@stack_colours <- input$stack_colours_plot
    barPlotData@blast_duration <- as.numeric(input$blast_length_plot)
    barPlotData@blast_count <- as.numeric(input$blast_count_plot)
    barPlotData@run <- as.numeric(input$replicate_plot)
    barPlotData@combine_replicates <- input$combine_replicates_plot
    barPlotData <- selectData(barPlotData)
    barPlotData <- generatePlot(barPlotData)
    barPlotData
  })
  
  progInit <- reactive({
    progPlotData <- new(Class = "progressionPlotClass", blastData = blastData)
    progPlotData
  })
  
  progPlotData <- reactive({
    progPlotData <- progInit()
    progPlotData@bead_weight <- as.numeric(input$bead_weight)
    progPlotData@colours_prog <- input$colours_prog
    progPlotData@y.variance <- input$y.variance
    progPlotData@hideUnchanged <- input$hide_unchanged_data_radio
    if(input$hide_unchanged_data_radio != "hide_unchanged"){
      progPlotData@alphaVal <- input$alpha_val_progression
    }
    progPlotData@run <- as.numeric(input$replicate_plot_progression)
    progPlotData <- selectData(progPlotData)
    progPlotData@simBlast <- simulateBlasts(progPlotData)
    progPlotData <- generatePlot(progPlotData)
    progPlotData
  })
  
  progSummary <- reactive({
    progPlotData <- progInit()
    progPlotData@bead_weight <- as.numeric(input$bead_weight_table)
    progPlotData@colours_prog <- input$colours_prog_table
    progPlotData@hideUnchanged <- input$hide_unchanged_data_radio_table
    progPlotData@run <- as.numeric(input$run_table)
    progPlotData@blast_count <- as.numeric(input$blast_count_table)
    progPlotData@blast_duration <- as.numeric(input$blast_duration_table)
    progPlotData@run <- as.numeric(input$run_table)
    progPlotData@moving <- as.logical(input$move_val_table)
    progPlotData@row_val <- as.numeric(input$row_val_table)
    progPlotData@col_val <- as.numeric(input$col_val_table)
    progPlotData@list.group <- as.list(input$group_by_table)
    progPlotData <- selectData(progPlotData)
    progPlotData@simBlast <- simulateBlasts(progPlotData)
    progSummary <- summariseData(progPlotData)
    progSummary
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
  } )
  
output$all_bar_plots <- renderPlot({
  barPlotData <- barPlotData()
  barPlotData@p[[1]]
#  barPlotData@p
})

output$columns_and_rows_plot <- renderPlot({
  plotData <- plotData()
  selectedData <- selectData(plotData, as.numeric(input$replicate_plot_combined), as.numeric(input$blast_count_plot_combined), as.numeric(input$blast_length_plot_combined), input$combine_replicates_plot_column)
  plotRows(selectedData, stackColours = input$stack_colours_plot_column)
  
})

output$progression_plot_all <- renderPlot({
  progPlotData()@p

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


output$progSummaryPlot <- renderPlot({
  progSummary <- progSummary()@progSummary
  x_val <- input$prog_table_x
  x_val <- as.symbol(x_val)
  y_val <- input$prog_table_y
  y_val <- as.symbol(y_val)
  progSummary <- progSummary %>% mutate(num.of.beads.shown = ifelse(blast_count == 0, 0, num.of.beads.shown))
  if(input$colour_by_prog == "-" & input$shape_by_prog == "-"){
    p <- ggplot(data = progSummary, aes(x = !!x_val, y = !!y_val)) +
      geom_point(size = input$point_size_prog) 
    if(input$show_trend_line){
    p <- p + stat_smooth(method = "lm", col = "red", se=input$show_confidence_interval)
    }
    p + theme_classic()
    
  }else{
    dat <- progSummary
    if(input$colour_by_prog != "-"){
      colour_by <- as.symbol(input$colour_by_prog)
      categories <- unique(unlist(progSummary[input$colour_by_prog]))
      fullSet <- unlist(progSummary[input$colour_by_prog])
      
      counter <- 0
      print(categories)
      for(cat in categories){
        counter <- counter + 1
        dat$colour_val[fullSet == cat] <- as.character(counter)
      }
    }else{
      dat$colour_val <- as.character(1)
    }
    if(input$shape_by_prog != "-"){
      shape_by <- as.symbol(input$shape_by_prog)
      categories <- unique(unlist(progSummary[input$shape_by_prog]))
      fullSet <- unlist(progSummary[input$shape_by_prog])
      
      counter <- 0
      print(categories)
      for(cat in categories){
        counter <- counter + 1
        dat$shape_val[fullSet == cat] <- as.character(counter)
      }
    }else{
      dat$shape_val <- as.character(1)
    }  
    dat <- dat %>% mutate(plot.id = paste0(colour_val, shape_val))
    
    if(input$colour_by_prog == "colour"){
      dat <- dat %>% mutate(colour_val = ifelse(colour == "yellow", "#E79F00", ifelse(colour == "red", "#FF3333", ifelse(colour == "blue", "#339BFF", "#FFFFFF"))))
      p <- ggplot(data = dat, aes(x = !!x_val, y = !!y_val, group = plot.id, shape = shape_val)) +
        geom_point( color = dat$colour_val, size = input$point_size_prog)
      
      if(input$show_trend_line){
      p <- p + stat_smooth(method = "lm", col = "red", se=input$show_confidence_interval)
      }
      p <- p + theme_classic()
      p
    }else{
    p <- ggplot(data = dat, aes(x = !!x_val, y = !!y_val, group = plot.id, color = colour_val, shape = shape_val)) +
          geom_point(size = input$point_size_prog) 
      
    if(input$show_trend_line){
      p <- p + stat_smooth(method = "lm", col = "red", se=input$show_confidence_interval)
    } 
    p <- p + theme_classic()
      p
    }
  }
  
})

output$progTrend <- renderPrint({
  progSummary <- progSummary()@progSummary
  x_val <- input$prog_table_x
  x_val <- as.symbol(x_val)
  y_val <- input$prog_table_y
  y_val <- as.symbol(y_val)
  progSummary <- progSummary %>% mutate(num.of.beads.shown = ifelse(blast_count == 0, 0, num.of.beads.shown)) 
  xDat <- unlist(progSummary[input$prog_table_x])
  yDat <- unlist(progSummary[input$prog_table_y])
  
  if(input$colour_by_prog != "-" & input$shape_by_prog != "-"){
    colour_by <- as.symbol(input$colour_by_prog)
    shape_by <- as.symbol(input$shape_by_prog)
    progSummary <- progSummary %>% mutate(x = !!x_val, y = !!y_val) 
    fitted_models <- progSummary %>% group_by(!!colour_by, !!shape_by) %>% do(model = lm(y~x, data = .))
    textData <- c(paste0("Grouped by: ", colour_by, " and ", shape_by))
    
    for(i in 1:nrow(fitted_models)){
      intVal <- round(summary(fitted_models$model[[i]])$coefficients[1],2)
      gradVal <- round(summary(fitted_models$model[[i]])$coefficients[2],2)
      rsqrVal <- round(summary(fitted_models$model[[i]])$adj.r.squared, 3)
      tmpText <- paste0(colour_by, " value: ", fitted_models[i,1], ", ", shape_by, " value: ", fitted_models[i,2],  ", Intercept: ", intVal, ", Gradient: ", gradVal, ", R squared: ", rsqrVal)
      textData <- c(textData, tmpText)
    }
    textData
  }else if(input$colour_by_prog != "-" & input$shape_by_prog == "-"){
    colour_by <- as.symbol(input$colour_by_prog)
    progSummary <- progSummary %>% mutate(x = !!x_val, y = !!y_val) 
    fitted_models <- progSummary %>% group_by(!!colour_by) %>% do(model = lm(y~x, data = .))
    textData <- c(paste0("Grouped by: ", colour_by))
    for(i in 1:nrow(fitted_models)){
      intVal <- round(summary(fitted_models$model[[i]])$coefficients[1],2)
      gradVal <- round(summary(fitted_models$model[[i]])$coefficients[2],2)
      rsqrVal <- round(summary(fitted_models$model[[i]])$adj.r.squared, 3)
      tmpText <- paste0(colour_by, " value: ", fitted_models[i,1], ", Intercept: ", intVal, ", Gradient: ", gradVal, ", R squared: ", rsqrVal)
      textData <- c(textData, tmpText)
    }
    textData
  }else if(input$colour_by_prog == "-" & input$shape_by_prog != "-"){
    shape_by <- as.symbol(input$shape_by_prog)
    progSummary <- progSummary %>% mutate(x = !!x_val, y = !!y_val) 
    fitted_models <- progSummary %>% group_by(!!shape_by) %>% do(model = lm(y~x, data = .))
    textData <- c(paste0("Grouped by: ", shape_by))
    for(i in 1:nrow(fitted_models)){
      intVal <- round(summary(fitted_models$model[[i]])$coefficients[1],2)
      gradVal <- round(summary(fitted_models$model[[i]])$coefficients[2],2)
      rsqrVal <- round(summary(fitted_models$model[[i]])$adj.r.squared, 3)
      tmpText <- paste0(shape_by, " value: ", fitted_models[i,1], ", Intercept: ", intVal, ", Gradient: ", gradVal, ", R squared: ", rsqrVal)
      textData <- c(textData, tmpText)
    }
    textData
  }else{
   fit <- lm(yDat ~ xDat)
   textData <- paste0("Intercept: ", round(fit$coefficients[1], 2), ", Gradient: ", round(fit$coefficients[2], 2), ", R squared: ", round(summary(fit)$adj.r.squared, 3))
   textData
  }
   })

output$save_plot_1 <- downloadHandler(
  filename = function() {
    paste("barplot", ".jpg", sep = "")
  },
  content = function(file) {
    barPlotData <- barPlotData()
    p <- barPlotData@p[[1]]
    ggsave(filename = file, plot = p, device = "svg", height = 500, units = "px")
  }
)

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

  
  
  
  output$progTable <- renderDT({
    

    datatable(progSummary()@progSummary, options = list(
      pageLength = 15
    ))
  })

# About tab ---------------------------------------------------------------

output$random_image  <- renderUI({
  # Return a list
  randVal <- floor(runif(n = 1, min = 1, max = 6.999))
  tags$img(src =paste0(randVal, ".jpg"))

})
  
  

# Image selecting ---------------------------------------------------------
  
observeEvent(input$myFile, {
    inFile <- input$myFile
    if (is.null(inFile))
      return()
    file.remove("tmp/upload.jpg")
    file.copy(inFile$datapath, file.path("tmp", "upload.jpg") )
  })

  
output$set_image <- renderImage({
      # Return a list
  input$myFile
      list(src = "tmp/upload.jpg",
           alt = "This is alternate text")
})

  image_clicks <- reactiveValues(x_vals = NULL, y_vals = NULL, line_val = NULL)



observeEvent(input$image_click_id, {

  if (is.null(image_clicks$x_vals)) {
    image_clicks$x_vals <- input$image_click_id$x
    image_clicks$y_vals <- input$image_click_id$y
    image_clicks$line_val <- input$line_position
  } else {
    image_clicks$x_vals <- c(image_clicks$x_vals, input$image_click_id$x)
    image_clicks$y_vals <- c(image_clicks$y_vals, input$image_click_id$y)
    image_clicks$line_val <- c(image_clicks$line_val, input$line_position)
  }
})

output$image_click_output <- renderPrint({
  data.frame(x = image_clicks$x_vals, y = image_clicks$y_vals)
})
  
output$image_plot_values_tmp <- renderPlot({
  if (is.null(image_clicks$x_vals)) {
    NULL
  }else{
  dat <- data.frame(x = image_clicks$x_vals, y = (image_clicks$y_vals))
  ggplot(data = dat, aes(x = x, y = y)) +
    geom_point()
  }
})

observeEvent(input$save_data_image, {
  load("tmp/imageClickData.Rda")
  
  dat <- data.frame(x = image_clicks$x_vals, y = image_clicks$y_vals, type = image_clicks$line_val, 
                               blast_count = input$blast_count_image,
                               blast_duration = input$blast_length_image,
                               run = input$replicate_image,
                               current_blast = input$current_blast_image,
                               move_val = input$move_value_image
                               )
  cornerData <- dat %>% filter(type == "corners")
  upperCorners <- cornerData %>% arrange(y) %>% top_n(n = 2, wt = y) %>% mutate(y = -y)
  rightCorners <- cornerData %>% arrange(x) %>% top_n(n = 2, wt = x) %>% mutate(y = -y)
  minXU <- min(upperCorners$x)
  minYU <- min(upperCorners$y)
  minXR <- min(rightCorners$x)
  minYR <- min(rightCorners$y)
  upperCorners <- upperCorners %>% mutate(x = x - minXU, y = y - minYU)
  rightCorners <- rightCorners %>% mutate(x = x - minXR, y = y - minYR)
  
  theta.x <- atan(max(upperCorners$y)/max(upperCorners$x))
  theta.y <- atan(max(rightCorners$y)/max(rightCorners$x))
  dat<- dat %>% mutate(y = -y) %>% mutate(y.adj = y - x * tan(theta.x),
                                                                x.adj = x + y / tan(theta.y))
  
  dat <- dat %>% mutate(id = paste0(blast_duration, blast_count, run, move_val, current_blast))
  imageClickData <- imageClickData %>% rbind(dat)
  save(imageClickData, file = "tmp/imageClickData.Rda")
  image_clicks$x_vals <- NULL
  image_clicks$y_vals <- NULL
  image_clicks$line_val <- NULL
})

observeEvent(input$clear_data_image, {
  image_clicks$x_vals <- NULL
  image_clicks$y_vals <- NULL
  image_clicks$line_val <- NULL
})

output$image_plot_values <- renderPlot({
  load("tmp/imageClickData.Rda")
  
  imageClickData <- imageClickData %>% mutate(side.group = ifelse(x.adj < 170, "l", "r")) %>% mutate(plot.group = paste0(id, type, side.group))
  
  ggplot() +
    geom_point(data = imageClickData, aes(x = x.adj, y = y.adj, group = plot.group, color = type, shape = run)) +
    geom_path(data = imageClickData %>% filter(type != "corners"), aes(x = x.adj, y = y.adj, group = plot.group, color = type)) 
})


} 






