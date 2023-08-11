library(shinyjs)
library(shinyalert)
library(fontawesome)

jsCode <- '
shinyjs.backgroundCol = function(params) {
var defaultParams = {
id : null,
col : "red"
};
params = shinyjs.getParams(params, defaultParams);
var el = $("#" + params.id);
el.css("background-color", params.col);
}'

tags$style(type="text/css",
           ".shiny-output-error { visibility: hidden; }",
           ".shiny-output-error:before { visibility: hidden; }"
)

titleData <- tags$a(href="https://www.nature.com/articles/s41598-021-03307-7",
                     icon("volcano"),
                    "Beads Vis", target="_blank")

ui <- dashboardPage(

               
                    dashboardHeader(title = titleData, titleWidth = 350),
                    dashboardSidebar(width = 350,
                                     sidebarMenu(id = "tabs",
                                                 menuItem("Plots",tabName =  "plots_tab", icon = icon("chart-bar"),
                                                          menuSubItem(text = "Segments by colour", "plot_1"),
                                                          menuSubItem(text = "Columns and rows analysis", tabName = "plot_2"),
                                                          menuSubItem(text = "Progression", tabName = "plot_3")),
                                                 menuItem("Data input", tabName = "data_input_tab", icon = icon("pen")
                                                 ),
                                                 
                                                 menuItem("View Data",tabName =  "data_view_tab", icon = icon("table")),
                                                 menuItem("About", tabName = "about_tab", icon = icon("circle-info"))
                                     )
                    ),
                    dashboardBody(
                      useShinyjs(),
                      extendShinyjs(text  = jsCode, functions = 'backgroundCol'),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "custom1.css")
                      ),
                      tabItems(
                        tabItem(tabName = "data_input_tab", helpText("Input Data"),
                                helpText("DO NOT USE"),
                                fluidRow(
                                  column(width = 4,  box(title = "", solidHeader = TRUE, width = "30%", 
                                                         column(width = 3, numericInput(inputId = "r1_c1_input_yellow", value = 0, label = "")),
                                                         column(width = 3, numericInput(inputId = "r1_c1_input_blue", value = 0, label = "")),
                                                         column(width = 3, numericInput(inputId = "r1_c1_input_red", value = 0, label = ""))
                                                     )
                                  ),
                                  column(width = 4, box(title = "", solidHeader = TRUE, width = "30%", 
                                                        column(width = 3, numericInput(inputId = "r1_c2_input_yellow", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r1_c2_input_blue", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r1_c2_input_red", value = 0, label = ""))
                                  )),
                                  column(width = 4, box(title = "", solidHeader = TRUE, width = "30%", 
                                                        column(width = 3, numericInput(inputId = "r1_c3_input_yellow", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r1_c3_input_blue", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r1_c3_input_red", value = 0, label = ""))
                                  ))
                                ),
                                fluidRow(
                                  column(width = 4,  box(title = "", solidHeader = TRUE, width = "30%", 
                                                         column(width = 3, numericInput(inputId = "r2_c1_input_yellow", value = 0, label = "")),
                                                         column(width = 3, numericInput(inputId = "r2_c1_input_blue", value = 0, label = "")),
                                                         column(width = 3, numericInput(inputId = "r2_c1_input_red", value = 0, label = ""))
                                  )
                                  ),
                                  column(width = 4, box(title = "", solidHeader = TRUE, width = "30%", 
                                                        column(width = 3, numericInput(inputId = "r2_c2_input_yellow", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r2_c2_input_blue", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r2_c2_input_red", value = 0, label = ""))
                                  )),
                                  column(width = 4, box(title = "", solidHeader = TRUE, width = "30%", 
                                                        column(width = 3, numericInput(inputId = "r2_c3_input_yellow", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r2_c3_input_blue", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r2_c3_input_red", value = 0, label = ""))
                                  ))
                                ),
                                fluidRow(
                                  column(width = 4,  box(title = "", solidHeader = TRUE, width = "30%", 
                                                         column(width = 3, numericInput(inputId = "r3_c1_input_yellow", value = 0, label = "")),
                                                         column(width = 3, numericInput(inputId = "r3_c1_input_blue", value = 0, label = "")),
                                                         column(width = 3, numericInput(inputId = "r3_c1_input_red", value = 0, label = ""))
                                  )
                                  ),
                                  column(width = 4, box(title = "", solidHeader = TRUE, width = "30%", 
                                                        column(width = 3, numericInput(inputId = "r3_c2_input_yellow", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r3_c2_input_blue", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r3_c2_input_red", value = 0, label = ""))
                                  )),
                                  column(width = 4, box(title = "", solidHeader = TRUE, width = "30%", 
                                                        column(width = 3, numericInput(inputId = "r3_c3_input_yellow", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r3_c3_input_blue", value = 0, label = "")),
                                                        column(width = 3, numericInput(inputId = "r3_c3_input_red", value = 0, label = ""))
                                  ))
                                ),
                               
                                selectInput(inputId = 'replicate', label = "Run", choices = c("-", "1", "2"), selected = "-", multiple = FALSE),
                                selectInput(inputId = 'blast_count', label = "Number of blasts", choices = c("-", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10"), selected = "-", multiple = FALSE),
                                selectInput(inputId = 'blast_length', label = "Blast length (seconds)", choices = c("-", "0.1", "1", "2", "3", "4"), selected = "-", multiple = FALSE),
                                checkboxInput(inputId = "move_value", label = "Blast location is moving", value = F),
                                actionButton(inputId = "save_data", label = "Add data"),
                                textOutput('data_input_help_message')
                        ),
                        
                        tabItem(tabName = "plot_1",
                                helpText('Segments by colour'),
                                textOutput(outputId = "segment_text"),
                                plotOutput(outputId = "all_bar_plots", height = "1000px", click = "segments_click", hover = hoverOpts(id = "segments_hover")),
                                fluidRow(
                                column(width = 2, uiOutput(outputId = "replicate_plot_input")),
                                column(width = 2, uiOutput(outputId = "blast_count_plot_input")),
                                column(width = 2, uiOutput(outputId = "blast_duration_plot_input")),
                                column(width = 2, uiOutput(outputId = "move_plot_input"))
                                ),
                                fluidRow(
                                  column(width = 2, checkboxInput(inputId = "stack_colours_plot", label = "Stack colours", value = F)),
                                  column(width = 2, checkboxInput(inputId = "combine_replicates_plot", "Combine data from each replicate", value = F))
                                )
                               ),
                        
                        tabItem(tabName = "plot_2",
                                helpText('Columns and rows analysis (rows not implemented yet).'),
                                textOutput(outputId = "column_text"),
                                plotOutput(outputId = "columns_and_rows_plot", height = "1000px", click = "columns_click", hover = hoverOpts(id = "columns_hover")),
                                fluidRow(
                                  column(width = 3, uiOutput(outputId = "replicate_plot_combined_input")),
                                  column(width = 3, uiOutput(outputId = "blast_count_plot_combined_input")),
                                  column(width = 3, uiOutput(outputId = "blast_duration_plot_combined_input")),
                                  
                                ),
                                fluidRow(
                                  column(width = 2, checkboxInput(inputId = "stack_colours_plot_column", label = "Stack colours", value = T)),
                                  column(width = 2, checkboxInput(inputId = "combine_replicates_plot_column", "Combine data from each replicate", value = T))
                                )
                        ),
                        
                        tabItem(tabName = "plot_3",
                                markdown('For each gram of beads points are randomly generated in the segment in which the bead was found. Starting data can be adjusted.
                                
                                          The number of points is calculated using `ceiling(weight/bead_weight)` where:
                                          * `ceiling()` rounds up to the nearest whole number
                                          * `weight` is the weight in grams from the spreadsheet
                                          * `bead_weight` is the selected value for the weight of the beads using the slider'),
                                fluidRow(
                                  column(width = 3, radioButtons(inputId = "hide_unchanged_data_radio", choices = c("Hide unchanged data" = "hide_unchanged", "Show unchanged data" = "show_unchanged", "Grey out unchanged data" = "grey_unchanged"), 
                                                                 label = "Choose points to view"), uiOutput(outputId = "alpha_val_input")),
                                  column(width = 2, uiOutput(outputId = "replicate_plot_progression_input"), selectInput(inputId = 'colours_prog', label = "Select colours:", choices = c("yellow", "blue", "red"), selected = c("yellow", "blue", "red"), multiple = T)),
                                  column(width = 3, sliderInput(inputId = "progression_plot_height", label = "Plot height (px)", min = 200, max = 2000, value = 650, step = 50)),
                                  column(width = 3, sliderInput(inputId = "bead_weight", label = "Bead weight (grams)", min = 0.01, max = 1, value = 0.2, step = 0.01))
                                ),
                               
                                uiOutput(outputId = "progression_plot_output"),
                                sliderInput(inputId = "y.height", label = "Adjust y position of initial data", min = 0.001, max = 0.999, value = 0.5, step = 0.01),
                                sliderInput(inputId = "y.variance", label = "Adjust y variance of initial data", min = 0.001, max = 0.999, value = 0.45, step = 0.01)
                        ),
                        tabItem(tabName = "data_view_tab",
                                DTOutput("blastDataTable"),
                                downloadButton("downloadData", "Download")
                        ),
                        tabItem(tabName = "about_tab",
                                uiOutput(outputId = "random_image"),
                                helpText("Data gathering and science thinking stuff performed by MK Fitzgerald."),
                                helpText("Code for the app written by Dr TJ Nicholson."),
                                tags$a(href="https://github.com/TJN25/chelle_data_vis", 
                                       "Source code is available here"),
                                helpText(""),
                                tags$a(href="https://github.com/TJN25/chelle_data_vis/issues", 
                                       "Support and discussion"))
                      )
                    )

)

