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

durationLabel <- function(string) {
  string <- paste0("Blast Duration: ", string)
  string
}
countLabel <- function(string) {
  string <- paste0("Blast Count: ", string)
  string
}

rowLabel <- function(string) {
  string <- paste0("Row: ", string)
  string
}
columnLabel <- function(string) {
  string <- paste0("Column: ", string)
  string
}
