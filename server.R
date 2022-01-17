# currently not working

server <- function(input, output, session) {
  # seed_val <- reactiveVal(101)
  output$word_set_selection <- renderUI({
    selectInput("word_set_selected", "Choose Words:", choices = word_sets, selected = word_sets[1])
  })
  
  output$done_button <- renderUI({
    if(turnInfo$player@game_state == "ongoing"){
    actionButton(inputId = "done_guessing", "Done Guessing")
    }else{
      actionButton(inputId = "done_guessing", "Play again")
    }
  })
  output$done_button_tom <- renderUI({

    if(turnInfo$player@game_state == "ongoing"){
      
      actionButton(inputId = "done_guessing_tom", "Done Guessing")
      
    }else{
      actionButton(inputId = "done_guessing_tom", "Play again")
    }
  })
  
  word_list <- reactive({
    print(input$word_set_selected)
    if(input$word_set_selected == "Random words") {
      dat <- codenames
    }
    if(input$word_set_selected == "Harry Potter"){
      dat <- harry_potter
    }
    dat
  })
  
  output$seed_text <- renderUI({
    input$reset_seed
    random_value <- round(runif(1, min = 100, max = 100000000))
    textInput("seed_input_val", label = "Select seed:", value = floor(as.numeric(Sys.time())/3600))
  })
  seed_val <- reactiveValues()
  ##each word and the state for each player
  selected_word_list <- reactiveValues()
    
    
  
  
    
    
  turnInfo <- reactiveValues()
  img1 <- reactiveValues()

  ##update
  output$plot_a <- renderPlot({
    makePlot(img_subset = img1$a, player_info = turnInfo$player)
  })
  output$plot_b <- renderPlot({
    makePlot(img_subset = img1$b, player_info = turnInfo$player)
  })
  output$plot_c <- renderPlot({
    makePlot(img_subset = img1$c, player_info = turnInfo$player)
  })
  output$plot_d <- renderPlot({
    makePlot(img_subset = img1$d, player_info = turnInfo$player)
  })
  output$plot_e <- renderPlot({
    makePlot(img_subset = img1$e, player_info = turnInfo$player)
  })
  output$plot_f <- renderPlot({
    makePlot(img_subset = img1$f, player_info = turnInfo$player)
  })
  output$plot_g <- renderPlot({
    makePlot(img_subset = img1$g, player_info = turnInfo$player)
  })
  output$plot_h <- renderPlot({
    makePlot(img_subset = img1$h, player_info = turnInfo$player)
  })
  output$plot_i <- renderPlot({
    makePlot(img_subset = img1$i, player_info = turnInfo$player)
  })
  output$plot_j <- renderPlot({
    makePlot(img_subset = img1$j, player_info = turnInfo$player)
  })
  output$plot_k <- renderPlot({
    makePlot(img_subset = img1$k, player_info = turnInfo$player)
  })
  output$plot_l <- renderPlot({
    makePlot(img_subset = img1$l, player_info = turnInfo$player)
  })
  output$plot_m <- renderPlot({
    makePlot(img_subset = img1$m, player_info = turnInfo$player)
  })
  output$plot_n <- renderPlot({
    makePlot(img_subset = img1$n, player_info = turnInfo$player)
  })
  output$plot_o <- renderPlot({
    makePlot(img_subset = img1$o, player_info = turnInfo$player)
  })
  output$plot_p <- renderPlot({
    makePlot(img_subset = img1$p, player_info = turnInfo$player)
  })
  output$plot_q <- renderPlot({
    makePlot(img_subset = img1$q, player_info = turnInfo$player)
  })
  output$plot_r <- renderPlot({
    makePlot(img_subset = img1$r, player_info = turnInfo$player)
  })
  output$plot_s <- renderPlot({
    makePlot(img_subset = img1$s, player_info = turnInfo$player)
  })
  output$plot_t <- renderPlot({
    makePlot(img_subset = img1$t, player_info = turnInfo$player)
  })
  output$plot_u <- renderPlot({
    makePlot(img_subset = img1$u, player_info = turnInfo$player)
  })
  output$plot_v <- renderPlot({
    makePlot(img_subset = img1$v, player_info = turnInfo$player)
  })
  output$plot_w <- renderPlot({
    makePlot(img_subset = img1$w, player_info = turnInfo$player)
  })
  output$plot_x <- renderPlot({
    makePlot(img_subset = img1$x, player_info = turnInfo$player)
  })
  output$plot_y <- renderPlot({
    makePlot(img_subset = img1$y, player_info = turnInfo$player)
  })
  
  
  output$plot_a_tom <- renderPlot({
    makePlot(img_subset = img1$a, player_info = turnInfo$player)
  })
  output$plot_b_tom <- renderPlot({
    makePlot(img_subset = img1$b, player_info = turnInfo$player)
  })
  output$plot_c_tom <- renderPlot({
    makePlot(img_subset = img1$c, player_info = turnInfo$player)
  })
  output$plot_d_tom <- renderPlot({
    makePlot(img_subset = img1$d, player_info = turnInfo$player)
  })
  output$plot_e_tom <- renderPlot({
    makePlot(img_subset = img1$e, player_info = turnInfo$player)
  })
  output$plot_f_tom <- renderPlot({
    makePlot(img_subset = img1$f, player_info = turnInfo$player)
  })
  output$plot_g_tom <- renderPlot({
    makePlot(img_subset = img1$g, player_info = turnInfo$player)
  })
  output$plot_h_tom <- renderPlot({
    makePlot(img_subset = img1$h, player_info = turnInfo$player)
  })
  output$plot_i_tom <- renderPlot({
    makePlot(img_subset = img1$i, player_info = turnInfo$player)
  })
  output$plot_j_tom <- renderPlot({
    makePlot(img_subset = img1$j, player_info = turnInfo$player)
  })
  output$plot_k_tom <- renderPlot({
    makePlot(img_subset = img1$k, player_info = turnInfo$player)
  })
  output$plot_l_tom <- renderPlot({
    makePlot(img_subset = img1$l, player_info = turnInfo$player)
  })
  output$plot_m_tom <- renderPlot({
    makePlot(img_subset = img1$m, player_info = turnInfo$player)
  })
  output$plot_n_tom <- renderPlot({
    makePlot(img_subset = img1$n, player_info = turnInfo$player)
  })
  output$plot_o_tom <- renderPlot({
    makePlot(img_subset = img1$o, player_info = turnInfo$player)
  })
  output$plot_p_tom <- renderPlot({
    makePlot(img_subset = img1$p, player_info = turnInfo$player)
  })
  output$plot_q_tom <- renderPlot({
    makePlot(img_subset = img1$q, player_info = turnInfo$player)
  })
  output$plot_r_tom <- renderPlot({
    makePlot(img_subset = img1$r, player_info = turnInfo$player)
  })
  output$plot_s_tom <- renderPlot({
    makePlot(img_subset = img1$s, player_info = turnInfo$player)
  })
  output$plot_t_tom <- renderPlot({
    makePlot(img_subset = img1$t, player_info = turnInfo$player)
  })
  output$plot_u_tom <- renderPlot({
    makePlot(img_subset = img1$u, player_info = turnInfo$player)
  })
  output$plot_v_tom <- renderPlot({
    makePlot(img_subset = img1$v, player_info = turnInfo$player)
  })
  output$plot_w_tom <- renderPlot({
    makePlot(img_subset = img1$w, player_info = turnInfo$player)
  })
  output$plot_x_tom <- renderPlot({
    makePlot(img_subset = img1$x, player_info = turnInfo$player)
  })
  output$plot_y_tom <- renderPlot({
    makePlot(img_subset = img1$y, player_info = turnInfo$player)
  })

  
  output$which_player <- renderUI({
    if(turnInfo$player@game_state == "ongoing"){
    if(myTurn(turnInfo$player)){
      tags$h4(paste0("Opponent turn to guess. ", turnInfo$player@turn_counter, " turns so far. ", 15 -  turnInfo$player@green_counter, " words remaining."))
    }else{
      tags$h4(paste0("Your turn to guess. ", turnInfo$player@turn_counter, " turns so far. ", 15 -  turnInfo$player@green_counter, " words remaining."))
    }
    }else if(turnInfo$player@game_state == "won"){
      # seed_val <- callModule(module = welcome, id = "welcome")
      shinyalert(title = paste0("You won in ", turnInfo$player@turn_counter, " turns"), 
                 text = paste0("To play again, go back to the home page and choose a new number"), 
                 type = "success",
                 showConfirmButton = T, confirmButtonText = "Play again",
                 inputId = "win_a_play_again")
      tags$h4(paste0("You won in ", turnInfo$player@turn_counter, " turns!."))
    }else{
      shinyalert(title = paste0("You lost in ", turnInfo$player@turn_counter, " turns"), 
                 text = paste0("To play again, go back to the home page and choose a new number"), 
                 type = "error",
                 showConfirmButton = T, confirmButtonText = "Play again",
                 inputId = "lost_a_play_again")
      
      tags$h4(paste0("You lost in ", turnInfo$player@turn_counter, " turns!."))
    }
  })
  output$which_player_tom <- renderUI({
    if(turnInfo$player@game_state == "ongoing"){
      if(myTurn(turnInfo$player)){
        
        tags$h4(paste0("Opponent turn to guess. ", turnInfo$player@turn_counter, " turns so far. ", 15 -  turnInfo$player@green_counter, " words remaining."))
      }else{
        tags$h4(paste0("Your turn to guess. ", turnInfo$player@turn_counter, " turns so far. ", 15 -  turnInfo$player@green_counter, " words remaining."))
      }
    }else if(turnInfo$player@game_state == "won"){
      shinyalert(title = paste0("You won in ", turnInfo$player@turn_counter, " turns"), 
                 text = paste0("To play again, go back to the home page and choose a new number"), 
                 type = "success",
                 showConfirmButton = T, confirmButtonText = "Play again",
                 inputId = "win_b_play_again")
      # seed_val <- callModule(module = welcome, id = "welcome")
      tags$h4(paste0("You won in ", turnInfo$player@turn_counter, " turns!."))
    }else{
      shinyalert(title = paste0("You lost in ", turnInfo$player@turn_counter, " turns"), 
                 text = paste0("To play again, go back to the home page and choose a new number"), 
                 type = "error",
                 showConfirmButton = T, confirmButtonText = "Play again",
                 inputId = "lost_b_play_again")
      tags$h4(paste0("You lost in ", turnInfo$player@turn_counter, " turns!."))
    }
  })
  
  #update
 observeEvent(input$click_a, {
   img_subset <- img1$a
   selected_word_val <- selected_word_list$a[[1]]
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
   img1$a <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
   selected_word_list$a[[1]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
   turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
                                  
   if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
     shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                type = "error",
                showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
     updatePlayerTurn(turnInfo$player)<-T
     updateTurnCounter(turnInfo$player)<-T

     }
   }                                
 })
 observeEvent(input$click_b, {
   img_subset <- img1$b
   selected_word_val <- selected_word_list$a[[2]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$b <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[2]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T

     }
   }                                
 })
 observeEvent(input$click_c, {
   img_subset <- img1$c
   selected_word_val <- selected_word_list$a[[3]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$c <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[3]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
       
     }
   }                                
 })
 observeEvent(input$click_d, {
   img_subset <- img1$d
   selected_word_val <- selected_word_list$a[[4]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$d <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[4]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_e, {
   img_subset <- img1$e
   selected_word_val <- selected_word_list$a[[5]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$e <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[5]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_f, {
   img_subset <- img1$f
   selected_word_val <- selected_word_list$a[[6]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$f <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[6]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_g, {
   img_subset <- img1$g
   selected_word_val <- selected_word_list$a[[7]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$g <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[7]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_h, {
   selected_word_val <- selected_word_list$a[[8]]
   
   img_subset <- img1$h
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$h <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[8]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_i, {
   selected_word_val <- selected_word_list$a[[9]]
   
   img_subset <- img1$i
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$i <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[9]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_j, {
   img_subset <- img1$j
   selected_word_val <- selected_word_list$a[[10]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$j <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[10]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_k, {
   img_subset <- img1$k
   selected_word_val <- selected_word_list$a[[11]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$k <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[11]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_l, {
   img_subset <- img1$l
   selected_word_val <- selected_word_list$a[[12]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$l <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[12]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_m, {
   selected_word_val <- selected_word_list$a[[13]]
   
   img_subset <- img1$m
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$m <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[13]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_n, {
   selected_word_val <- selected_word_list$a[[14]]
   
   img_subset <- img1$n
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$n <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[14]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_o, {
   img_subset <- img1$o
   selected_word_val <- selected_word_list$a[[15]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$o <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[15]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_p, {
   img_subset <- img1$p
   selected_word_val <- selected_word_list$a[[16]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$p <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[16]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_q, {
   img_subset <- img1$q
   selected_word_val <- selected_word_list$a[[17]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$q <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[17]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_r, {
   selected_word_val <- selected_word_list$a[[18]]
   
   img_subset <- img1$r
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$r <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[18]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_s, {
   selected_word_val <- selected_word_list$a[[19]]
   
   img_subset <- img1$s
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$s <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[19]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_t, {
   img_subset <- img1$t
   selected_word_val <- selected_word_list$a[[20]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$t <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[20]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_u, {
   img_subset <- img1$u
   selected_word_val <- selected_word_list$a[[21]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$u <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[21]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_v, {
   img_subset <- img1$v
   selected_word_val <- selected_word_list$a[[22]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$v <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[22]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_w, {
   selected_word_val <- selected_word_list$a[[23]]
   
   img_subset <- img1$w
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$w <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[23]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_x, {
   selected_word_val <- selected_word_list$a[[24]]
   
   img_subset <- img1$x
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$x <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[24]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_y, {
   img_subset <- img1$y
   selected_word_val <- selected_word_list$a[[25]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$y <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[25]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 
 observeEvent(input$click_a_tom, {
   img_subset <- img1$a
   selected_word_val <- selected_word_list$a[[1]]
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$a <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     selected_word_list$a[[1]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
       
     }
   }                                
 })
 observeEvent(input$click_b_tom, {
   img_subset <- img1$b
   selected_word_val <- selected_word_list$a[[2]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$b <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[2]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
       
     }
   }                                
 })
 observeEvent(input$click_c_tom, {
   img_subset <- img1$c
   selected_word_val <- selected_word_list$a[[3]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$c <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[3]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
       
     }
   }                                
 })
 observeEvent(input$click_d_tom, {
   img_subset <- img1$d
   selected_word_val <- selected_word_list$a[[4]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$d <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[4]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_e_tom, {
   img_subset <- img1$e
   selected_word_val <- selected_word_list$a[[5]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$e <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[5]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_f_tom, {
   img_subset <- img1$f
   selected_word_val <- selected_word_list$a[[6]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$f <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[6]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_g_tom, {
   img_subset <- img1$g
   selected_word_val <- selected_word_list$a[[7]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$g <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[7]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_h_tom, {
   selected_word_val <- selected_word_list$a[[8]]
   
   img_subset <- img1$h
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$h <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[8]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_i_tom, {
   selected_word_val <- selected_word_list$a[[9]]
   
   img_subset <- img1$i
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$i <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[9]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_j_tom, {
   img_subset <- img1$j
   selected_word_val <- selected_word_list$a[[10]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$j <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[10]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_k_tom, {
   img_subset <- img1$k
   selected_word_val <- selected_word_list$a[[11]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$k <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[11]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_l_tom, {
   img_subset <- img1$l
   selected_word_val <- selected_word_list$a[[12]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$l <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[12]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_m_tom, {
   selected_word_val <- selected_word_list$a[[13]]
   
   img_subset <- img1$m
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$m <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[13]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_n_tom, {
   selected_word_val <- selected_word_list$a[[14]]
   
   img_subset <- img1$n
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$n <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[14]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_o_tom, {
   img_subset <- img1$o
   selected_word_val <- selected_word_list$a[[15]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$o <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[15]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_p_tom, {
   img_subset <- img1$p
   selected_word_val <- selected_word_list$a[[16]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$p <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[16]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_q_tom, {
   img_subset <- img1$q
   selected_word_val <- selected_word_list$a[[17]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$q <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[17]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_r_tom, {
   selected_word_val <- selected_word_list$a[[18]]
   
   img_subset <- img1$r
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$r <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[18]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_s_tom, {
   selected_word_val <- selected_word_list$a[[19]]
   
   img_subset <- img1$s
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$s <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[19]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_t_tom, {
   img_subset <- img1$t
   selected_word_val <- selected_word_list$a[[20]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$t <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[20]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_u_tom, {
   img_subset <- img1$u
   selected_word_val <- selected_word_list$a[[21]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$u <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[21]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_v_tom, {
   img_subset <- img1$v
   selected_word_val <- selected_word_list$a[[22]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$v <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[22]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_w_tom, {
   selected_word_val <- selected_word_list$a[[23]]
   
   img_subset <- img1$w
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$w <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[23]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_x_tom, {
   selected_word_val <- selected_word_list$a[[24]]
   
   img_subset <- img1$x
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$x <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[24]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 observeEvent(input$click_y_tom, {
   img_subset <- img1$y
   selected_word_val <- selected_word_list$a[[25]]
   
   if(!checkGuess(player_info = turnInfo$player, img_subset = img_subset)){
     
     img1$y <- updateImageClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset)
     turnInfo$player <- updateTurnInfo(player_info = turnInfo$player, img_subset =  img_subset)
     selected_word_list$a[[25]] <- updateWordClicked(player_turn = turnInfo$player@player_turn, img_subset = img_subset, selected_word_val = selected_word_val)
     
     if(correctGuessCheck(player_turn = turnInfo$player@player_turn, img_subset = img_subset) == "grey"){
       shinyalert(title = paste0("Wrong choice: ", ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), 
                  text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), 
                  type = "error",
                  showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
       updatePlayerTurn(turnInfo$player)<-T
       updateTurnCounter(turnInfo$player)<-T
     }
   }                                
 })
 
 
 
 
  observeEvent(input$side_a, {
    # print('side_a')
    newtab <- switch(input$tabs,
                     "welcome" = "chelle"
    )
    # print(newtab)
    updateTabItems(session, "tabs", newtab)
    seed_val$a <- input$seed_input_val
    selected_word_list$a <- getWordsAndTypes(seed_val = seed_val$a, word_list = word_list(), words_length = words_length)
    print(seed_val$a)
    turnInfo$player <- new("player_turn", player = "A", name = "Tom")
    i <- 1
    img1$a <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 2
    img1$b <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 3
    img1$c <- new("image_selection",
                     filenum=as.character(i),
                     word_val=selected_word_list$a[[i]]@word_val,
                     word_type_a=selected_word_list$a[[i]]@word_type_a,
                     word_type_b=selected_word_list$a[[i]]@word_type_b,
                     word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                     lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                     lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 4
    img1$d <- new("image_selection",
                     filenum=as.character(i),
                     word_val=selected_word_list$a[[i]]@word_val,
                     word_type_a=selected_word_list$a[[i]]@word_type_a,
                     word_type_b=selected_word_list$a[[i]]@word_type_b,
                     word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                     lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                     lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 5
    img1$e <- new("image_selection",
                     filenum=as.character(i),
                     word_val=selected_word_list$a[[i]]@word_val,
                     word_type_a=selected_word_list$a[[i]]@word_type_a,
                     word_type_b=selected_word_list$a[[i]]@word_type_b,
                     word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                     lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                     lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 6
    img1$f <- new("image_selection",
                     filenum=as.character(i),
                     word_val=selected_word_list$a[[i]]@word_val,
                     word_type_a=selected_word_list$a[[i]]@word_type_a,
                     word_type_b=selected_word_list$a[[i]]@word_type_b,
                     word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                     lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                     lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 7
    img1$g <- new("image_selection",
                     filenum=as.character(i),
                     word_val=selected_word_list$a[[i]]@word_val,
                     word_type_a=selected_word_list$a[[i]]@word_type_a,
                     word_type_b=selected_word_list$a[[i]]@word_type_b,
                     word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                     lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                     lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 8
    img1$h <- new("image_selection",
                     filenum=as.character(i),
                     word_val=selected_word_list$a[[i]]@word_val,
                     word_type_a=selected_word_list$a[[i]]@word_type_a,
                     word_type_b=selected_word_list$a[[i]]@word_type_b,
                     word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                     lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                     lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 9
    img1$i <- new("image_selection",
                     filenum=as.character(i),
                     word_val=selected_word_list$a[[i]]@word_val,
                     word_type_a=selected_word_list$a[[i]]@word_type_a,
                     word_type_b=selected_word_list$a[[i]]@word_type_b,
                     word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                     lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                     lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 10
    img1$j <- new("image_selection",
                     filenum=as.character(i),
                     word_val=selected_word_list$a[[i]]@word_val,
                     word_type_a=selected_word_list$a[[i]]@word_type_a,
                     word_type_b=selected_word_list$a[[i]]@word_type_b,
                     word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                     lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                     lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 11
    img1$k <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 12
    img1$l <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 13
    img1$m <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 14
    img1$n <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 15
    img1$o <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 16
    img1$p <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 17
    img1$q <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 18
    img1$r <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 19
    img1$s <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 20
    img1$t <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 21
    img1$u <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 22
    img1$v <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 23
    img1$w <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 24
    img1$x <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 25
    img1$y <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
  })
  observeEvent(input$side_b, {
    # print('side_a')
    newtab <- switch(input$tabs,
                     "welcome" = "tom"
    )
    # print(newtab)
    updateTabItems(session, "tabs", newtab)
    seed_val$a <- input$seed_input_val
    selected_word_list$a <- getWordsAndTypes(seed_val = seed_val$a, word_list = word_list(), words_length = words_length)
    print(seed_val$a)
    turnInfo$player <- new("player_turn", player = "B", name = "Tom")
    i <- 1
    img1$a <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 2
    img1$b <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 3
    img1$c <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 4
    img1$d <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 5
    img1$e <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 6
    img1$f <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 7
    img1$g <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 8
    img1$h <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 9
    img1$i <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 10
    img1$j <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 11
    img1$k <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 12
    img1$l <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 13
    img1$m <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 14
    img1$n <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 15
    img1$o <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 16
    img1$p <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 17
    img1$q <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 18
    img1$r <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 19
    img1$s <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 20
    img1$t <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 21
    img1$u <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 22
    img1$v <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 23
    img1$w <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 24
    img1$x <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 25
    img1$y <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
  })
  
  observeEvent(input$done_guessing, {
    if(turnInfo$player@game_state == "ongoing"){
      shinyalert(title = paste0(ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), type = "success",
                 showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)   
      updatePlayerTurn(turnInfo$player)<-T
    updateTurnCounter(turnInfo$player)<-T
    
    }else{
      if(input$tabs == "chelle"){
      # newtab <- switch(input$tabs,
      #                  "chelle" = "welcome"
      # )
      # # print(newtab)
      # updateTabItems(session, "tabs", newtab)
        seed_val$a <- as.numeric(seed_val$a) + 1
        selected_word_list$a <- getWordsAndTypes(seed_val = seed_val$a, word_list = word_list(), words_length = words_length)
        print(seed_val$a)
        turnInfo$player <- new("player_turn", player = "A", name = "Tom")
        i <- 1
        img1$a <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 2
        img1$b <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 3
        img1$c <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 4
        img1$d <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 5
        img1$e <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 6
        img1$f <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 7
        img1$g <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 8
        img1$h <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 9
        img1$i <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 10
        img1$j <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 11
        img1$k <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 12
        img1$l <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 13
        img1$m <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 14
        img1$n <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 15
        img1$o <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 16
        img1$p <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 17
        img1$q <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 18
        img1$r <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 19
        img1$s <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 20
        img1$t <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 21
        img1$u <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 22
        img1$v <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 23
        img1$w <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 24
        img1$x <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 25
        img1$y <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
      }
    }
  })
  observeEvent(input$lost_a_play_again, {
    print(input$shinyalert)
    seed_val$a <- as.numeric(seed_val$a) + 1
    selected_word_list$a <- getWordsAndTypes(seed_val = seed_val$a, word_list = word_list(), words_length = words_length)
    print(seed_val$a)
    turnInfo$player <- new("player_turn", player = "A", name = "Tom")
    i <- 1
    img1$a <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 2
    img1$b <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 3
    img1$c <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 4
    img1$d <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 5
    img1$e <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 6
    img1$f <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 7
    img1$g <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 8
    img1$h <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 9
    img1$i <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 10
    img1$j <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 11
    img1$k <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 12
    img1$l <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 13
    img1$m <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 14
    img1$n <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 15
    img1$o <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 16
    img1$p <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 17
    img1$q <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 18
    img1$r <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 19
    img1$s <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 20
    img1$t <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 21
    img1$u <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 22
    img1$v <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 23
    img1$w <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 24
    img1$x <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 25
    img1$y <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
  })
  observeEvent(input$win_a_play_again, {
    print(input$shinyalert)
    seed_val$a <- as.numeric(seed_val$a) + 1
    selected_word_list$a <- getWordsAndTypes(seed_val = seed_val$a, word_list = word_list(), words_length = words_length)
    print(seed_val$a)
    turnInfo$player <- new("player_turn", player = "A", name = "Tom")
    i <- 1
    img1$a <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 2
    img1$b <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 3
    img1$c <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 4
    img1$d <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 5
    img1$e <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 6
    img1$f <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 7
    img1$g <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 8
    img1$h <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 9
    img1$i <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 10
    img1$j <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 11
    img1$k <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 12
    img1$l <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 13
    img1$m <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 14
    img1$n <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 15
    img1$o <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 16
    img1$p <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 17
    img1$q <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 18
    img1$r <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 19
    img1$s <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 20
    img1$t <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 21
    img1$u <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 22
    img1$v <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 23
    img1$w <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 24
    img1$x <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 25
    img1$y <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
  })
  
  observeEvent(input$lost_b_play_again, {
    seed_val$a <- as.numeric(seed_val$a) + 1
    selected_word_list$a <- getWordsAndTypes(seed_val = seed_val$a, word_list = word_list(), words_length = words_length)
    print(seed_val$a)
    turnInfo$player <- new("player_turn", player = "B", name = "Tom")
    i <- 1
    img1$a <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 2
    img1$b <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 3
    img1$c <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 4
    img1$d <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 5
    img1$e <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 6
    img1$f <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 7
    img1$g <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 8
    img1$h <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 9
    img1$i <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 10
    img1$j <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 11
    img1$k <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 12
    img1$l <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 13
    img1$m <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 14
    img1$n <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 15
    img1$o <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 16
    img1$p <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 17
    img1$q <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 18
    img1$r <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 19
    img1$s <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 20
    img1$t <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 21
    img1$u <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 22
    img1$v <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 23
    img1$w <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 24
    img1$x <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 25
    img1$y <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
  })
  observeEvent(input$win_b_play_again, {
    seed_val$a <- as.numeric(seed_val$a) + 1
    selected_word_list$a <- getWordsAndTypes(seed_val = seed_val$a, word_list = word_list(), words_length = words_length)
    print(seed_val$a)
    turnInfo$player <- new("player_turn", player = "B", name = "Tom")
    i <- 1
    img1$a <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 2
    img1$b <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 3
    img1$c <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 4
    img1$d <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 5
    img1$e <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 6
    img1$f <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 7
    img1$g <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 8
    img1$h <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 9
    img1$i <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 10
    img1$j <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 11
    img1$k <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 12
    img1$l <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 13
    img1$m <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 14
    img1$n <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 15
    img1$o <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 16
    img1$p <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 17
    img1$q <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 18
    img1$r <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 19
    img1$s <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 20
    img1$t <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 21
    img1$u <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 22
    img1$v <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 23
    img1$w <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 24
    img1$x <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
    i <- 25
    img1$y <- new("image_selection",
                  filenum=as.character(i),
                  word_val=selected_word_list$a[[i]]@word_val,
                  word_type_a=selected_word_list$a[[i]]@word_type_a,
                  word_type_b=selected_word_list$a[[i]]@word_type_b,
                  word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                  lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                  lose_state_b=selected_word_list$a[[i]]@lose_state_b)
  })
  observeEvent(input$done_guessing_tom, {
    if(turnInfo$player@game_state == "ongoing"){
      shinyalert(title = paste0(ifelse(myTurn(turnInfo$player) == F, "Your ", "Opponent's"), "Turn Complete"), text = paste0("It's your ",ifelse(myTurn(turnInfo$player) == F, "opponent's ", "")," turn now."), type = "success",
                 showConfirmButton = T, confirmButtonText = "Dismiss", timer = 2000)  
      updatePlayerTurn(turnInfo$player)<-T
      updateTurnCounter(turnInfo$player)<-T
      }else{
      if(input$tabs == "tom"){
        seed_val$a <- as.numeric(seed_val$a) + 1
        selected_word_list$a <- getWordsAndTypes(seed_val = seed_val$a, word_list = word_list(), words_length = words_length)
        print(seed_val$a)
        turnInfo$player <- new("player_turn", player = "B", name = "Tom")
        i <- 1
        img1$a <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 2
        img1$b <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 3
        img1$c <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 4
        img1$d <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 5
        img1$e <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 6
        img1$f <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 7
        img1$g <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 8
        img1$h <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 9
        img1$i <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 10
        img1$j <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 11
        img1$k <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 12
        img1$l <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 13
        img1$m <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 14
        img1$n <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 15
        img1$o <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 16
        img1$p <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 17
        img1$q <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 18
        img1$r <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 19
        img1$s <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 20
        img1$t <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 21
        img1$u <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 22
        img1$v <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 23
        img1$w <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 24
        img1$x <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
        i <- 25
        img1$y <- new("image_selection",
                      filenum=as.character(i),
                      word_val=selected_word_list$a[[i]]@word_val,
                      word_type_a=selected_word_list$a[[i]]@word_type_a,
                      word_type_b=selected_word_list$a[[i]]@word_type_b,
                      word_type_ab=selected_word_list$a[[i]]@word_type_ab,
                      lose_state_a=selected_word_list$a[[i]]@lose_state_a,
                      lose_state_b=selected_word_list$a[[i]]@lose_state_b)
      }
    }
  })
  
  chelle_word_colours <- reactive({
    dat <- data.frame(word = rep("", words_length), colour = "", guessed = F)
    for(i in 1:words_length){
      
      dat[i,1] <- selected_word_list$a[[i]]@word_val
      dat[i,2] <- selected_word_list$a[[i]]@word_type_a
      dat[i,3] <- selected_word_list$a[[i]]@aa
      if(selected_word_list$a[[i]]@word_type_a == "green" & selected_word_list$a[[i]]@word_type_b == "green" & selected_word_list$a[[i]]@bb){
        dat[i,3] <- T
      }
      if(selected_word_list$a[[i]]@word_type_a == "grey" & selected_word_list$a[[i]]@word_type_b == "green" & selected_word_list$a[[i]]@bb){
        dat[i,3] <- T
      }
      if(selected_word_list$a[[i]]@word_type_a == "black" & selected_word_list$a[[i]]@word_type_b == "green" & selected_word_list$a[[i]]@bb){
        dat[i,3] <- T
      }
      
    }
    # if(length(dat$guessed[dat$colour == "green" & dat$guessed]) == 8){
    #   updatePlayerTurn(turnInfo$player)<-T
    #   updateTurnCounter(turnInfo$player)<-T
    # }
    dat
    
  })
  tom_word_colours <- reactive({
    dat <- data.frame(word = rep("", words_length), colour = "", guessed = F)
    for(i in 1:words_length){
      dat[i,1] <- selected_word_list$a[[i]]@word_val
      dat[i,2] <- selected_word_list$a[[i]]@word_type_b
      dat[i,3] <- selected_word_list$a[[i]]@bb
      if(selected_word_list$a[[i]]@word_type_a == "green" & selected_word_list$a[[i]]@word_type_b == "green" & selected_word_list$a[[i]]@aa){
        dat[i,3] <- T
      }
      if(selected_word_list$a[[i]]@word_type_b == "grey" & selected_word_list$a[[i]]@word_type_a == "green" & selected_word_list$a[[i]]@aa){
        dat[i,3] <- T
      }
      if(selected_word_list$a[[i]]@word_type_b == "black" & selected_word_list$a[[i]]@word_type_a == "green" & selected_word_list$a[[i]]@aa){
        dat[i,3] <- T
      }
    }
    # if(length(dat$guessed[dat$colour == "green" & dat$guessed]) == 8){
    #   updatePlayerTurn(turnInfo$player)<-T
    #   updateTurnCounter(turnInfo$player)<-T
    # }
    dat

  })
  
  output$green_chelle <- renderUI({
    # print(chelle_word_colours())
    HTML(paste(chelle_word_colours()$word[chelle_word_colours()$colour == "green" & !chelle_word_colours()$guessed], collapse = "<br/>"))
    # "test"
  })
  output$grey_chelle <- renderUI({
    # print(chelle_word_colours())
    HTML(paste(chelle_word_colours()$word[chelle_word_colours()$colour == "grey" & !chelle_word_colours()$guessed], collapse = "<br/>"))
    # "test"
  })
  output$black_chelle <- renderUI({
    # print(chelle_word_colours())
    HTML(paste(chelle_word_colours()$word[chelle_word_colours()$colour == "black" & !chelle_word_colours()$guessed], collapse = "<br/>"))
    # "test"
  })
    
  
  output$green_tom <- renderUI({
    # print(chelle_word_colours())
    HTML(paste(tom_word_colours()$word[tom_word_colours()$colour == "green" & !tom_word_colours()$guessed], collapse = "<br/>"))
    # "test"
  })
  output$grey_tom <- renderUI({
    # print(chelle_word_colours())
    HTML(paste(tom_word_colours()$word[tom_word_colours()$colour == "grey" & !tom_word_colours()$guessed], collapse = "<br/>"))
    # "test"
  })
  output$black_tom <- renderUI({
    # print(chelle_word_colours())
    HTML(paste(tom_word_colours()$word[tom_word_colours()$colour == "black" & !tom_word_colours()$guessed], collapse = "<br/>"))
    # "test"
  })
} 

