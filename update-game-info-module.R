updateImageClicked <- function(player_turn, img_subset){
  if(player_turn){
    if(img_subset@word_type_b == "green" & img_subset@bb){
      print("Already Guessed.")
    }else{
      updateImgA(img_subset)<-T
    }
  }else{
    if(img_subset@word_type_a == "green" & img_subset@aa){
      print("Already Guessed.")
    }else{
      updateImgB(img_subset)<-T
    } 
  }
  return(img_subset)
}

updateWordClicked <- function(player_turn, img_subset, selected_word_val){
  if(player_turn){
    if(img_subset@word_type_b == "green" & img_subset@bb){
      print("Already Guessed.")
    }else{
      updateImgA(selected_word_val)<-T
    }
  }else{
    if(img_subset@word_type_a == "green" & img_subset@aa){
      print("Already Guessed.")
    }else{
      updateImgB(selected_word_val)<-T
    } 
  }
  return(selected_word_val)
}

updateTurnInfo <- function(player_info, img_subset){
 
  
  if(player_info@player_turn){
    if(img_subset@word_type_b == "green" & img_subset@bb){
      print("Already Guessed.")
    }else{
      if(img_subset@lose_state_a){
        updateGameState(player_info) <- "lost"
      }
      if(img_subset@word_type_a == "green"){
        updateGreenCounter(player_info) <- T
      }
    }
  }else{
    if(img_subset@word_type_a == "green" & img_subset@aa){
      print("Already Guessed.")
    }else{
      if(img_subset@lose_state_b){
        updateGameState(player_info) <- "lost"
      }
      if(img_subset@word_type_b == "green"){
        updateGreenCounter(player_info) <- T
      }
    }
  }
  if(player_info@green_counter == 15){
    updateGameState(player_info) <- "won"
  }
   return(player_info)
}

correctGuessCheck <- function(player_turn, img_subset){
  correct_guess <- "grey"
  if(player_turn){
    if(img_subset@word_type_a == "green"){
      correct_guess <- "green"
    }
    if(img_subset@word_type_a == "black"){
      correct_guess <- "black"
    }
  }else{
    if(img_subset@word_type_b == "green"){
      correct_guess <- "green"
    }
    if(img_subset@word_type_b == "black"){
      correct_guess <- "black"
    }
  }
  #image data
  return(correct_guess)
}

youGuessedYellow <- function(player_info, img_subset){
  res <- F
  if(player_info@player == "A" & img_subset@aa){
    res <- T
  }else if(player_info@player == "B" & img_subset@bb){
    res <- T
  }
  return(res)
}
opponentGuessedYellow <- function(player_info, img_subset){
  res <- F
  if(player_info@player == "B" & img_subset@aa){
    res <- T
  }else if(player_info@player == "A" & img_subset@bb){
    res <- T
  }
  return(res)
}


checkGuess <- function(player_info, img_subset){
  skip_guess <- F
  if(player_info@game_state != "ongoing"){
    print("Game complete")
    skip_guess <- T
  }
  if(getColour(img_subset) == "green"){
    print("Guessed correctly on prior turn")
    skip_guess <- T
  }
  if(player_info@player_turn == T & img_subset@aa == T){
    print("A already guessed and it is A's turn to guess")
    skip_guess <- T
  }
  if(player_info@player_turn == F & img_subset@bb == T){
    print("B already guessed and it is A's turn to guess")
    skip_guess <- T
  }
  return(skip_guess)
}