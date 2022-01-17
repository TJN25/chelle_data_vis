# packages
library("shiny")
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(png)
library(shinyjs)
library(ggplot2)
library(shinyalert)
# modules
source("image-generation-module.R")
source("word-selection-module.R")
source("update-game-info-module.R")
source("welcome-module.R")


# functions

source("utils.R")

# global variables
grid_x <- 5
grid_y <- 5

words_length <- 25


word_sets <- c("Harry Potter", "Random words")


# the word and the colours
setClass("word_type", 
         slots=list(word_val="character", 
                    word_type_a="character", 
                    word_type_b="character",
                    word_type_ab="character",
                    lose_state_a="logical", 
                    lose_state_b="logical", 
                    aa="logical", 
                    bb="logical"),
         prototype=list(
           word_type_a = "grey",
           word_type_b = "rgrey",
           word_type_ab = "grey",
           lose_state_a = F,
           lose_state_b = F,
           aa = F,
           bb = F
         ))

#card number and whether it was clicked
setClass("image_selection", 
         contains = "word_type",
         slots = list(filenum="character"))





setGeneric("getColour", function(x) standardGeneric("getColour"))
setGeneric("getWord", function(x) standardGeneric("getWord"))
setGeneric("getLoseStateA", function(x) standardGeneric("getLoseStateA"))
setGeneric("getLoseStateB", function(x) standardGeneric("getLoseStateB"))
setGeneric("updateImgA<-", function(x, value) standardGeneric("updateImgA<-"))
setGeneric("updateImgB<-", function(x, value) standardGeneric("updateImgB<-"))

setMethod(f = "getColour", 
          signature="image_selection",
          function(x){
            if(x@aa == T & x@bb == T){
              x@word_type_ab
            }else if(x@aa){
              x@word_type_a
            }else if(x@bb){
              x@word_type_b
            }else{
              "grey"
            }
          }
)

setMethod(f = "getWord", 
          signature="image_selection",
          function(x){
          x@word_val
          }
)

setMethod(f = "getLoseStateA", 
          signature="image_selection",
          function(x){
           x@lose_state_a 
          }

)

setMethod(f = "getLoseStateB", 
          signature="image_selection",
          function(x){
            x@lose_state_b
          }
          
)

setMethod("updateImgA<-", "image_selection", function(x, value) {
  x@aa <- T
  x
})

setMethod("updateImgA<-", "word_type", function(x, value) {
  x@aa <- T
  x
})

setMethod("updateImgB<-", "image_selection", function(x, value) {
  x@bb <- T
  x
})

setMethod("updateImgB<-", "word_type", function(x, value) {
  x@bb <- T
  x
})

setClass("player_turn", slots = 
           list(
             player="character",
             name="character",
             player_turn="logical",
             turn_counter="numeric",
             game_state="character",
             green_counter="numeric"
           ),
         prototype = list(turn_counter = 0,
                          player_turn=T,
                          game_state="ongoing",
                          green_counter=0)
)



setGeneric("myTurn", function(x, value) standardGeneric("myTurn"))
setGeneric("updateTurnCounter<-", function(x, value) standardGeneric("updateTurnCounter<-"))
setGeneric("updatePlayerTurn<-", function(x, value) standardGeneric("updatePlayerTurn<-"))
setGeneric("updateGameState<-", function(x, value) standardGeneric("updateGameState<-"))
setGeneric("updateGreenCounter<-", function(x, value) standardGeneric("updateGreenCounter<-"))

setMethod("myTurn", "player_turn", function(x, value) {
  if(x@player_turn & x@player == 'A' | !x@player_turn & x@player == 'B'){
    T
  }else{
    F
  }
})

setMethod("updateTurnCounter<-", "player_turn", function(x, value) {
  x@turn_counter <- x@turn_counter + 1
  x
})

setMethod("updateGreenCounter<-", "player_turn", function(x, value) {
  x@green_counter <- x@green_counter + 1
  x
})

setMethod("updatePlayerTurn<-", "player_turn", function(x, value) {
  x@player_turn <- !x@player_turn
  x
})
setMethod("updateGameState<-", "player_turn", function(x, value) {
  x@game_state <- value
  x
})


# global files

harry_potter <- read.csv("harry_potter_codenames.csv")
codenames <- read.csv("codenames_words.csv")



