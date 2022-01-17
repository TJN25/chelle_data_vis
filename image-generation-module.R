
makePlot <- function(img_subset, player_info){
  text_colour <- ifelse(getColour(img_subset) == "black", "grey", "black")
  p <- ggplot() +
    annotate(geom="text", x=3, y=30, label=getWord(img_subset),color=text_colour, size = 7) +
    theme(panel.background = element_rect(fill = getColour(img_subset)), 
          axis.line = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank())
  if(getColour(img_subset) == "grey"){
    youY <- youGuessedYellow(player_info = player_info, img_subset = img_subset)
    oppY <- opponentGuessedYellow(player_info = player_info, img_subset = img_subset)
    
    
    if(youY & oppY){
      p <- p +   annotate(geom="text", x=2.8, y=27, label="You guessed",color=text_colour, size = 3) + 
        annotate(geom="text", x=3.15, y=33, label="Opponent guessed",color=text_colour, size = 3) +
        
        annotate(geom="text", x=3.35, y=33.5, label=" ",color=text_colour, size = 3) +
        annotate(geom="text", x=2.65, y=26.5, label=" ",color=text_colour, size = 3)
      
    }else if(oppY){
      p <- p +   annotate(geom="text", x=2.8, y=27, label="You guessed",color=text_colour, size = 3) + 
        
        annotate(geom="text", x=3.35, y=33.5, label=" ",color=text_colour, size = 3) +
        annotate(geom="text", x=2.65, y=26.5, label=" ",color=text_colour, size = 3)
    }else if(youY){
      p <- p + annotate(geom="text", x=3.15, y=33, label="Opponent guessed",color=text_colour, size = 3) +
        annotate(geom="text", x=3.35, y=33.5, label=" ",color=text_colour, size = 3) +
        annotate(geom="text", x=2.65, y=26.5, label=" ",color=text_colour, size = 3)
    }
  }
  return(p)
}