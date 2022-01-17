

getWordsAndTypes <- function(seed_val, word_list, words_length){
  set.seed(seed_val)
  selected_words <- sample(x = word_list[,1], size = 25, replace = F)
  all_pos <- 1:25
  greens <- sample(all_pos, size = 15, replace = F)
  not_greens <- all_pos[-greens]
  black_1_a <- sample(greens[10:15], size = 1)
  black_1_b <- sample(greens[1:6], size = 1)
  remaining_black <- sample(not_greens, size = 3, replace = F)
  colours <- data.frame(values = 1:25, colour_a = "grey", colours_b = "grey", colours_both = "grey")
  colours[greens[1:9],c(2,4)] <- "green"
  colours[greens[7:15],c(3,4)] <- "green"
  colours[black_1_a,c(2,4)] <- "black"
  colours[black_1_b,c(3,4)] <- "black"
  colours[remaining_black[1:2],c(2,4)] <- "black"
  colours[remaining_black[2:3],c(3,4)] <- "black"

  selected_word_list <- list()
  for(i in 1:words_length){
    selected_word_list[[i]] <- new("word_type",
                          word_val=selected_words[i], 
                          word_type_a=colours[i,2], 
                          word_type_b=colours[i,3], 
                          word_type_ab=colours[i,4],
                          lose_state_a=ifelse(colours[i,2] == "black", T,F), 
                          lose_state_b=ifelse(colours[i,3] == "black", T,F))
  }
  return(selected_word_list)
  }

