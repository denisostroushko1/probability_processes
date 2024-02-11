
rm(list = ls())

library(tidyverse)

### define a deck of cards 
deck_of_cards <- rep(1:13, each = 4)

### shuffle the deck 7 times

shuffle_new_deck <- function(){
  deck_of_cards %>% 
    sample(., size = length(.)) %>% 
    sample(., size = length(.))%>% 
    sample(., size = length(.))%>% 
    sample(., size = length(.))%>% 
    sample(., size = length(.))%>% 
    sample(., size = length(.))%>% 
    sample(., size = length(.))
}

shuffled_deck <- shuffle_new_deck()

### number of unknown cards to play with 
N_play_with = 5
### lay out N cards next to the deck for guessing 
cards_to_play_with <- shuffled_deck[1:N_play_with]

### cards left in the deck after setting up the game 
left_over_deck <- shuffled_deck[-(1:N_play_with)]

### flip over the top card in the deck 
fliped_top_card <- left_over_deck[1]
### update leftover deck of unknown cards 
left_over_deck <- left_over_deck[-1]


# define a simpler stratey 
simple_strategy <- function(x, threshold = 7){
  ifelse(x <= threshold, "higher", "lower")
}

counting_strategy <- function(x, set_cards = cards_to_play_with, left_deck = left_over_deck){
  
  all_unknown_cards <- c(set_cards, left_deck)
  all_lower <- all_unknown_cards[all_unknown_cards <= x] %>% length()
  all_higher <- all_unknown_cards[all_unknown_cards > x] %>% length()
  
  ifelse(all_lower >= all_higher, "lower", "higher")
}

random_strategy <- function(x){
  sample(c("lower", "higher"), size = 1, p = c(0.5, 0.5))
}

play_one_game <- function(strategy_fucntion){
  ## this varaiable counts how many cards in a row we guessed 
  ### if we make a mistake we start over a deal new cards 
  counter = 0
  
  ### this variable will keep track of how many times we fail until we finish the game 
  counter_history = integer()
  
  ### at the beginning of the game most recent card is the one we put on top of the deck
  ### inside the game loop, it gets updated 
  
  most_recent_card <- fliped_top_card
  
  while(counter < N_play_with){
    
    ## make a decison
    decision = strategy_fucntion(most_recent_card)
    ## observed event
    observed_card = cards_to_play_with[1]
    observed_outcome = ifelse(observed_card > most_recent_card, "higher", "lower")
    
    ## outcome of one bet 
    bet_outcome = observed_outcome == decision
    ## if we run out of cards we need to shuffle the deck again and set up new cards 
    
    ## if we get the bet right, we add to the counter and continue the loop 
    if(bet_outcome == T){
      counter = counter + 1
      
      ## update most recent card 
      most_recent_card = observed_card
      cards_to_play_with = cards_to_play_with[-1]
    }else{
      ## record previous counter
      counter_history = c(counter_history, counter)
      ## set new one at the beginning again 
      counter = 0
      
      ## deal new cards 
      cards_to_play_with <- left_over_deck[1:N_play_with]
      
      ### cards left in the deck after setting up the game 
      left_over_deck <- left_over_deck[-(1:N_play_with)]
      
      ### flip over the top card in the deck 
      fliped_top_card <- left_over_deck[1]
      ### update leftover deck of unknown cards 
      left_over_deck <- left_over_deck[-1]
      
      ### flip card to be on top the game and initiate the game 
      most_recent_card <- fliped_top_card
      
    } 
    ### if there are less than 6 cards left in the leftover deck we need to shuffle again 
    ### we basically recognize that we do not have enough cards to deal out the round so we start over 
    
    if(length(left_over_deck) < (N_play_with + 1)){
      left_over_deck <- shuffle_new_deck()
    }
  }
      
    ### if it so happens that the game finished on the first go at N cards, then we just set counter 
    # history to 
    if(length(counter_history) == 0){
      counter_history = N_play_with
    }
  
  return(counter_history)
}

set.seed(8172)
games <- 100000 
results_dumb <- vector(mode = "list", length = games)
results_smart <- vector(mode = "list", length = games)
results_random <- vector(mode = "list", length = games)

N_play_with = 6

for(i in 1:games){

  if(i %% 1000 == 0){print(paste0("start game ", i))}
  
  shuffled_deck <- shuffle_new_deck()
  
  ### number of unknown cards to play with 
  
  ### lay out N cards next to the deck for guessing 
  cards_to_play_with <- shuffled_deck[1:N_play_with]
  
  ### cards left in the deck after setting up the game 
  left_over_deck <- shuffled_deck[-(1:N_play_with)]
  
  ### flip over the top card in the deck 
  fliped_top_card <- left_over_deck[1]
  ### update leftover deck of unknown cards 
  left_over_deck <- left_over_deck[-1]
  
  results_dumb[[i]]   <- play_one_game(strategy_fucntion = simple_strategy)
  results_smart[[i]]  <- play_one_game(strategy_fucntion = counting_strategy)
  results_random[[i]] <- play_one_game(strategy_fucntion = random_strategy)
}
############################################################
results_dumb_df <- 
  bind_rows(
    lapply(
      seq_along(results_dumb), function(i) {
        data.frame(
          Entry = results_dumb[[i]],
          Index = i
          )
        }
      )
    )

write_rds(results_dumb_df, "Game of Bus/dumb.rds")

results_strategy_df <- 
  bind_rows(
    lapply(
      seq_along(results_smart), function(i) {
        data.frame(
          Entry = results_smart[[i]],
          Index = i
          )
        }
      )
    )

write_rds(results_strategy_df, "Game of Bus/counting.rds")

results_random_df <- 
  bind_rows(
    lapply(
      seq_along(results_random), function(i) {
        data.frame(
          Entry = results_random[[i]],
          Index = i
          )
        }
      )
    )

write_rds(results_random_df, "Game of Bus/random.rds")
############################################################
length_of_games_dumb <- 
  results_dumb_df %>% 
  group_by(Index) %>% 
  summarise(L = n())

summary(length_of_games_dumb$L)

ggplot(data = length_of_games_dumb, 
       aes(x = L)) + 
  theme_classic() + 
  geom_histogram(binwidth = 1, fill = 'lightgrey', color = 'black') + 
  labs(x = "Game Length", 
       y = "Count", 
       title = paste0("% of Games Finished in 1 Go: ", 
                      round(length(which(length_of_games_dumb$L == 1))/nrow(length_of_games_dumb),3)*100))
  

############################################################
length_of_games_smart <- 
  results_strategy_df %>% 
  group_by(Index) %>% 
  summarise(L = n())

summary(length_of_games_smart$L)

ggplot(data = length_of_games_smart, 
       aes(x = L)) + 
  theme_classic() + 
  geom_histogram(binwidth = 1, fill = 'lightgrey', color = 'black') + 
  labs(x = "Game Length", 
       y = "Count", 
       title = paste0("% of Games Finished in 1 Go: ", 
                      round(length(which(length_of_games_smart$L == 1))/nrow(length_of_games_smart),3)*100))

############################################################
length_of_games_random <- 
  results_random_df %>% 
  group_by(Index) %>% 
  summarise(L = n())

summary(length_of_games_random$L)

ggplot(data = length_of_games_random, 
       aes(x = L)) + 
  theme_classic() + 
  geom_histogram(binwidth = 1, fill = 'lightgrey', color = 'black') + 
  labs(x = "Game Length", 
       y = "Count", 
       title = paste0("% of Games Finished in 1 Go: ", 
                      round(length(which(length_of_games_random$L == 1))/nrow(length_of_games_random),3)*100))

############################################################

rbind(
  length_of_games_random %>% 
    summarise(
      mean_L = mean(L), 
      median = median(L), 
      q_25 = quantile(L, 0.25),
      q_75 = quantile(L, 0.75), 
      p_1 = length(which(L == 1))/nrow(.),
      p_10_plus = length(which(L >= 10))/nrow(.)
    ),
  
  length_of_games_dumb %>% 
    summarise(
      mean_L = mean(L), 
      median = median(L), 
      q_25 = quantile(L, 0.25),
      q_75 = quantile(L, 0.75), 
      p_1 = length(which(L == 1))/nrow(.),
      p_10_plus = length(which(L >= 10))/nrow(.)
    ),
  
  length_of_games_smart %>% 
    summarise(
      mean_L = mean(L), 
      median = median(L), 
      q_25 = quantile(L, 0.25),
      q_75 = quantile(L, 0.75), 
      p_1 = length(which(L == 1))/nrow(.),
      p_10_plus = length(which(L >= 10))/nrow(.)
    )
  
) %>% 
  cbind(data.frame(Strategy = c("Random", "Simple", "Counting")), 
        .)

 
