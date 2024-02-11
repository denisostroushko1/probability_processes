
rm(list = ls())

library(tidyverse)



transition_m =
          
  matrix(  # to good | to sick | to dead 
          c(0.6,       0.2,       0.2, # from good 
            0.3,       0.4,       0.3,  # from sick 
            0,         0,         1  # from dead 
           ), 
          
          nrow = 3, ncol = 3) 

emission_m = 
  matrix( #     good | sick | dead 
          c(
                0.8,    0.3,  0.1, # prob nothing given col.state
                0.1,    0.3,  0.4, # prob marks 
                0.1,    0.4,  0.5  # dead leaf
          ),  
         nrow = 3, ncol = 3) 


rownames(emission_m) <- c("g", "m", 'd')

forward_HMM_k <- function(
    
    ## returns: joint pmf for a given observed signal 
    ##            and possible hidden states of a markov chain 
  
    TRANSITION_M,  ## obv. a matrix  
    EMISSION_M, ## matrix of emitted probabilities 
    EMITTED_SIGNALS,  ## needs to be a vector 
    STARTING_P, ## needs to be a vector
    DESIRED_K
){
  
  ## initialize 
  first_signal = EMITTED_SIGNALS[1] # first signal 
  STARTING_P * EMISSION_M[rownames(EMISSION_M) == first_signal] -> working_F
  
  if(DESIRED_K > 1){
    ## future states 2, 3, etc... 
    for( STEPS in 2:DESIRED_K){
      
      current_F = rep(NA, length(STARTING_P))
      
      for( STATES in 1:length(STARTING_P)){
        
        current_F[STATES] <- 
          EMISSION_M[
            ## get conditional probs given potential HMM state 
            rownames(EMISSION_M) ==  
              ## get probability for the Hidden state we work with here 
                       EMITTED_SIGNALS[STEPS]][STATES]  * 
                                                          
           ## multiply previous Forward F's  with transition probabilities 
          sum(working_F * TRANSITION_M[,STATES] )
        
      }
      
      working_F = current_F
    }
  }else{
      current_F = working_F
    }
  
  return(current_F)
}

backward_HMM_k <- 
  function(
    TRANSITION_M, 
    EMISSION_M, 
    EMITTED_SIGNALS, 
    STARTING_P, 
    LAST_K
  ){
  
  current_B = rep(1, length(STARTING_P))
  ## past states states 2, 1, etc... {2 and 1 is all we get in this example} 
  
  backward_state_sequence = seq(from = length(EMITTED_SIGNALS) - 1, to = LAST_K, by = -1)
  
  for( STEPS in backward_state_sequence){
  
    working_B = rep(NA, length(STARTING_P))
    
    for( STATES in 1:length(STARTING_P)){
      
      working_B[STATES] <- 
        sum(
          EMISSION_M[rownames(EMISSION_M) == 
                       EMITTED_SIGNALS[STEPS + 1]] * 
          TRANSITION_M[STATES,] * 
          current_B
        )
    }
    
    current_B = working_B
    
  }
  
  return(current_B)
  }


conditional_pmf <- 
  function(
    DESIRED_STATE, 
    DESIRED_K_H, 
    
    EMITTED_SIGNALS_H, 
    TRANSITION_M_H, 
    EMISSION_M_H,
    STARTING_P_H, 
    
    LAST_K_H
    
  ){
    
    forward_HMM_k(
      TRANSITION_M = TRANSITION_M_H, 
      EMISSION_M = EMISSION_M_H, 
      EMITTED_SIGNALS = EMITTED_SIGNALS_H, 
      STARTING_P = STARTING_P_H, 
      DESIRED_K = DESIRED_K_H
    ) -> forward_part 
    
    backward_part <- 
      
      ifelse(
        ## if we want to predict X at the time of last observed 
        ##    signal, then we will assign B_k all 1 
        ##    otherwise, we will use the fucntion to calculate our stuff 
        DESIRED_K_H == length(EMITTED_SIGNALS_H),  
          
        rep(1, length(STARTING_P_H)), 
        
        backward_HMM_k(
          TRANSITION_M = TRANSITION_M_H, 
          EMISSION_M = EMISSION_M_H, 
          EMITTED_SIGNALS = EMITTED_SIGNALS_H, 
          STARTING_P = STARTING_P_H, 
          LAST_K = LAST_K_H
        )
      )
    
    ### now we need to recreate 
    
    prod = forward_part * backward_part
    
    results = prod[DESIRED_STATE]/sum(prod)
    
    return(results)
  }



### loop over the sequence of observed signals and 
signal_seq <- c(
  sample(c("g", "m", "d"), size = 10, replace = TRUE, prob = c(0.7, 0.2, 0.1)), 
  sample(c("g", "m", "d"), size = 10, replace = TRUE, prob = c(0.1, 0.5, 0.4)), 
  sample(c("g", "m", "d"), size = 10, replace = TRUE, prob = c(0, 0.5, 0.5))
)

prob_seq1 <- rep(NA, length(signal_seq))
prob_seq2 <- rep(NA, length(signal_seq))
prob_seq3 <- rep(NA, length(signal_seq))

starting_prob = c(0.6,0.2, 0.2)

for(it in 1:length(signal_seq)){
  
  signal_seq_it <- signal_seq[1:it]
  
  conditional_pmf(
    DESIRED_STATE = 1, 
    DESIRED_K_H = length(signal_seq_it), 
    TRANSITION_M_H = transition_m, 
    EMISSION_M_H = emission_m, 
    STARTING_P_H = starting_prob, 
    EMITTED_SIGNALS_H = signal_seq_it , 
    
    LAST_K_H = 1
  ) -> prob_seq1[it]
  
  conditional_pmf(
    DESIRED_STATE = 2, 
    DESIRED_K_H = length(signal_seq_it), 
    TRANSITION_M_H = transition_m, 
    EMISSION_M_H = emission_m, 
    STARTING_P_H = starting_prob, 
    EMITTED_SIGNALS_H = signal_seq_it , 
    
    LAST_K_H = 1
  ) -> prob_seq2[it]
  
  
  conditional_pmf(
    DESIRED_STATE = 3, 
    DESIRED_K_H = length(signal_seq_it), 
    TRANSITION_M_H = transition_m, 
    EMISSION_M_H = emission_m, 
    STARTING_P_H = starting_prob, 
    EMITTED_SIGNALS_H = signal_seq_it , 
    
    LAST_K_H = 1
  ) -> prob_seq3[it]
}


#########
# 
plot_data = 
  data.frame(obs = rep(seq(from = 1, to = length(signal_seq), by = 1), 3), 
             prob = c(prob_seq1, prob_seq2, prob_seq3), 
             type = c(rep("good", length(prob_seq1)),
                      rep("sick", length(prob_seq2)),
                      rep("dead", length(prob_seq3))
                      )
             )

ggplot(data = plot_data, 
       aes(x = obs, 
           y = prob, 
           group = type, 
           color = type)) + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1)) + 
  scale_x_continuous(breaks = seq(from = 1, to = max(plot_data$obs), length.out = 4), 
                     labels = function(x){round(x)}) +
  
  geom_line()
  # geom_line(color = "#fcc203") + 
  # geom_ribbon(aes(ymin = prob -sd, ymax = prob + sd), fill = "grey", alpha = 0.25)


################
# now I need to sample some probabilities, run the path, store, and get confidence ribbon 

# sample probabilities 

set.seed(125)
reps = 100
sd_control = 0.1

signal_seq <- c(
  sample(c("g", "m", "d"), size = 10, replace = TRUE, prob = c(0.7, 0.2, 0.1)), 
  sample(c("g", "m", "d"), size = 10, replace = TRUE, prob = c(0.3, 0.3, 0.4)), 
  sample(c("g", "m", "d"), size = 10, replace = TRUE, prob = c(0.15, 0.35, 0.5))
)

clip_01 <- function(x){
  returned <- ifelse(x < 0, 0, x)
  returned <- ifelse(returned > 1, 1, returned)
  return(returned)
}

results_replicate <- 
  data.frame(
    obs =  rep(NA, 3 * length(signal_seq) * reps), 
    prob = rep(NA, 3 * length(signal_seq) * reps), 
    type = rep("", 3 * length(signal_seq) * reps), 
    replicate_id = rep(NA, 3 * length(signal_seq) * reps)
    )

start_p_good = 0.9
start_p_sick = 0.1
start_p_dead = 0

for(i in 1:reps){
  
  print(i)
  
  {
    p_good_good <- clip_01(rnorm(n = 1, mean = 0.8, sd = sd_control))
    p_good_sick <- clip_01(rnorm(n = 1, mean = 0.2, sd = sd_control))
    p_good_dead = 1 - p_good_good - p_good_sick
    
    p_sick_good <- clip_01(rnorm(n = 1, mean = 0.4, sd = sd_control))
    p_sick_sick <- clip_01(rnorm(n = 1, mean = 0.4, sd = sd_control))
    p_sick_dead = 1 - p_sick_good - p_sick_sick
    
    p_dead_good = 0
    p_dead_sick = 0
    p_dead_dead = 1
    
    transition_m =
              
      matrix(  # to good | to sick | to dead 
              c(p_good_good,       p_good_sick,       p_good_dead, # from good 
                p_sick_good,       p_sick_sick,       p_sick_dead,  # from sick 
                p_dead_good,       p_dead_sick,       p_dead_dead  # from dead 
               ), 
              
              ncol = 3, nrow = 3) %>% t()
  }
  
  {
    p_nothin_w_good <- clip_01(rnorm(n = 1, mean = 0.8, sd = sd_control))
    p_mark_w_good   <- clip_01(rnorm(n = 1, mean = 0.1, sd = sd_control))
    p_leaf_w_dead = 1 - p_nothin_w_good - p_mark_w_good
    
    p_nothin_w_sick <- clip_01(rnorm(n = 1, mean = 0.5, sd = sd_control))
    p_mark_w_sick <- clip_01(rnorm(n = 1, mean = 0.4, sd = sd_control))
    p_leaf_w_sick = 1 - p_sick_good - p_sick_sick
    
    p_nothin_w_dead = clip_01(rnorm(n = 1, mean = 0.3, sd = sd_control))
    p_mark_w_dead = clip_01(rnorm(n = 1, mean = 0.3, sd = sd_control))
    p_leaf_w_dead = 1 - p_nothin_w_dead - p_mark_w_dead
    
    emission_m = 
      matrix( #     good | sick | dead 
              c(
                    p_nothin_w_good,    p_nothin_w_sick,  p_nothin_w_dead, # prob nothing given col.state
                    p_mark_w_good,      p_mark_w_sick,  p_mark_w_dead, # prob marks 
                    p_leaf_w_dead,      p_leaf_w_sick,  p_leaf_w_dead  # dead leaf
              ),  
             nrow = 3, ncol = 3)
  }
  
  rownames(emission_m) <- c("g", "m", 'd')
  
  prob_seq1 <- rep(NA, length(signal_seq))
  prob_seq2 <- rep(NA, length(signal_seq))
  prob_seq3 <- rep(NA, length(signal_seq))
  
  p1 = clip_01(rnorm(n = 1, mean = start_p_good, sd= sd_control))
  p2 = clip_01(rnorm(n = 1, mean = start_p_sick, sd = sd_control))
  
  starting_prob = c(p1,
                    p2, 
                    1 - p1 - p2)
  
  for(it in 1:length(signal_seq)){
    
    signal_seq_it <- signal_seq[1:it]
    
    conditional_pmf(
      DESIRED_STATE = 1, 
      DESIRED_K_H = length(signal_seq_it), 
      TRANSITION_M_H = transition_m, 
      EMISSION_M_H = emission_m, 
      STARTING_P_H = starting_prob, 
      EMITTED_SIGNALS_H = signal_seq_it , 
      
      LAST_K_H = 1
    ) -> prob_seq1[it]
    
    conditional_pmf(
      DESIRED_STATE = 2, 
      DESIRED_K_H = length(signal_seq_it), 
      TRANSITION_M_H = transition_m, 
      EMISSION_M_H = emission_m, 
      STARTING_P_H = starting_prob, 
      EMITTED_SIGNALS_H = signal_seq_it , 
      
      LAST_K_H = 1
    ) -> prob_seq2[it]
    
    
    conditional_pmf(
      DESIRED_STATE = 3, 
      DESIRED_K_H = length(signal_seq_it), 
      TRANSITION_M_H = transition_m, 
      EMISSION_M_H = emission_m, 
      STARTING_P_H = starting_prob, 
      EMITTED_SIGNALS_H = signal_seq_it , 
      
      LAST_K_H = 1
    ) -> prob_seq3[it]
  }
  
  s = (i-1)*90 + 1
  f = (i)*90 
  
  results_replicate[(s:f), ] = 
    data.frame(obs = rep(seq(from = 1, to = length(signal_seq), by = 1), 3), 
             prob = c(prob_seq1, prob_seq2, prob_seq3), 
             type = c(rep("good", length(prob_seq1)),
                      rep("sick", length(prob_seq2)),
                      rep("dead", length(prob_seq3))
                      ), 
             replicate_id = i
             )
}

head(results_replicate)

ggplot(data = results_replicate, 
       aes(x = obs, 
           y = prob, 
           group = type, 
           color = type)) + 
  theme_classic() + 
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.1), 
                     limits = c(0,1)) + 
  scale_x_continuous(breaks = seq(from = 1, to = max(results_replicate$obs), length.out = 4), 
                     labels = function(x){round(x)}) + 
  geom_smooth(method = "loess")

