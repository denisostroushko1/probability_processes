
library(tidyverse)

#########################################################################
# loop over probability options:
#   from 0.1 to 0.9 in 0.1 increments 
#   for each increment do 10,000 draws and capture the number of streaks 
########################################################################

success_probs = seq(from = 0.1, to = 0.9, by = 0.1)
draws_n = 10000

results = 
  data.frame(
    true_prob = rep(success_probs, draws_n), 
    draw_id = rep(seq(from = 1, to = draws_n, by = 1), length(success_probs))
  ) %>% 
  arrange(true_prob, draw_id)

set.seed(125897)

results <- 
  results %>% 
  mutate(
    draw_result = as.numeric(mapply(rbernoulli,n = 1, p = true_prob)), 
    streak = NA,
    streak_id = NA
  )

# Now i guess we need two types of streaks: success streaks and same outcome streaks 

results[results$draw_id == 1, ]$streak <- 1
results[results$draw_id == 1, ]$streak_id <- 1

for(ROW in 2:nrow(results)){
  
  if(ROW %% 1000 == 0){print(paste("On row ", ROW))}
  
  results$streak[ROW] <- 
    ifelse(
      results$draw_result[ROW-1] == results$draw_result[ROW], 
      results$streak[ROW-1] + 1, 
      1
    )
  
  results$streak_id[ROW] <- 
    ifelse(
      results$draw_result[ROW-1] == results$draw_result[ROW], 
      results$streak_id[ROW-1], 
      results$streak_id[ROW-1] + 1
    )
  
}

results[results$draw_id == 1, ]$streak <- 1

####################
# summarize results 

summary_results <- 
  results %>% 
  group_by(
    true_prob, streak_id 
  ) %>% 
  summarize(
    outcome = unique(draw_result), 
    streak_length = max(streak)
  ) %>% 
  ungroup()


ggplot(
  data = summary_results, 
  aes(x = log10(streak_length), 
      color = true_prob, 
      group = true_prob)) + 
    geom_density() + 
  scale_colour_gradientn(colors = rainbow(5)) + 
  scale_x_continuous(
    labels = function(x){round(10^x, 2)}
  )


ggplot(
  data = summary_results %>% filter(outcome == 1), 
  aes(x = (streak_length), 
      color = true_prob, 
      group = true_prob)) + 
    geom_density()
