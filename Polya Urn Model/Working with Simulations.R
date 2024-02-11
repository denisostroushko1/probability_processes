
library(roll)
library(tictoc)
library(tidyverse)

set.seed(18901723)

Global_iterations = 500
Local_iterations = 10000
Total_rows = Global_iterations * Local_iterations

N_0_number = 50
N_1_number = 50

final_data = 
  data.frame(
    iter = rep(NA, Total_rows), 
    N_0 = rep(NA, Total_rows), 
    N_1 = rep(NA, Total_rows), 
    glob_iter = rep(NA, Total_rows)
  )

tic()
for(i in 1:Global_iterations){
  
  if(i %% 10 == 0){print(paste0("Iteration: ", i))}
  iteration = one_binary_polya_process(N_0 = N_0_number, N_1 = N_1_number, ITERATIONS = Local_iterations)
  iteration$glob_iter = i
  
  final_data[(1+Local_iterations*(i-1)): (Local_iterations*i), ] <- iteration
}

toc()
# 922.413 sec elapsed

final_data_work <- final_data

write.csv(final_data_work, file = 'final_data_work.csv')

# final_data_work <- read_csv('final_data_work.csv') %>% select(-`...1`)

final_data_work <- 
  final_data_work %>% 
  mutate(
    P_0 = N_0 /(N_1 + N_0)
  )


##### figuring out convergence of the 
# https://stats.stackexchange.com/questions/195020/how-to-test-whether-a-time-series-of-measurements-have-converged-to-an-equilibri#:~:text=1%20Answer&text=Calculate%20the%20root%20mean%20square,value%20falls%20below%20the%20threshold.



                                              #### summary things   

# investigate a given global iteration and its convergence 

GI = 3

ggplot(data = final_data_work %>% filter(glob_iter == GI), 
       aes(x = iter, y = P_0)) + 
  theme_minimal() + 
  geom_hline(
    yintercept = final_data_work %>% filter(glob_iter == GI & iter == Local_iterations) %>% select(P_0) %>% unlist(), 
    color = "red"
  ) + 
  geom_line() + 
  ggtitle(
    paste0(
      "Initial P(X = 0) in a sample of ", N_0_number + N_1_number, ": ", round(N_0_number/(N_0_number + N_1_number), 2), 
      "\n Converged P(X = 0): ", 
      round(final_data_work %>% filter(glob_iter == GI & iter == Local_iterations) %>% select(P_0) %>% unlist(), 2)
    )
  )

## summary of final convergence numbers: seems to be sort of symmetric 
final_data_work %>% 
  filter(iter == Local_iterations) %>% 
  select(P_0) %>% 
  summary()

## add final convergence numbers to the global data set 
final_data_work %>% 
  filter(iter == Local_iterations) %>% 
  select(glob_iter, P_0) %>% 
  rename(final_P_0 = P_0)-> final_convergence

final_data_work <- 
  final_data_work %>% 
  left_join(
    final_convergence, 
    by = "glob_iter"
  ) 

## distriution of final convergences 
final_data_work %>% 
  filter(iter == Local_iterations) %>% 
  ggplot(aes(x = P_0)) + 
  geom_histogram() + 
  theme_minimal() + 
  ggtitle(
    paste0(
      "Distribution of final convergence numbers. \n Using ", nrow(final_convergence), " scenarios",
      "\n Initial Average: ", round(N_0_number / (N_0_number + N_1_number), 2),
      "\n Final Average: ", round(mean(final_convergence$final_P_0), 2), 
      '\n Lower 2.5% bound: ', round(quantile(final_convergence$final_P_0, 0.025), 2),
      '\n Upper 97.5% bound: ', round(quantile(final_convergence$final_P_0, 0.975), 2)
    )
  )

# validating that the distribution of final convergences is normal -- due to independence and CLT 
nice_qq_norm = 
  data.frame(
    theoretical = (qqnorm(final_data_work %>% filter(iter == Local_iterations) %>% select(P_0) %>% unlist()))[["x"]],
    sample = (qqnorm(final_data_work %>% filter(iter == Local_iterations) %>% select(P_0) %>% unlist()))[["y"]]
  )

ggplot(data = nice_qq_norm, 
       aes(x = theoretical, y = sample)) + geom_point() + 
  geom_smooth(method = "lm", color = "red", se = T) + theme_minimal()
