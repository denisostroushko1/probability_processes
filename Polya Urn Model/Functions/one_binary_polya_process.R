
one_binary_polya_process <- 
  function(
    N_0, 
    N_1, 
    ITERATIONS){
    
    current_sample_vector = 
      c(
        rep(0, N_0), 
        rep(1, N_1), 
        rep(NA, ITERATIONS)
      )
    
    results = 
      data.frame(
        iter = seq(from = 1, to = ITERATIONS, by = 1)
      ) %>% 
      mutate(
        N_0 = N_0, 
        N_1 = N_1
      )
    
    for(i in 1:ITERATIONS){
      
      picked_element = 
        sample(
          current_sample_vector[1:(N_0+N_1+i-1)], 
          size = 1
        )
      
      current_sample_vector[N_0+N_1+i] <- picked_element
      
      if(picked_element == 0){results$N_0[i:ITERATIONS] = results$N_0[i] + 1}
      if(picked_element == 1){results$N_1[i:ITERATIONS] = results$N_1[i] + 1}
    }
    
    return(results)
  }

