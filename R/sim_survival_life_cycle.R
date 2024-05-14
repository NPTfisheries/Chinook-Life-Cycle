#' Title Function to iterate across life-cycle.
#'
#' Author Ryan Kinzer
#' Created March 8, 2024

sim_life_cycle <- function(
    iterations,
    num_generations,
    spawners, sex_p, fecundity, parr_s, smolt_s, hydro_s, ocean_s, adult_s, spawn_s
){
  
  
  
  all_results <- vector("list", iterations)
  
  for (i in 1:iterations){
    
    # Initialize a list to store results
    results_list <- vector("list", num_generations)
    
    # Initial population
    results_list[[1]] <- life_cycle(spawners, sex_p, fecundity, parr_s, smolt_s, hydro_s, ocean_s, adult_s, spawn_s)
    results_list[[1]]$generation <- 1
    results_list[[1]]$iteration <- i
    
    # Iterate over generations
    for (j in 2:num_generations) {
      # Use the results from the previous iteration as inputs
      prev_results <- results_list[[j - 1]]
      results_list[[j]] <- life_cycle(prev_results$adults_trib, sex_p, fecundity, parr_s, smolt_s, hydro_s, ocean_s, adult_s, spawn_s)
      results_list[[j]]$generation <- j
      results_list[[j]]$iteration <- i
    }
    
    all_results[[i]] <- results_list
    
  }
  
  return(all_results)
    
}
  
