# Run the simulation
source("code/R/functions/simulation_functions.R")

results <- sim_consolidation(
  n_farms = 100, 
  n_sims = 10, 
  distrib = "lognormal", 
  pareto.xmin = 1, 
  pareto.alpha = 1.5, 
  stop_pct = 0.9
  )

# View the results
print(head(results))  # Display the first few rows of the resulting data frame

# Plot results with both indicators normalized to base 100, combined plot
plot_combined <- plot_simulation_results(
  results, 
  normalize_base100 = T,
  plot_type = "combined"
)
print(plot_combined)
