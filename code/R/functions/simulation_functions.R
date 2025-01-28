# Corrected Gini Index
gini_index <- function(y) {
  n <- length(y)
  sorted_y <- sort(y)
  gini <- sum((2 * 1:n - n - 1) * sorted_y) / (n * sum(sorted_y))
  return(gini)
}

# Consolidation 
consolidate <- function(f_areas) {
  # Randomly select two indices from the farmland_areas
  idxs <- sample(1:length(f_areas), 2)
  total_area <- sum(f_areas[idxs])
  f_areas <- f_areas[-idxs]
  f_areas <- c(f_areas, total_area)
  return(sort(f_areas))
}

# Pareto random number generation
rpareto <- function(n, alpha, xmin) {
  u <- runif(n)
  return(xmin / (u^(1/alpha)))
}

# Modified simulation function with percentage stop
sim_consolidation <- function(n_farms = 10, n_sims = 1, distrib = "", 
                              pareto.xmin = 0, pareto.alpha = 0, 
                              ln.mean = 5, ln.sd = 1, stop_pct = 0.95) {
  
  min_obs <- as.integer(0.01 * n_farms)
  
  # Initialize an empty data frame to store results for all simulations
  results_df <- data.frame()
  
  for (sim in 1:n_sims) {
    # Set seed for reproducibility
    set.seed(sim)
    
    # Draw initial farm size distribution
    if (distrib == "pareto") {
      f_areas <- rpareto(n_farms, pareto.alpha, pareto.xmin)
    } else if (distrib == "lognormal") {
      f_areas <- exp(rnorm(n_farms, mean = ln.mean, sd = ln.sd))
    } else {
      stop("Please choose a distribution between pareto and lognormal")
    }
    
    # Initialize vectors to store values for this simulation
    gini_values <- c(gini_index(f_areas))
    avg_farm_size <- c(mean(f_areas))
    sd_values <- c(sd(f_areas))
    steps <- c(1)
    
    # Loop through consolidation steps
    lim <- n_farms - 1
    
    # Determine the stopping point based on stop_pct
    stop_at <- ceiling(stop_pct * lim)
    
    for (step in 1:lim) {
      f_areas <- consolidate(f_areas)
      gini_values <- c(gini_values, gini_index(f_areas))
      avg_farm_size <- c(avg_farm_size, mean(f_areas))
      sd_values <- c(sd_values, sd(f_areas))
      steps <- c(steps, step + 1)
      
      # Stop simulation at the percentage defined by stop_pct
      if (step >= stop_at || length(f_areas) <= min_obs) break
    }
    
    # Combine results for this simulation into a data frame
    sim_df <- data.frame(
      sim = rep(sim, length(gini_values)),
      step = steps,
      gini = gini_values,
      avg_size = avg_farm_size,
      sd = sd_values
    )
    
    # Append to the overall results data frame
    results_df <- rbind(results_df, sim_df)
  }
  
  return(results_df)
}

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Plotting function to visualize Gini and Farm Size with optional base-100 normalization
plot_simulation_results <- function(results_df, normalize_base100 = TRUE, plot_type = "combined") {
  
  # If normalize_base100 is TRUE, convert Gini and average farm size to index base 100
  if (normalize_base100) {
    results_df <- results_df %>%
      group_by(sim) %>%
      mutate(
        gini_index100 = (gini / first(gini)) * 100,
        avg_size_index100 = (avg_size / first(avg_size)) * 100
      ) %>%
      ungroup()
  } else {
    results_df <- results_df %>%
      mutate(
        gini_index100 = gini,
        avg_size_index100 = avg_size
      )
  }
  
  # Generate the plot based on the selected plot_type
  if (plot_type == "combined") {
    # Combined plot with all simulations overlaid
    plot <- ggplot(results_df, aes(x = step)) +
      
      # Gini index line plot on primary y-axis
      geom_line(aes(y = gini_index100, color = "Gini Index"), size = 1) +
      
      # Average farm size line plot on secondary y-axis, using log scale
      geom_line(aes(y = avg_size_index100, color = "Average Farm Size"), linetype = "dashed", size = 1) +
      
      # Primary y-axis for Gini index
      scale_y_continuous(
        name = "Gini Index (Base 100)",
        sec.axis = sec_axis(~ ., name = "Average Farm Size (Base 100)")
      ) +
      
      # Labels, theme, and titles
      labs(
        title = "Gini Index and Average Farm Size (Base 100) Progression",
        x = "Consolidation Step",
        color = "Indicator"
      ) +
      theme_minimal()
    
  } else if (plot_type == "facet") {
    # Facet plot with each simulation in its own subplot
    plot <- ggplot(results_df, aes(x = step)) +
      
      # Gini index line plot on primary y-axis
      geom_line(aes(y = gini_index100, color = "Gini Index"), size = 1) +
      
      # Average farm size line plot on secondary y-axis, using log scale
      geom_line(aes(y = avg_size_index100, color = "Average Farm Size"), linetype = "dashed", size = 1) +
      
      # Primary y-axis for Gini index
      scale_y_continuous(
        name = "Gini Index (Base 100)",
        sec.axis = sec_axis(~ ., name = "Average Farm Size (Base 100)")
      ) +
      
      # Labels, theme, and titles
      labs(
        title = "Gini Index and Average Farm Size (Base 100) Progression by Simulation",
        x = "Consolidation Step",
        color = "Indicator"
      ) +
      
      # Facet by simulation
      facet_wrap(~ sim) +
      theme_minimal()
  }
  
  # Return the plot
  return(plot)
}
