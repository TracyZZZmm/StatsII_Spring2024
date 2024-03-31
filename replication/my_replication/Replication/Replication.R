# Load necessary libraries
library(tidyverse) # Load the tidyverse library, providing rich functionality for data manipulation and visualization
library(truncnorm) # Load the truncnorm library for generating truncated normal distribution random numbers


# R scripts:
source("methods.R") # Source the R script containing methods for the simulation
source("analysis_tools.R") # Source the R script containing analysis tools
source("simulation.R") # Source the R script for simulation

# Modify simulation.R to include skewness and interaction terms
nr_simulations <- 100000 # Number of simulations
mu_0 <- 100 # Assumed true mean

# Include additional columns for skewness and interaction terms
result_cols <- c(
  "mu", "sigma", "n", "mpsd", "fact",   # Basic statistics and actual outcome
  sapply(METHODS, function(x) x@str),  # Decisions of various methods
  "skewness", "n_mpsd_interaction"  # Skewness and interaction terms
)

results <- matrix(nrow = nr_simulations, ncol = length(result_cols))

set.seed(1) # Set the random seed for reproducibility
for (i in 1:nr_simulations) {  # Start simulation loop
  if ((i / nr_simulations * 100) %% 10 == 0) {  # Output a message every 10% of the simulations
    message("Simulation ", i, " of ", nr_simulations)
  }
  
  # Generate random parameters for the simulation
  mu    <- sample(75:125, size = 1)  # Generate a random mean
  sigma <- sample(4:60, size = 1)     # Generate a random standard deviation
  n     <- sample(5:100, size = 1)    # Generate a random sample size
  mpsd  <- sample(2:20, size = 1)     # Generate a random mpsd
  
  # Generate data for the simulation
  x <- rnorm(n, mean = mu, sd = sigma)
  
  # Calculate skewness (placeholder value, replace with actual calculation)
  skewness <- 0
  
  # Record simulation case
  results[i, 1:4] <- c(mu, sigma, n, mpsd)
  results[i, 5] <- abs(mu - mu_0) > mpsd  # Record actual outcome
  
  # Calculate interaction term
  n_mpsd_interaction <- n * mpsd
  
  # Record facts and decisions
  for (j in 1:length(METHODS)) {
    method <- METHODS[[j]]
    results[i, 5 + j] <- getDecision(method, x = x, mu_0 = mu_0, mpsd = mpsd)
  }
  
  # Record additional covariates
  results[i, length(result_cols) - 1] <- skewness
  results[i, length(result_cols)] <- n_mpsd_interaction
}

results <- data.frame(results)  # Convert results to a data frame
colnames(results) <- result_cols  # Set column names for the results
results$fact <- as.logical(results$fact)  # Convert actual outcome to logical

for (method in METHODS) {  # Convert decision results of methods to logical
  results[[method@str]] <- as.logical(results[[method@str]])
}

# Post-process the simulation results
results$power <- nominal_power(0.05, results$sigma, results$n, results$mpsd)
results$relative_mpsd <- results$mpsd / results$sigma

# Analysis including skewness and interaction terms
# Add analysis for skewness and interaction terms here, define analysis process based on your research questions

# Now generate the plots with the additional covariates
# Generate plots using additional covariates


# Example:
# results %>%
#   group_by(skewness, n_mpsd_interaction) %>%
#   summarise(
#     mean_power = mean(power),
#     mean_relative_mpsd = mean(relative_mpsd)
#   )

# Now generate the plots with the additional covariates
# plot_impact_of_power(results, methods = METHODS)
# plot_impact_of_MPSD(results, methods = METHODS)

# Print a completion message
message("Simulation and analysis complete.")


# Code for generating main findings plots (add after the simulation and analysis completion message)

# Example of generating plots:
plot_data <- results %>%
  group_by(skewness, n_mpsd_interaction) %>%
  summarise(
    mean_power = mean(power),
    mean_relative_mpsd = mean(relative_mpsd)
  )

ggplot(plot_data, aes(x = n_mpsd_interaction, y = mean_power, color = as.factor(skewness))) +
  geom_line() +
  labs(x = "Interaction Term", y = "Mean Power", title = "Impact of Interaction Term on Power") +
  theme_minimal()



## Figure 1
plot_impact_of_power(results)


## Figure 2
plot_impact_of_MPSD(results)


