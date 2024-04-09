# Load necessary libraries
library(tidyverse) # Load the tidyverse library, which provides rich data manipulation and visualization functions
library(truncnorm) # Load the truncnorm library for generating truncated normal distribution random numbers

# R Script:
source("methods.R") # Import the R script containing simulation methods
source("analysis_tools.R") # Import the R script containing analysis tools
source("simulation.R") # Import the R script for simulation

# Modify simulation.R to include skewness and interaction terms
nr_simulations <- 100000 # Number of simulations
mu_0 <- 100 # Assumed true mean

# Modify simulation.R file to include skewness and interaction terms ("skewness", "n_mpsd_interaction")
result_cols <- c(
  "mu", "sigma", "n", "mpsd", "fact",   # Basic statistics and actual results
  sapply(METHODS, function(x) x@str),  # Decisions for different methods
  "skewness", "n_mpsd_interaction"  # Skewness and interaction terms
)
# result_cols: This is a vector containing column names to store various columns of simulated results. Where:
# "mu" represents the mean of simulated data;
# "sigma" represents the standard deviation of simulated data;
# "n" represents the sample size of simulated data;
# "mpsd" represents the minimum practical significance difference of simulated data;
# "fact" represents the actual result, whether the null hypothesis is rejected or not;
# For each method in METHODS, use the sapply function to generate corresponding column names to store the decision results of that method;
# "skewness" represents the skewness of simulated data;
# "n_mpsd_interaction" represents the interaction term between skewness and minimum practical significance difference.

# Above, by introducing skewness and interaction terms, increasing the number of simulations, and setting the assumed true mean, the simulation experiment can be made more realistic and credible,
# thereby better simulating and analyzing data situations in the real world.

# Create a matrix named results with nr_simulations rows and length(result_cols) columns to store the results of the simulation experiment.
results <- matrix(nrow = nr_simulations, ncol = length(result_cols))
# nr_simulations is the number of simulation experiments, i.e., the number of simulated data to be generated.
# length(result_cols) is the number of columns per row in the results matrix, i.e., the number of variables in the results, including basic statistics of simulated data, decision results of various methods, and additional skewness and interaction terms.




set.seed(1) # Set the random seed for reproducibility
for (i in 1:nr_simulations) {  # Start the simulation loop
  if ((i / nr_simulations * 100) %% 10 == 0) {  # Output a message every 10% of the simulation is completed
    message("Simulating ", i, " out of ", nr_simulations, " simulations")
  }
  
  # Below is how the original author instructed to modify the settings
  # Generate simulation parameters
  mu    <- sample(75:125, size = 1)  # Generate a random mean
  sigma <- sample(4:60, size = 1)     # Generate a random standard deviation
  n     <- sample(5:100, size = 1)    # Generate a random sample size
  mpsd  <- sample(2:20, size = 1)     # Generate a random mpsd
  
  # Generate simulated data
  x <- rnorm(n, mean = mu, sd = sigma)
  
  # Calculate skewness (placeholder value, replace with actual calculation)
  skewness <- 0
  
  # Record simulation conditions
  results[i, 1:4] <- c(mu, sigma, n, mpsd)
  results[i, 5] <- abs(mu - mu_0) > mpsd  # Record actual result
  
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
# The above content includes:
# Skewness represents the skewness, which is a statistic describing the symmetry of the data distribution.
# n_mpsd_interaction represents the interaction term, which is the product of sample size (n) and minimum practical significance difference (mpsd).
# This value can be used to indicate the interaction effect between sample size and minimum practical significance difference in the simulation experiment.

# n is the sample size, representing the number of samples drawn from the population in each simulation experiment.
# n directly affects the power of statistical tests, i.e., the ability to reject the null hypothesis given the effect size.

# mpsd is the minimum practical significance difference, representing the minimum significance level set when conducting hypothesis tests,
# which is an important parameter for hypothesis testing, indicating how much the sample mean needs to deviate from the assumed true mean to be considered significant.
# It is used to determine whether the sample mean significantly deviates from the assumed true mean.




results <- data.frame(results)  # Convert results to a data frame
colnames(results) <- result_cols  # Set column names for the results
results$fact <- as.logical(results$fact)  # Convert actual results to logical values




for (method in METHODS) {  # Convert decision results of methods to logical values
  results[[method@str]] <- as.logical(results[[method@str]])
}




# Post-process the simulation results
# Calculate the power of each sample in the simulation results.
# Power refers to the ability to reject the null hypothesis at a given significance level (typically 0.05).
results$power <- nominal_power(0.05, results$sigma, results$n, results$mpsd)
# Calculate the relative minimum detectable effect,
# which is the ratio of the minimum detectable effect (mpsd) to the standard deviation (sigma).
# This value can be used to measure the relative size of the minimum detectable effect given the standard deviation.
results$relative_mpsd <- results$mpsd / results$sigma


head(results$relative_mpsd)
summary(results$relative_mpsd)



# Analysis including skewness and interaction terms
# Define the analysis process including skewness and interaction terms according to the research question

# Now generate plots using additional covariates
# Example:
# results %>%
#   group_by(skewness, n_mpsd_interaction) %>%
#   summarise(
#     mean_power = mean(power),
#     mean_relative_mpsd = mean(relative_mpsd)
#   )

# Now generate plots using additional covariates
# plot_impact_of_power(results, methods = METHODS)
# plot_impact_of_MPSD(results, methods = METHODS)

# Print completion message
message("Simulation and analysis completed.")


# Generate plots for main findings (to be added after the simulation and analysis completion message)

# Example of generating plots:
# The first plot shows the relationship between the interaction term and the mean power.
# The x-axis represents the values of the interaction term, and the y-axis represents the values of the mean power.
# Different colors of lines represent different skewness values.
# By observing this plot, we can analyze the impact of the interaction term on power and understand how the interaction term affects power under different skewness conditions.
plot_data <- results %>%
  group_by(skewness, n_mpsd_interaction) %>%
  summarise(
    mean_power = mean(power),
    mean_relative_mpsd = mean(relative_mpsd)
  )

# The second plot may be to show the relationship between the interaction term and the mean relative minimum detectable effect.
# However, the specific code for generating the second plot is not included in the provided code, so the content and purpose of the second plot cannot be determined.
ggplot(plot_data, aes(x = n_mpsd_interaction, y = mean_power, color = as.factor(skewness))) +
  geom_line() +
  labs(x = "Interaction Term", y = "Mean Power", title = "Impact of Interaction Term on Power") +
  theme_minimal()




## Figure 1
# Figure 1 - Impact of Power:
# This figure shows the variation of power under different parameter conditions.
# Through this figure, we can analyze how the power of the simulation experiment changes under different simulation parameter settings.
plot_impact_of_power(results)


## Figure 2
# Figure 2 - Impact of Minimum Detectable Effect:
# This figure shows the impact of different parameter conditions on the minimum detectable effect (MPSD) of the simulation results.
# Through this figure, we can analyze how the minimum detectable effect of the simulation experiment is affected under different simulation parameter settings.
plot_impact_of_MPSD(results)

