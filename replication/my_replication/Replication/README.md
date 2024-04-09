## Replication and Extension of Goodman et al. (2019)

This repository contains the code for replicating and extending the study conducted by Goodman et al. (2019) on the proposed hybrid criterion for statistical significance, termed "Minimum Effect Size Plus p-Value Criterion" (MESP).



#### Original Study

Goodman et al. (2019) introduced a novel criterion for statistical significance, which integrates a minimum effect size threshold with a p-value threshold. Their study involved a simulation analysis comparing the performance of the proposed hybrid criterion with other conventional methods, such as the traditional t-test and a small alpha approach.

#### Replication and Extension

In this project, I aim to replicate the original simulation study while extending it by introducing additional covariates and interaction terms. This extension allows for a more comprehensive evaluation of the performance of the hybrid criterion under various conditions. Specifically, I incorporate the following extensions:



#### Skewness: 

I vary the skewness of the underlying sample distribution to assess the robustness of the hybrid criterion to deviations from normality.



#### Sample Size and MPSD Interaction: 

An interaction term between sample size (n) and the minimum practically significant distance (MPSD) is included to examine how the performance of the hybrid criterion varies across different combinations of these factors.



#### Repository Structure

- methods.R: Defines the statistical methods compared in the simulation study.
  simulation.R: Contains the code for the simulation study, including the generation of data and the application of the different methods.

- analysis_tools.R: Provides functions for analyzing the simulation results, such as calculating power, error rates, and other performance metrics.

- results.R: Generates the main results of the replication and extension, including tables and plots summarizing the performance of the different methods across various conditions.

- README.md: Provides an overview of the project and instructions for running the code.
  Dependencies

- The code requires the following R packages:

  - tidyverse

  - truncnorm
  - sn (for generating skewed data)

  

#### Running the Code

To run the replication and extension:

1. Install the required dependencies.
2. Source the necessary R scripts: methods.R, analysis_tools.R, and simulation.R.
3. Run the results.R script to generate the main results.

The results.R script will output tables and plots summarizing the performance of different methods across various conditions, including the impact of skewness and the sample size and MPSD interaction.



#### Results

The replication and extension provide a more nuanced understanding of the hybrid criterion's performance. Key findings include:

- The hybrid criterion maintains an advantage over the conventional t-test across different levels of power and skewness.
- As the sample size and MPSD interaction term increases, the performance of all methods decreases slightly, but the hybrid criterion still outperforms the conventional t-test.

These results support the original findings of Goodman et al. (2019) while providing additional insights into the boundary conditions and factors influencing the hybrid criterion's performance.



#### Key findings include:
The hybrid criterion demonstrates superiority over the conventional t-test across various levels of power and skewness.

With the increase in the sample size and MPSD interaction term, there is a slight decline in the performance of all methods.  However, the hybrid criterion continues to outperform the conventional t-test.

These outcomes corroborate the initial findings of Goodman et al. (2019) while offering further insights into the boundary conditions and factors influencing the hybrid criterion's effectiveness.

#####  

#### References

Goodman, S. N., Fanelli, D., & Ioannidis, J. P. A. (2019). A proposed hybrid effect size plus p-value criterion for statistical significance. arXiv preprint arXiv:1909.13293.