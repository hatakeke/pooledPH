# Presence Hallucination (PH) Bayesian Pooled Analysis Pipeline

This project contains a modularized R pipeline for the Bayesian hierarchical analysis of Presence Hallucination (PH) data using pooled results from multiple experiments.

## Project Scope
The pipeline performs a comprehensive Bayesian analysis using cumulative probit models (via `brms`) to investigate the effects of asynchrony (Condition), body position, location, and various demographic/experimental covariates on PH and Control ratings.

## Directory Structure
- `modules_english/`: Contains the modular R scripts for each step of the analysis.
- `outputs/`: (Created upon run) Stores model objects (`.rds`), plots (`.png`, `.svg`), and results (`.csv`).

## Module Descriptions

1.  **`00_setup.R`**: Global configurations, library loading, data cleaning, and preprocessing.
2.  **`01_main_model.R`**: Construction and estimation of the main Bayesian cumulative probit models for PH (Q7) and Control (Q1).
3.  **`02_export_posterior.R`**: Exports MCMC posterior draws to CSV format for use in sub-models or external analysis.
4.  **`03_compare_posterior.R`**: Comparative analysis between PH and Control distributions.
5.  **`04_probability_analysis.R`**: Calculates the probability of scores being above zero (Effect vs Control).
6.  **`05_forest_plot.R`**: Generates forest plots to visualize experimental effect sizes across studies (Random Effects).
7.  **`06_funnel_plot.R`**: Visualizes publication bias and study heterogeneity.
8.  **`07_posterior_check.R`**: Performs Posterior Predictive Checks (PPC) including ECDF and Histograms.
9.  **`08_convergence_diagnostics.R`**: MCMC health checks including Caterpillar plots, R-hat, and Effective Sample Size (ESS).
10. **`09_coefficient_plots.R`**: Visualizes posterior distributions (Half-eye plots) for all fixed effect coefficients.
11. **`10_rope_analysis.R`**: Region of Practical Equivalence (ROPE) analysis to assess practical significance.
12. **`11_additional_models.R`**: Batch execution of sub-models for covariates (Order, EHI, PDI, Force Field) using main model posteriors as informative priors.
13. **`12_mediation_analysis.R`**: Mediation analysis investigating the relationship between PH and Perceived Effort (PE).
14. **`13_simulation.R`**: Simulation studies to verify the robustness of informative priors.

## Installation & Requirements

### R Version
Statistical analyses were performed using R version 4.3.3 and Rtools 4.3.

### Dependencies
Required packages: `tidyverse`, `readxl`, `brms`, `tidybayes`, `ggmcmc`, `bayestestR`, `extrafont`, `crayon`, `ordinal`, `RVAideMemoire`.

Run the installation script to set up the environment:
```r
source("install_packages.R")
```

## How to Run

### Full Pipeline
To run the entire analysis sequentially:
```r
source("main.R")
```

### Individual Modules
Each module can be run independently after the initial model estimation:
```r
source("00_setup.R")
# Load or build model
current_model <- readRDS("path/to/model.rds")
source("05_forest_plot.R")
```

### Testing
Use the following scripts to check syntax and dependencies:
- `test_modules.R`: Performs syntax checks on all files.
- `test_full_pipeline.R`: Runs a "light" version of the model to verify the entire pipeline logic.

## Logic & Configuration
- **Model Family**: Cumulative Probit with flexible thresholds.
- **Priors**: Uses weakly informative priors by default, or automated informative priors derived from main model posteriors for sub-module analysis.
- **Variable Names**: All original variable names (e.g., `Question_ID_7`, `ConditionAsync`) are preserved to maintain consistency with the experimental dataset.

---
*Created by conversion from Japanese sources.*
