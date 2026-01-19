# ==============================================================================
#  main.R - Main Script (Multi-Question Support)
# ==============================================================================
#  Summary: Main script to execute all modules sequentially.
#  Support: Multi-question analysis for PH (Q7) and Control (Q1).
#  Usage: source("main.R") or Rscript main.R
# ==============================================================================

cat("
================================================================================
Pooled Analysis for Presence Hallucination (PH) vs Control
Multiple Question Analysis (Q1: Control, Q7: PH)
================================================================================
")

# ==============================================================================
#  Working Directory Configuration
# ==============================================================================
# setwd("./R/pooledPH/code/modules")

# ==============================================================================
#  Module Execution Sequence
# ==============================================================================

# 0. Setup (Required)
cat("\n[00/13] Loading setup...\n")
source("00_setup.R")

# 1. Main Model (Multi-question support - takes time)
cat("\n[01/13] Building main models for PH and Control...\n")
# Warning: This process may take minutes to hours (processing 2 questions)
source("01_main_model.R")

# 2. Posterior CSV Export (Multi-question support)
cat("\n[02/13] Exporting posteriors to CSV...\n")
source("02_export_posterior.R")

# 3. Posterior Comparison Analysis
cat("\n[03/13] Comparing posteriors: PH vs Control...\n")
source("03_compare_posterior.R")

# 4. Score Probability Analysis
cat("\n[04/13] Analyzing score probabilities: Score >= 1...\n")
source("04_probability_analysis.R")

# ==============================================================================
#  Analysis per Question (05-10: Forest Plot, Funnel Plot, etc.)
# ==============================================================================

cat("\n[05-10/13] Running analysis for each question...\n")

for (q_idx in seq_along(analysis_questions)) {
    current_iquest <- analysis_questions[q_idx]
    current_question_label <- question_labels[q_idx]
    current_model <- models_all[[current_question_label]]
    selectedData_current <- selectedData_all[[current_question_label]]
    
    cat("\n", strrep("=", 80), "\n", sep="")
    cat("Processing Question", current_iquest, "-", current_question_label, "\n")
    cat(strrep("=", 80), "\n", sep="")
    
    # 5. Forest Plot
    cat("\n[05/13] Creating forest plot...\n")
    source("05_forest_plot.R")
    
    # 6. Funnel Plot
    cat("\n[06/13] Creating funnel plots...\n")
    source("06_funnel_plot.R")
    
    # 7. Posterior Predictive Checks
    cat("\n[07/13] Running posterior predictive checks...\n")
    source("07_posterior_check.R")
    
    # 8. Convergence Diagnostics
    cat("\n[08/13] Running convergence diagnostics...\n")
    source("08_convergence_diagnostics.R")
    
    # 9. Coefficient Plots
    cat("\n[09/13] Creating coefficient plots...\n")
    source("09_coefficient_plots.R")
    
    # 10. ROPE Analysis
    cat("\n[10/13] Running ROPE analysis...\n")
    source("10_rope_analysis.R")
}

# ==============================================================================
#  Options: Additional Models (Commented out - Run as needed)
# ==============================================================================

# 11. Additional Models (Order, EHI, PDI, Force)
# Executes all sub-models by loading 11_additional_models.R
# (It checks use_sub_model and use_model_cache flags inside 11)
# cat("\n[11/13] Running additional models (Order, EHI, PDI, Force)...\n")
# source("11_additional_models.R")

# ==============================================================================
#  Options: Mediation Analysis (Commented out - Run as needed)
# ==============================================================================

# 12. Mediation Analysis
# cat("\n[12/13] Running mediation analysis...\n")
# source("12_mediation_analysis.R")
# # Execute following after data prep:
# # mediationFrame <- compute_relative_increase(selectedData)
# # model_PHmedPE <- build_ph_mediated_by_pe(mediationFrame)

# ==============================================================================
#  Options: Simulation (Commented out - extremely time-consuming)
# ==============================================================================

# 13. Simulation
# cat("\n[13/13] Running simulation...\n")
# source("13_simulation.R")
# post <- posterior_samples(model_fullExperimentalParameters_dem) %>% mutate(iter = 1:n())
# sim_results <- run_prior_effect_simulation(post, N_simulations = 15)

cat("
================================================================================
Analysis Complete
================================================================================

Module List:
    00_setup.R                   - Setup and configuration
    01_main_model.R              - Main full model
    02_export_posterior.R        - Posterior export to CSV
    03_compare_posterior.R       - Posterior comparison analysis
    04_probability_analysis.R    - Score probability analysis
    05_forest_plot.R             - Forest Plot
    06_funnel_plot.R             - Funnel Plot
    07_posterior_check.R         - Posterior Predictive Checks
    08_convergence_diagnostics.R - Convergence diagnostics
    09_coefficient_plots.R       - Coefficient plots
    10_rope_analysis.R           - ROPE analysis
    11_additional_models.R       - Additional models (Order, EHI, PDI, Force)
    12_mediation_analysis.R      - Mediation analysis
    13_simulation.R              - Simulation

Usage of individual modules:
    source('XX_module.R')

")
