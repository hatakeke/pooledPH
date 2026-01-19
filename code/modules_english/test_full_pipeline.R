# ==============================================================================
#  test_full_pipeline.R - Full Pipeline Test (Fast Model Version)
# ==============================================================================
#  Summary: Verify that there are no errors in post-modeling modules (02-10).
#  Method: Build a model with a small number of iterations and execute all modules.
# ==============================================================================

cat("
================================================================================
Pipeline Test - Quick Model Build + All Modules Test
================================================================================
\n")

# ==============================================================================
#  Step 1: Load Setup
# ==============================================================================
cat("[1/11] Loading setup...\n")

source("00_setup.R")

# Create selectedData (same process as in 01_main_model.R)
selectedData <- filter(data_orig, !is.na(Age))
selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)

cat("  ✓ Setup loaded successfully\n")
cat("  Data available:", exists("selectedData"), "\n")
cat("  selectedData rows:", nrow(selectedData), "\n")

# ==============================================================================
#  Step 2: Quick Model Construction (for Testing)
# ==============================================================================
cat("\n[2/11] Building quick test model (iter=200, warmup=100)...\n")
cat("  This should take ~1-2 minutes...\n")

# Define model formula (same as 01_main_model.R)
model_formula <- bf(
    Question_ID_7 ~ Condition + Position + Condition:Position + 
        Location + Condition:Location + 
        Previous_Exposure + Condition:Previous_Exposure +
        Cognitive_Load + Condition:Cognitive_Load + 
        Duration_sec + Condition:Duration_sec + 
        Age + Age:Condition + Gender_IsMale +
        (1 + Condition | Experiment_ID)
)

# Define priors
priors <- c(
    prior(normal(0, 1.5), class = "Intercept"),
    prior(normal(0, 0.5), class = "b"),
    prior(exponential(2), class = "sd")
)

# Build model for quick testing
quick_model <- brm(
    formula = model_formula,
    data = selectedData,
    family = cumulative("probit"),
    prior = priors,
    warmup = 100,
    iter = 200,
    chains = 2,
    cores = 2,
    seed = 1234,
    init = 0,
    backend = "rstan",
    silent = 2,
    refresh = 0
)

# Save with the same name as the main model (so each module can reference it)
model_fullExperimentalParameters_dem <- quick_model

cat("  ✓ Quick model built successfully\n")
print(summary(quick_model))

# ==============================================================================
#  Step 3-11: Testing Each Module
# ==============================================================================

test_module <- function(module_name, module_file, step_num) {
    cat(sprintf("\n[%d/11] Testing %s...\n", step_num, module_name))
    
    result <- tryCatch({
        source(module_file)
        cat(sprintf("  ✓ %s completed successfully\n", module_name))
        TRUE
    }, error = function(e) {
        cat(sprintf("  ✗ %s failed:\n", module_name))
        cat("    Error:", conditionMessage(e), "\n")
        FALSE
    })
    
    return(result)
}

# Save test results
results <- list()

results$forest_plot <- test_module("Forest Plot", "02_forest_plot.R", 3)
results$funnel_plot <- test_module("Funnel Plot", "03_funnel_plot.R", 4)
results$posterior_check <- test_module("Posterior Check", "04_posterior_check.R", 5)
results$additional_models <- test_module("Additional Models", "05_additional_models.R", 6)
results$convergence <- test_module("Convergence Diagnostics", "06_convergence_diagnostics.R", 7)
results$coefficient_plots <- test_module("Coefficient Plots", "07_coefficient_plots.R", 8)
results$rope_analysis <- test_module("ROPE Analysis", "08_rope_analysis.R", 9)
results$mediation <- test_module("Mediation Analysis", "09_mediation_analysis.R", 10)
results$simulation <- test_module("Simulation", "10_simulation.R", 11)

# ==============================================================================
#  Summary of Results
# ==============================================================================
cat("\n")
cat("================================================================================\n")
cat("Test Results Summary\n")
cat("================================================================================\n")

passed <- sum(unlist(results))
total <- length(results)

for (name in names(results)) {
    status <- if (results[[name]]) "✓ PASS" else "✗ FAIL"
    cat(sprintf("  %s: %s\n", name, status))
}

cat("--------------------------------------------------------------------------------\n")
cat(sprintf("Total: %d/%d modules passed\n", passed, total))

if (passed == total) {
    cat("\n✓ All modules passed! Safe to run full model.\n")
} else {
    cat("\n✗ Some modules failed. Please fix errors before running full model.\n")
}

cat("================================================================================\n")
