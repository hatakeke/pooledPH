# ==============================================================================
#  05_posterior_check.R - Posterior Predictive Checks（modules版を再利用）
# ==============================================================================

if (!exists("model_fullExperimentalParameters_dem") && file.exists(file.path(output_dir, "model_force.rds"))) {
    model_force <- readRDS(file.path(output_dir, "model_force.rds"))
    model_fullExperimentalParameters_dem <- model_force
    model <- model_force
}

source("../modules/04_posterior_check.R")
