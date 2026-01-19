# ==============================================================================
#  06_convergence_diagnostics.R - 収束診断（modules版を再利用）
# ==============================================================================

if (!exists("model_fullExperimentalParameters_dem") && file.exists(file.path(output_dir, "model_force.rds"))) {
    model_force <- readRDS(file.path(output_dir, "model_force.rds"))
    model_fullExperimentalParameters_dem <- model_force
    model <- model_force
}

source("../modules/06_convergence_diagnostics.R")
