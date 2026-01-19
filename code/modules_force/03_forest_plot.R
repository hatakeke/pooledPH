# ==============================================================================
#  03_forest_plot.R - Forest Plot（modules版を再利用）
# ==============================================================================

# forceモデルが未ロードなら読み込む
if (!exists("model_fullExperimentalParameters_dem") && file.exists(file.path(output_dir, "model_force.rds"))) {
    model_force <- readRDS(file.path(output_dir, "model_force.rds"))
    model_fullExperimentalParameters_dem <- model_force
    model <- model_force
}

source("../modules/02_forest_plot.R")
