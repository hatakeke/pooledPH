# ==============================================================================
#  08_rope_analysis.R - ROPE分析（modules版を再利用）
# ==============================================================================

if (!exists("model_fullExperimentalParameters_dem") && file.exists(file.path(output_dir, "model_force.rds"))) {
    model_force <- readRDS(file.path(output_dir, "model_force.rds"))
    model_fullExperimentalParameters_dem <- model_force
    model <- model_force
}

# modules側のROPEは追加モデル(EHI/PDI等)も見ますが、存在しなければスキップされます
source("../modules/08_rope_analysis.R")
