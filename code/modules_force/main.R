# ==============================================================================
#  main.R - module_force メイン
# ==============================================================================

cat("\n================================================================================\n")
cat("module_force: Force_field model using modules posterior as priors\n")
cat("================================================================================\n\n")

cat("[1/7] Setup...\n")
source("00_setup.R")

cat("[2/7] Force model...\n")
source("02_force_model.R")

cat("[3/7] Forest plot...\n")
source("03_forest_plot.R")

cat("[4/7] Funnel plot...\n")
source("04_funnel_plot.R")

cat("[5/7] Posterior checks...\n")
source("05_posterior_check.R")

cat("[6/7] Convergence diagnostics...\n")
source("06_convergence_diagnostics.R")

cat("[7/7] Coefficients + ROPE...\n")
source("07_coefficient_plots.R")
source("08_rope_analysis.R")

cat("\n[module_force] Done.\n")
