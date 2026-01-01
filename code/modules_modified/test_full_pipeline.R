# ==============================================================================
#  test_full_pipeline.R - 全パイプラインのテスト（高速モデル版）
# ==============================================================================
#  概要: モデル構築後のモジュール（02〜10）にエラーがないか確認
#  方法: 小さなiterationでモデルを構築し、全モジュールを実行
# ==============================================================================

cat("
================================================================================
Pipeline Test - Quick Model Build + All Modules Test
================================================================================
\n")

# ==============================================================================
#  Step 1: セットアップ読み込み
# ==============================================================================
cat("[1/11] Loading setup...\n")

source("00_setup.R")

# selectedDataを作成（01_main_model.Rと同様の処理）
selectedData <- filter(data_orig, !is.na(Age))
selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)

cat("  ✓ Setup loaded successfully\n")
cat("  Data available:", exists("selectedData"), "\n")
cat("  selectedData rows:", nrow(selectedData), "\n")

# ==============================================================================
#  Step 2: 高速モデル構築（テスト用）
# ==============================================================================
cat("\n[2/11] Building quick test model (iter=200, warmup=100)...\n")
cat("  This should take ~1-2 minutes...\n")

# モデル式を定義（01_main_model.Rと同じ）
model_formula <- bf(
    Question_ID_7 ~ Condition + Position + Condition:Position + 
        Location + Condition:Location + 
        Previous_Exposure + Condition:Previous_Exposure +
        Cognitive_Load + Condition:Cognitive_Load + 
        Duration_sec + Condition:Duration_sec + 
        Age + Age:Condition + Gender_IsMale +
        (1 + Condition | Experiment_ID)
)

# 事前分布を定義
priors <- c(
    prior(normal(0, 1.5), class = "Intercept"),
    prior(normal(0, 0.5), class = "b"),
    prior(exponential(2), class = "sd")
)

# 高速テスト用のモデル構築
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

# メインモデルと同じ名前で保存（各モジュールが参照できるように）
model_fullExperimentalParameters_dem <- quick_model

cat("  ✓ Quick model built successfully\n")
print(summary(quick_model))

# ==============================================================================
#  Step 3-11: 各モジュールのテスト
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

# テスト結果を保存
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
#  結果サマリー
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
