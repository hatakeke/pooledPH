# ==============================================================================
#  main.R - メインスクリプト
# ==============================================================================
#  概要: 全モジュールを順次実行するメインスクリプト
#  使用方法: source("main.R") または Rscript main.R
# ==============================================================================

cat("
================================================================================
Pooled Analysis for Presence Hallucination (PH)
================================================================================
")

# ==============================================================================
#  作業ディレクトリ設定
# ==============================================================================
# setwd("./R/pooledPH/code/modules")

# ==============================================================================
#  モジュール読み込み順序
# ==============================================================================

# 1. セットアップ（必須）
cat("\n[1/10] Loading setup...\n")
source("00_setup.R")

# 2. メインモデル（時間がかかります）
cat("\n[2/10] Building main model...\n")
# 警告：この処理には数分〜数時間かかる場合があります
source("01_main_model.R")

# 3. Forest Plot
cat("\n[3/10] Creating forest plot...\n")
source("02_forest_plot.R")

# 4. Funnel Plot
cat("\n[4/10] Creating funnel plots...\n")
source("03_funnel_plot.R")

# 5. Posterior Predictive Checks
cat("\n[5/10] Running posterior predictive checks...\n")
source("04_posterior_check.R")

# 6. 収束診断
cat("\n[6/10] Running convergence diagnostics...\n")
source("06_convergence_diagnostics.R")

# 7. 係数プロット
cat("\n[7/10] Creating coefficient plots...\n")
source("07_coefficient_plots.R")

# 8. ROPE分析
cat("\n[8/10] Running ROPE analysis...\n")
source("08_rope_analysis.R")

# ==============================================================================
#  オプション：追加モデル（コメントアウト - 必要に応じて実行）
# ==============================================================================

# 追加モデルは時間がかかるため、デフォルトでコメントアウト
# cat("\n[Optional] Building additional models...\n")
# source("05_additional_models.R")
# model_fullExperimentalParameters_dem_order <- build_order_model(data_orig, iquest, questionToColumn)
# model_fullExperimentalParameters_dem_EHI <- build_ehi_model(data_orig, iquest, questionToColumn)
# model_fullExperimentalParameters_dem_PDI <- build_pdi_model(data_orig, iquest, questionToColumn)

# ==============================================================================
#  オプション：媒介分析（コメントアウト - 必要に応じて実行）
# ==============================================================================

# cat("\n[Optional] Running mediation analysis...\n")
# source("09_mediation_analysis.R")
# データ準備後に以下を実行：
# mediationFrame <- compute_relative_increase(selectedData)
# model_PHmedPE <- build_ph_mediated_by_pe(mediationFrame)

# ==============================================================================
#  オプション：シミュレーション（コメントアウト - 非常に時間がかかります）
# ==============================================================================

# cat("\n[Optional] Running simulation...\n")
# source("10_simulation.R")
# post <- posterior_samples(model_fullExperimentalParameters_dem) %>% mutate(iter = 1:n())
# sim_results <- run_prior_effect_simulation(post, N_simulations = 15)

cat("
================================================================================
Analysis Complete
================================================================================

モジュール一覧:
    00_setup.R                - セットアップと設定
    01_main_model.R           - メインフルモデル
    02_forest_plot.R          - Forest Plot
    03_funnel_plot.R          - Funnel Plot
    04_posterior_check.R      - Posterior Predictive Checks
    05_additional_models.R    - 追加モデル（Order, EHI, PDI）
    06_convergence_diagnostics.R - 収束診断
    07_coefficient_plots.R    - 係数プロット
    08_rope_analysis.R        - ROPE分析
    09_mediation_analysis.R   - 媒介分析
    10_simulation.R           - シミュレーション

個別モジュールの使用:
    source('XX_module.R')

")
