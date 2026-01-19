# ==============================================================================
#  main.R - メインスクリプト（複数質問対応）
# ==============================================================================
#  概要: 全モジュールを順次実行するメインスクリプト
#  対応: PH (Q7) と Control (Q1) の複数質問分析
#  使用方法: source("main.R") または Rscript main.R
# ==============================================================================

cat("
================================================================================
Pooled Analysis for Presence Hallucination (PH) vs Control
Multiple Question Analysis (Q1: Control, Q7: PH)
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
cat("\n[1/13] Loading setup...\n")
source("00_setup.R")

# 2. メインモデル（複数質問対応・時間がかかります）
cat("\n[2/13] Building main models for PH and Control...\n")
# 警告：この処理には数分〜数時間かかる場合があります（2つの質問を処理）
source("01_main_model.R")

# 2.1 事後分布CSV（複数質問対応）
cat("\n[2.1/13] Exporting posteriors to CSV...\n")
source("export_posterior.R")

# 2.2 事後分布の比較分析（新機能）
cat("\n[2.2/13] Comparing posteriors: PH vs Control...\n")
source("compare_posterior.R")

# ==============================================================================
#  質問ごとの分析（3-8: Forest Plot, Funnel Plot等）
# ==============================================================================

cat("\n[3-8/13] Running analysis for each question...\n")

for (q_idx in seq_along(analysis_questions)) {
    current_iquest <- analysis_questions[q_idx]
    current_question_label <- question_labels[q_idx]
    current_model <- models_all[[current_question_label]]
    selectedData_current <- selectedData_all[[current_question_label]]
    
    cat("\n", strrep("=", 80), "\n", sep="")
    cat("Processing Question", current_iquest, "-", current_question_label, "\n")
    cat(strrep("=", 80), "\n", sep="")
    
    # 3. Forest Plot
    cat("\n[3/13] Creating forest plot...\n")
    source("02_forest_plot.R")
    
    # 4. Funnel Plot
    cat("\n[4/13] Creating funnel plots...\n")
    source("03_funnel_plot.R")
    
    # 5. Posterior Predictive Checks
    cat("\n[5/13] Running posterior predictive checks...\n")
    source("04_posterior_check.R")
    
    # 6. 収束診断
    cat("\n[6/13] Running convergence diagnostics...\n")
    source("06_convergence_diagnostics.R")
    
    # 7. 係数プロット
    cat("\n[7/13] Creating coefficient plots...\n")
    source("07_coefficient_plots.R")
    
    # 8. ROPE分析
    cat("\n[8/13] Running ROPE analysis...\n")
    source("08_rope_analysis.R")
}

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
