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

# 0. セットアップ（必須）
cat("\n[00/13] Loading setup...\n")
source("00_setup.R")

# 1. メインモデル（複数質問対応・時間がかかります）
cat("\n[01/13] Building main models for PH and Control...\n")
# 警告：この処理には数分〜数時間かかる場合があります（2つの質問を処理）
source("01_main_model.R")

# 2. 事後分布CSV（複数質問対応）
cat("\n[02/13] Exporting posteriors to CSV...\n")
source("02_export_posterior.R")

# 3. 事後分布の比較分析（新機能）
cat("\n[03/13] Comparing posteriors: PH vs Control...\n")
source("03_compare_posterior.R")

# 4. スコア確率分析（新機能）
cat("\n[04/13] Analyzing score probabilities: Score >= 1...\n")
source("04_probability_analysis.R")

# ==============================================================================
#  質問ごとの分析（05-10: Forest Plot, Funnel Plot等）
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
    
    # 8. 収束診断
    cat("\n[08/13] Running convergence diagnostics...\n")
    source("08_convergence_diagnostics.R")
    
    # 9. 係数プロット
    cat("\n[09/13] Creating coefficient plots...\n")
    source("09_coefficient_plots.R")
    
    # 10. ROPE分析
    cat("\n[10/13] Running ROPE analysis...\n")
    source("10_rope_analysis.R")
}

# ==============================================================================
#  オプション：追加モデル（コメントアウト - 必要に応じて実行）
# ==============================================================================

# 11. 追加モデル（Order, EHI, PDI, Force）
# 11_additional_models.R を読み込んでサブモデルを一括実行
# （11内部で use_sub_model と use_model_cache のフラグを確認します）
# cat("\n[11/13] Running additional models (Order, EHI, PDI, Force)...\n")
# source("11_additional_models.R")

# ==============================================================================
#  オプション：媒介分析（コメントアウト - 必要に応じて実行）
# ==============================================================================

# 12. 媒介分析
# cat("\n[12/13] Running mediation analysis...\n")
# source("12_mediation_analysis.R")
# # データ準備後に以下を実行：
# # mediationFrame <- compute_relative_increase(selectedData)
# # model_PHmedPE <- build_ph_mediated_by_pe(mediationFrame)

# ==============================================================================
#  オプション：シミュレーション（コメントアウト - 非常に時間がかかります）
# ==============================================================================

# 13. シミュレーション
# cat("\n[13/13] Running simulation...\n")
# source("13_simulation.R")
# post <- posterior_samples(model_fullExperimentalParameters_dem) %>% mutate(iter = 1:n())
# sim_results <- run_prior_effect_simulation(post, N_simulations = 15)

cat("
================================================================================
Analysis Complete
================================================================================

モジュール一覧:
    00_setup.R                - セットアップと設定
    01_main_model.R           - メインフルモデル
    02_export_posterior.R     - 事後分布のCSV出力
    03_compare_posterior.R    - 事後分布の比較分析
    04_probability_analysis.R - スコア確率分析
    05_forest_plot.R          - Forest Plot
    06_funnel_plot.R          - Funnel Plot
    07_posterior_check.R      - Posterior Predictive Checks
    08_convergence_diagnostics.R - 収束診断
    09_coefficient_plots.R    - 係数プロット
    10_rope_analysis.R        - ROPE分析
    11_additional_models.R    - 追加モデル（Order, EHI, PDI, Force）
    12_mediation_analysis.R   - 媒介分析
    13_simulation.R           - シミュレーション

個別モジュールの使用:
    source('XX_module.R')

")
