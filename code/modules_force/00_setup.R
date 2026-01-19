# ==============================================================================
#  00_setup.R - module_force セットアップ
# ==============================================================================
#  方針:
#    - modules の 00_setup.R を流用してデータ読込・前処理を統一
#    - modules で保存した事後draw CSVを参照するパスを定義
# ==============================================================================

source("../modules/00_setup.R")

# modules 側で保存した「メインモデルの事後draw」
posterior_draws_path <- "../modules/outputs/posterior_draws_main_model.csv"
posterior_summary_path <- "../modules/outputs/posterior_summary_main_model.csv"

if (!file.exists(posterior_draws_path)) {
    cat("[module_force] posterior_draws CSV not found:\n  ", posterior_draws_path, "\n")
    cat("[module_force] 先に modules で事後draw CSVを出力してください。例: \n")
    cat("  - (推奨) cd ../modules ; Rscript export_posterior.R\n")
}
