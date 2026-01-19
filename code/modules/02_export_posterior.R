# ==============================================================================
#  02_export_posterior.R - 事後分布のCSV出力（複数質問対応）
# ==============================================================================
#  概要:
#    - 01_main_model.R で計算された複数質問のモデルから
#      全パラメータの事後drawと要約をCSVに保存します
#  依存: 00_setup.R, 01_main_model.R
# ==============================================================================

source("00_setup.R")

#' 事後分布をCSVにエクスポートする関数
#' @param model brmsモデルオブジェクト
#' @param output_dir 出力ディレクトリ
#' @param file_stub ファイル名スタブ
#' @return 保存されたファイルパスのリスト
export_posterior_to_csv <- function(model, output_dir, file_stub = "main_model") {
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    post_draws <- brms::posterior_samples(model) %>%
        dplyr::mutate(iter = seq_len(n()))

    draws_path <- file.path(output_dir, paste0("posterior_draws_", file_stub, ".csv"))
    readr::write_csv(post_draws, draws_path)
    cat("Posterior draws saved to:", draws_path, "\n")

    sum_mat <- brms::posterior_summary(model, probs = c(0.055, 0.5, 0.945))
    sum_df <- as.data.frame(sum_mat)
    sum_df$parameter <- rownames(sum_df)
    rownames(sum_df) <- NULL
    sum_df <- sum_df %>%
        dplyr::select(parameter, dplyr::everything())

    summary_path <- file.path(output_dir, paste0("posterior_summary_", file_stub, ".csv"))
    readr::write_csv(sum_df, summary_path)
    cat("Posterior summary saved to:", summary_path, "\n")

    invisible(list(draws_path = draws_path, summary_path = summary_path))
}

# ==============================================================================
#  複数質問の事後分布をエクスポート
# ==============================================================================

cat("\n====== Exporting posteriors for all questions ======\n")

for (idx in seq_along(analysis_questions)) {
    question_label <- question_labels[idx]
    
    cat("\n--- Exporting", question_label, "---\n")
    
    # 質問ごとの出力ディレクトリ
    question_output_dir <- file.path(output_dir, paste0("Q", analysis_questions[idx], "_", question_label))
    
    # モデルを取得
    model <- models_all[[question_label]]
    
    # CSVにエクスポート
    export_posterior_to_csv(model, output_dir = question_output_dir, file_stub = "main_model")
}

cat("\n====== Export completed ======\n")
