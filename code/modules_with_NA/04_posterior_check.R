# ==============================================================================
#  04_posterior_check.R - Posterior Predictive Checks
# ==============================================================================
#  概要: 事後予測チェックによるモデル評価
#  依存: 00_setup.R, 01_main_model.R
#  出力: PosteriorCheck_ECDF.png, PosteriorCheck_Hist.png, ObservedHistogram.png（質問ごとのディレクトリに保存）
# ==============================================================================

library(ggplot2)

# ==============================================================================
#  質問ごとの出力ディレクトリ設定
# ==============================================================================
if (!exists("current_question_label")) {
    stop("Error: current_question_label not defined. This script must be called from main.R with question context.")
}

question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label))

# ==============================================================================
# Posterior Predictive Check 関数
# ==============================================================================

#' ECDFオーバーレイによる事後予測チェック
#' @param model brmsモデルオブジェクト
#' @param output_file 出力ファイル名（拡張子なし）
#' @return ggplotオブジェクト
create_pp_check_ecdf <- function(model, output_file = NULL) {
    
    # ECDF オーバーレイプロット作成
    p <- pp_check(model, type = "ecdf_overlay")
    
    # ggplot2でカスタマイズ
    p <- p + 
        labs(
            title = "Posterior Predictive Check: ECDF Overlay",
            x = "Response Category (Quest)",
            y = "Cumulative Proportion"
        ) +
        scale_color_manual(
            name = "Data Type",
            values = c("black", "red"),
            labels = c("Observed Responses", "Model Predictions")
        ) +
        theme_minimal()
    
    # ファイル保存
    if (!is.null(output_file)) {
        png_file <- paste0(output_file, ".png")
        ggsave(png_file, plot = p, units = "mm", width = 120, height = 80, dpi = 300)
        cat("Posterior check (ECDF) saved to:", png_file, "\n")
    }
    
    return(p)
}

#' ヒストグラム型事後予測チェック
#' @param model brmsモデルオブジェクト
#' @param output_file 出力ファイル名（拡張子なし）
#' @return ggplotオブジェクト
create_pp_check_hist <- function(model, output_file = NULL) {
    p <- pp_check(model)
    
    if (!is.null(output_file)) {
        png_file <- paste0(output_file, ".png")
        ggsave(png_file, plot = p, units = "mm", width = 120, height = 80, dpi = 300)
        cat("Posterior check (Hist) saved to:", png_file, "\n")
    }
    
    return(p)
}

#' 実測データのヒストグラム
#' @param data データフレーム
#' @param question_col 質問列名
#' @param output_file 出力ファイル名（拡張子なし）
#' @return ggplotオブジェクト
create_observed_histogram <- function(data, question_col = "Question_ID_7", output_file = NULL) {
    
    p <- ggplot(data, aes_string(x = question_col)) +
        geom_bar() +
        scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
        labs(
            x = paste0("PH rating (observed data)"), 
            y = "Counts"
        ) + 
        theme_minimal()
    
    if (!is.null(output_file)) {
        png_file <- paste0(output_file, ".png")
        ggsave(png_file, plot = p, units = "mm", width = 100, height = 70, dpi = 300)
        cat("Observed histogram saved to:", png_file, "\n")
    }
    
    return(p)
}

# ==============================================================================
# 実行
# ==============================================================================

if (exists("current_model")) {
    cat("\n====== Creating Posterior Predictive Checks for Q", current_iquest, " (", current_question_label, ") ======\n", sep="")
    
    # 実測データヒストグラム（対応する質問列を動的に取得）
    question_col <- questionToColumn[current_iquest]
    histo <- create_observed_histogram(selectedData_current, names(selectedData_current)[question_col], output_file = file.path(question_output_dir, "ObservedHistogram"))
    
    # pp_check（標準）
    pp_hist <- create_pp_check_hist(current_model, output_file = file.path(question_output_dir, "PosteriorCheck_Hist"))
    
    # ECDF オーバーレイ
    pp_ecdf <- create_pp_check_ecdf(current_model, output_file = file.path(question_output_dir, "PosteriorCheck_ECDF"))
    
    cat("====== Posterior Predictive Checks completed ======\n")
} else {
    cat("Warning: Model not found for question", current_iquest, ". Skipping posterior checks.\n")
}
