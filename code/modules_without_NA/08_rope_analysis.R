# ==============================================================================
#  08_rope_analysis.R - ROPE分析
# ==============================================================================
#  概要: HDI-ROPE（Region of Practical Equivalence）比較分析
#  依存: 00_setup.R, 01_main_model.R, 05_additional_models.R
#  出力: ROPE_analysis.csv, ROPE_summary.csv（質問ごとのディレクトリに保存）
# ==============================================================================

library(bayestestR)

# ==============================================================================
#  質問ごとの出力ディレクトリ設定
# ==============================================================================
if (!exists("current_question_label")) {
    stop("Error: current_question_label not defined. This script must be called from main.R with question context.")
}

question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label))

# ==============================================================================
#  ROPE分析関数
# ==============================================================================

#' ROPE分析の実行
#' @param model brmsモデルオブジェクト
#' @param rope_range ROPE範囲（デフォルト: c(-0.1, 0.1)）
#' @param ci 信用区間（デフォルト: 1）
#' @return ROPE分析結果
run_rope_analysis <- function(
    model, 
    rope_range = c(-0.1, 0.1), 
    ci = 1
    ) {
    
    result <- rope(model, range = rope_range, ci = ci)
    return(result)
}

#' 複数モデルのROPE比較
#' @param models 名前付きモデルリスト
#' @param rope_range ROPE範囲
#' @param ci 信用区間
#' @return 比較結果のリスト
compare_rope_multiple <- function(
    models, 
    rope_range = c(-0.1, 0.1), 
    ci = 1
    ) {
    
    results <- lapply(names(models), function(name) {
        cat("\n--- ROPE Analysis for", name, "---\n")
        result <- run_rope_analysis(models[[name]], rope_range, ci)
        print(result)
        return(list(name = name, result = result))
    })
    
    return(results)
}

#' ROPE結果のサマリー作成
#' @param rope_result rope()の結果
#' @param output_file 出力ファイル名（拡張子なし）
#' @return サマリーデータフレーム
summarize_rope <- function(rope_result, output_file = NULL) {
    
    df <- as.data.frame(rope_result)
    
    # 実用的同等性の判断
    df$interpretation <- ifelse(
        df$ROPE_Percentage > 97.5, "Practically equivalent to zero",
        ifelse(
            df$ROPE_Percentage < 2.5, 
            "Practically different from zero",
            "Undecided"
        )
    )
    
    # CSV保存
    if (!is.null(output_file)) {
        csv_file <- paste0(output_file, ".csv")
        write.csv(df, csv_file, row.names = FALSE)
        cat("ROPE summary saved to:", csv_file, "\n")
    }
    
    return(df)
}

#' 効果の実用的重要性評価
#' @param post 事後サンプルデータフレーム
#' @param param_name パラメータ名
#' @param rope_range ROPE範囲
#' @return 評価結果
evaluate_practical_significance <- function(
    post, 
    param_name, 
    rope_range = c(-0.1, 0.1)
    ) {
    
    samples <- post[[param_name]]
    
    # ROPE内の割合
    in_rope <- mean(samples > rope_range[1] & samples < rope_range[2])
    
    # ROPE外（正方向）の割合
    above_rope <- mean(samples >= rope_range[2])
    
    # ROPE外（負方向）の割合
    below_rope <- mean(samples <= rope_range[1])
    
    result <- list(
        parameter = param_name,
        mean = mean(samples),
        sd = sd(samples),
        hdi_89 = mode_hdi(samples, .width = 0.89),
        in_rope_pct = in_rope * 100,
        above_rope_pct = above_rope * 100,
        below_rope_pct = below_rope * 100,
        interpretation = ifelse(
            in_rope > 0.975, 
            "Practically equivalent to zero",
            ifelse(
                in_rope < 0.025, 
                "Practically different from zero",
                "Undecided"
            )
        )
    )
    
    return(result)
}

# ==============================================================================
#  実行
# ==============================================================================

if (exists("current_model")) {
    cat("\n====== Running ROPE Analysis for Q", current_iquest, " (", current_question_label, ") ======\n", sep="")
    
    # メインモデルのROPE分析
    cat("\n--- Main Model ROPE Analysis ---\n")
    rope_main <- run_rope_analysis(current_model)
    print(rope_main)
    
    # サマリー（CSV保存）
    rope_summary <- summarize_rope(rope_main, output_file = file.path(question_output_dir, "ROPE_summary"))
    cat("\n--- ROPE Summary with Interpretation ---\n")
    print(rope_summary[, c("Parameter", "ROPE_Percentage", "interpretation")])
    
    cat("\n====== ROPE Analysis completed ======\n")
} else {
    cat("Warning: Model not found for question", current_iquest, ". Skipping ROPE analysis.\n")
}
