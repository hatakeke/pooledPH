# ==============================================================================
#  08_convergence_diagnostics.R - 収束診断
# ==============================================================================
#  概要: MCMCチェーンの収束診断（Caterpillarプロット等）
#  依存: 00_setup.R, 01_main_model.R
#  出力: Caterpillar.png, Rhat_diagnostics.csv, ESS_diagnostics.csv（質問ごとのディレクトリに保存）
# ==============================================================================

library(ggplot2)
library(ggmcmc)

# ==============================================================================
#  質問ごとの出力ディレクトリ設定
# ==============================================================================
if (!exists("current_question_label")) {
    stop("Error: current_question_label not defined. This script must be called from main.R with question context.")
}

question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label))

# ==============================================================================
#  Caterpillar Plot 関数
# ==============================================================================

#' Caterpillarプロット（トレースプロット）の作成
#' @param model brmsモデルオブジェクト
#' @param params プロットするパラメータ名（ベクトル）またはインデックス
#' @param burnin バーンイン期間の垂直線位置
#' @param output_file 出力ファイル名（拡張子なし）
#' @return ggplotオブジェクト
create_caterpillar_plot <- function(
    model, 
    params = NULL, 
    burnin = 1000,
    output_file = NULL
    ) {
    
    # ggmcmcでモデル変換
    modelADPT_full <- ggs(model)
    
    # 全パラメータ名を取得
    betas <- unique(modelADPT_full$Parameter)
    
    # パラメータが指定されていない場合は、最初の3つの係数（b_）を選択
    if (is.null(params)) {
        selected_params <- betas[grep("^b_", betas)][1:3]
        # 閾値しかない場合などは最初から3つ
        if (all(is.na(selected_params))) selected_params <- betas[1:3]
    } else if (is.numeric(params)) {
        selected_params <- betas[params]
    } else {
        selected_params <- params
    }
    
    # NAの除外
    selected_params <- selected_params[!is.na(selected_params)]
    
    p <- ggplot(
        filter(modelADPT_full, Parameter %in% selected_params),
        aes(x = Iteration, y = value, col = as.factor(Chain))
    ) +
        geom_line(alpha = 0.7) +
        geom_vline(xintercept = burnin, linetype = "dashed", color = "black") +
        facet_grid(Parameter ~ ., scales = 'free_y', switch = 'y') +
        labs(
            title = "MCMC Trace Plots (Caterpillar)", 
            subtitle = paste("Check for 'hairy caterpillar' appearance"),
            col = "Chains",
            y = "Value",
            x = "Iteration"
        ) +
        theme_minimal() +
        theme(strip.text.y.left = element_text(angle = 0))
    
    # ファイル保存
    if (!is.null(output_file)) {
        png_file <- paste0(output_file, ".png")
        # パラメータ数に応じて高さを調整 (1つあたり40mm)
        p_height <- 30 + (length(selected_params) * 30)
        ggsave(png_file, plot = p, units = "mm", width = 180, height = p_height, dpi = 300)
        cat("Caterpillar plot saved to:", png_file, "\n")
    }
    
    return(p)
}

#' 全パラメータのトレースプロット
#' @param model brmsモデルオブジェクト
#' @return ggmcmc変換済みデータ
get_ggs_data <- function(model) {
    ggs(model)
}

#' R-hat収束診断サマリー
#' @param model brmsモデルオブジェクト
#' @param output_file 出力ファイル名（拡張子なし）
#' @return サマリーテーブル
check_rhat <- function(model, output_file = NULL) {
    summary_df <- as.data.frame(summary(model)$fixed)
    
    if ("Rhat" %in% colnames(summary_df)) {
        rhat_check <- summary_df[, c("Estimate", "Est.Error", "Rhat")]
        rhat_check$converged <- rhat_check$Rhat < 1.1
        rhat_check$Parameter <- rownames(rhat_check)
        
        if (!is.null(output_file)) {
            csv_file <- paste0(output_file, ".csv")
            write.csv(rhat_check, csv_file, row.names = FALSE)
            cat("R-hat diagnostics saved to:", csv_file, "\n")
        }
        
        return(rhat_check)
    } else {
        cat("Rhat column not found in model summary.\n")
        return(summary(model)$fixed)
    }
}

#' 有効サンプルサイズのチェック
#' @param model brmsモデルオブジェクト
#' @param output_file 出力ファイル名（拡張子なし）
#' @return サマリー
check_effective_sample_size <- function(model, output_file = NULL) {
    summary_df <- as.data.frame(summary(model)$fixed)
    
    if ("Bulk_ESS" %in% colnames(summary_df)) {
        ess_check <- summary_df[, c("Estimate", "Bulk_ESS", "Tail_ESS")]
        ess_check$adequate_bulk <- ess_check$Bulk_ESS > 400
        ess_check$adequate_tail <- ess_check$Tail_ESS > 400
        ess_check$Parameter <- rownames(ess_check)
        
        if (!is.null(output_file)) {
            csv_file <- paste0(output_file, ".csv")
            write.csv(ess_check, csv_file, row.names = FALSE)
            cat("ESS diagnostics saved to:", csv_file, "\n")
        }
        
        return(ess_check)
    } else {
        cat("ESS columns not found. Model may use different naming convention.\n")
        return(summary(model)$fixed)
    }
}

# ==============================================================================
#  実行
# ==============================================================================

if (exists("current_model")) {
    cat("\n====== Running Convergence Diagnostics for Q", current_iquest, " (", current_question_label, ") ======\n", sep="")
    
    # 主要な固定効果（b_...）をいくつかピックアップしてプロット
    model_vars <- variables(current_model)
    fixed_effects <- model_vars[grep("^b_", model_vars)]
    # Intercept(閾値)以外の最初の3つの効果を表示
    plot_targets <- fixed_effects[!grepl("Intercept", fixed_effects)][1:3]
    
    # Caterpillarプロット
    caterpillar <- create_caterpillar_plot(
        current_model, 
        params = plot_targets,
        output_file = file.path(question_output_dir, "Caterpillar")
    )
    
    # R-hatチェック
    cat("\n--- R-hat Diagnostics ---\n")
    rhat_results <- check_rhat(current_model, output_file = file.path(question_output_dir, "Rhat_diagnostics"))
    
    # 有効サンプルサイズチェック
    cat("\n--- Effective Sample Size Diagnostics ---\n")
    ess_results <- check_effective_sample_size(current_model, output_file = file.path(question_output_dir, "ESS_diagnostics"))
    
    cat("\n====== Convergence Diagnostics completed ======\n")
} else {
    cat("Warning: Model not found for question", current_iquest, ". Skipping convergence diagnostics.\n")
}
