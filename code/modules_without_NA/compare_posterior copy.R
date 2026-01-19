# ==============================================================================
#  12_compare_posterior.R - 事後分布の比較分析（PH vs Control）
# ==============================================================================
#  概要: PHとControlの事後分布を比較し、グラフとCSVで出力
#  依存: 00_setup.R, 01_main_model.R
#  出力: 比較グラフ, 比較CSV
# ==============================================================================

source("00_setup.R")

cat("\n====== Comparing posteriors: PH vs Control ======\n")

# 比較用の出力ディレクトリ
comparison_output_dir <- file.path(output_dir, "Comparison_PH_vs_Control")
if (!dir.exists(comparison_output_dir)) {
    dir.create(comparison_output_dir, recursive = TRUE)
    cat("Created comparison output directory:", comparison_output_dir, "\n")
}

# ==============================================================================
#  関数：2つの事後分布を比較する
# ==============================================================================

#' 2つのモデルの事後分布を抽出して比較するデータフレームを作成
#' @param model1 モデル1（PH）
#' @param model2 モデル2（Control）
#' @param model1_name モデル1の名前
#' @param model2_name モデル2の名前
#' @return 比較用データフレーム
compare_posteriors <- function(model1, model2, model1_name = "PH", model2_name = "Control") {
    
    # 事後drawを抽出（全チェーンをプール）
    # as_draws_df を使うことで、チェーンが縦に結合されたきれいなDFになります
    draws1 <- as.data.frame(brms::as_draws_df(model1))
    draws2 <- as.data.frame(brms::as_draws_df(model2))
    
    # 以下の列を除外（チェーン・反復情報・メタデータ）
    cols_to_exclude <- c("lprior", "lp__", ".chain", ".iteration", ".draw")
    params1 <- setdiff(colnames(draws1), cols_to_exclude)
    params2 <- setdiff(colnames(draws2), cols_to_exclude)
    common_params <- intersect(params1, params2)
    
    cat("\n✓ Processing full pooled posterior (all chains combined)\n")
    cat("  Chains pooled:", nrow(draws1), "total posterior samples\n")
    cat("  Common parameters found:", length(common_params), "\n\n")
    
    # 比較用データフレームを構築
    comparison_list <- list()
    
    for (param in common_params) {
        # 基本統計量
        summary1 <- summary(draws1[[param]])
        summary2 <- summary(draws2[[param]])
        
        # 中央値と確信区間
        draws1_values <- draws1[[param]]
        draws2_values <- draws2[[param]]
        
        med1 <- median(draws1_values)
        med2 <- median(draws2_values)
        
        q_055_1 <- quantile(draws1_values, 0.055)
        q_945_1 <- quantile(draws1_values, 0.945)
        q_055_2 <- quantile(draws2_values, 0.055)
        q_945_2 <- quantile(draws2_values, 0.945)
        
        # 平均値
        mean1 <- mean(draws1_values)
        mean2 <- mean(draws2_values)
        
        # 標準偏差
        sd1 <- sd(draws1_values)
        sd2 <- sd(draws2_values)
        
        # 差分の計算
        S <- min(length(draws1_values), length(draws2_values))
        diff_draws <- sample(draws1_values, size = S, replace = TRUE) - sample(draws2_values, size = S, replace = TRUE)
        diff_median <- median(diff_draws)
        diff_mean <- mean(diff_draws)
        diff_sd <- sd(diff_draws)
        diff_q_055 <- quantile(diff_draws, 0.055)
        diff_q_945 <- quantile(diff_draws, 0.945)
        
        # 0を含むかチェック（信頼区間）
        contains_zero <- (diff_q_055 < 0) & (diff_q_945 > 0)
        
        comparison_list[[param]] <- data.frame(
            Parameter = param,
            PH_Median = med1,
            Control_Median = med2,
            PH_Mean = mean1,
            Control_Mean = mean2,
            PH_SD = sd1,
            Control_SD = sd2,
            PH_Lower_89 = q_055_1,
            PH_Upper_89 = q_945_1,
            Control_Lower_89 = q_055_2,
            Control_Upper_89 = q_945_2,
            Diff_Median = diff_median,
            Diff_Mean = diff_mean,
            Diff_SD = diff_sd,
            Diff_Lower_89 = diff_q_055,
            Diff_Upper_89 = diff_q_945,
            Diff_Contains_Zero = contains_zero,
            stringsAsFactors = FALSE
        )
    }
    
    comparison_df <- do.call(rbind, comparison_list)
    rownames(comparison_df) <- NULL
    
    return(comparison_df)
}

# ==============================================================================
#  比較データの作成とCSV出力
# ==============================================================================

comparison_df <- compare_posteriors(
    models_all[["PH"]], 
    models_all[["Control"]], 
    model1_name = "PH",
    model2_name = "Control"
)

# CSVに保存
comparison_csv_path <- file.path(comparison_output_dir, "posterior_comparison_ph_vs_control.csv")
readr::write_csv(comparison_df, comparison_csv_path)
cat("Comparison CSV saved to:", comparison_csv_path, "\n")

cat("\nComparison summary:\n")
print(head(comparison_df, 10))

# ==============================================================================
#  可視化：密度プロット（全パラメータ・カテゴリ別）
# ==============================================================================

cat("\nCreating comprehensive density plots for all parameters...\n")

# 全チェーンをプールした事後drawを抽出
ph_draws <- as.data.frame(brms::as_draws(models_all[["PH"]]))
control_draws <- as.data.frame(brms::as_draws(models_all[["Control"]]))

# チェーン・反復情報を含む列を除外
cols_to_exclude <- c("lprior", "lp__", ".chain", ".iteration", ".draw")
ph_params <- setdiff(colnames(ph_draws), cols_to_exclude)
control_params <- setdiff(colnames(control_draws), cols_to_exclude)
common_params_actual <- intersect(ph_params, control_params)

cat("  Extracted", length(ph_params), "parameters from PH draws\n")
cat("  Extracted", length(control_params), "parameters from Control draws\n")
cat("  Common parameters:", length(common_params_actual), "\n\n")

# データフレームから列を再抽出
ph_draws <- ph_draws[, ph_params]
control_draws <- control_draws[, control_params]

# 実際に存在するパラメータ名に基づいて分類
# X1.b_Intercept.1. のような形式のため、正規表現を調整
threshold_params <- grep("b_Intercept\\.", common_params_actual, value = TRUE)
fixed_effect_params <- grep("^X.*\\.b_", common_params_actual, value = TRUE)
other_fixed_params <- setdiff(fixed_effect_params, threshold_params)
intercept_params <- grep("^Intercept", common_params_actual, value = TRUE)
random_effect_params <- grep("sd_|r_|cor_", common_params_actual, value = TRUE)

cat("  Found:", length(threshold_params), "threshold params\n")
cat("  Found:", length(other_fixed_params), "other fixed effect params\n")
cat("  Found:", length(intercept_params), "intercept params\n")
cat("  Found:", length(random_effect_params), "random effect params\n\n")

# グループ定義
param_groups <- list(
    "Group 1: Threshold Parameters" = threshold_params,
    "Group 2: Fixed Effect Parameters" = other_fixed_params,
    "Group 3: Intercept Parameters" = intercept_params,
    "Group 4: Random Effect Parameters" = random_effect_params
)

# グループ1-3は通常どおり、グループ4は大きすぎる場合は分割
plot_counter <- 0

for (group_idx in 1:4) {
    params_subset <- param_groups[[group_idx]]
    group_name <- names(param_groups)[group_idx]
    
    if (length(params_subset) == 0) {
        cat(group_name, ": No parameters found. Skipping.\n")
        next
    }
    
    # グループ4が大きい場合は複数に分割
    if (group_idx == 4 && length(params_subset) > 50) {
        # グループ4を複数に分割
        n_subgroups <- ceiling(length(params_subset) / 50)
        params_per_subgroup <- ceiling(length(params_subset) / n_subgroups)
        
        for (sub_idx in 1:n_subgroups) {
            start_idx_sub <- (sub_idx - 1) * params_per_subgroup + 1
            end_idx_sub <- min(sub_idx * params_per_subgroup, length(params_subset))
            
            params_subset_sub <- params_subset[start_idx_sub:end_idx_sub]
            
            cat("Creating density plot 4-", sub_idx, "(", group_name, " -", length(params_subset_sub), "params)...\n")
            
            plot_data_list <- list()
            for (param in params_subset_sub) {
                if (param %in% colnames(ph_draws) && param %in% colnames(control_draws)) {
                    ph_values <- ph_draws[[param]]
                    control_values <- control_draws[[param]]
                    
                    plot_data_list[[param]] <- rbind(
                        data.frame(value = ph_values, condition = "PH", parameter = param),
                        data.frame(value = control_values, condition = "Control", parameter = param)
                    )
                }
            }
            
            if (length(plot_data_list) > 0) {
                plot_data <- do.call(rbind, plot_data_list)
                
                p_group <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value, fill = condition, color = condition)) +
                    ggplot2::geom_density(alpha = 0.4) +
                    ggplot2::facet_wrap(~parameter, scales = "free", ncol = 3) +
                    ggplot2::theme_minimal() +
                    ggplot2::scale_fill_manual(values = c("PH" = "#4589ff", "Control" = "#d4bbff")) +
                    ggplot2::scale_color_manual(values = c("PH" = "#0051ba", "Control" = "#8b7ba8")) +
                    ggplot2::labs(
                        title = paste0("Posterior Distribution Comparison: ", group_name, " (", sub_idx, "/", n_subgroups, ")"),
                        x = "Parameter Value",
                        y = "Density",
                        fill = "Condition",
                        color = "Condition"
                    ) +
                    ggplot2::theme(
                        text = ggplot2::element_text(size = 9),
                        strip.text = ggplot2::element_text(size = 7),
                        legend.position = "top"
                    )
                
                # サイズを自動計算（パラメータ数に応じて、最大値を制限）
                n_params_in_group <- length(params_subset_sub)
                n_rows <- ceiling(n_params_in_group / 3)
                plot_width <- 210
                plot_height <- min(max(150, 50 * n_rows), 800)
                
                plot_path <- file.path(comparison_output_dir, paste0("04_posterior_density_group_4_part", sub_idx, ".png"))
                ggplot2::ggsave(plot_path, plot = p_group, units = "mm", width = plot_width, height = plot_height, dpi = 300)
                cat("Density plot 4-", sub_idx, "saved to:", plot_path, "(", plot_width, "x", plot_height, "mm)\n")
            }
        }
    } else {
        # グループ1-3と小さいグループ4
        cat("Creating density plot", group_idx, "(", group_name, " -", length(params_subset), "params)...\n")
        
        plot_data_list <- list()
        for (param in params_subset) {
            if (param %in% colnames(ph_draws) && param %in% colnames(control_draws)) {
                ph_values <- ph_draws[[param]]
                control_values <- control_draws[[param]]
                
                plot_data_list[[param]] <- rbind(
                    data.frame(value = ph_values, condition = "PH", parameter = param),
                    data.frame(value = control_values, condition = "Control", parameter = param)
                )
            }
        }
        
        if (length(plot_data_list) > 0) {
            plot_data <- do.call(rbind, plot_data_list)
            
            p_group <- ggplot2::ggplot(plot_data, ggplot2::aes(x = value, fill = condition, color = condition)) +
                ggplot2::geom_density(alpha = 0.4) +
                ggplot2::facet_wrap(~parameter, scales = "free", ncol = 3) +
                ggplot2::theme_minimal() +
                ggplot2::scale_fill_manual(values = c("PH" = "#4589ff", "Control" = "#d4bbff")) +
                ggplot2::scale_color_manual(values = c("PH" = "#0051ba", "Control" = "#8b7ba8")) +
                ggplot2::labs(
                    title = paste0("Posterior Distribution Comparison: ", group_name),
                    x = "Parameter Value",
                    y = "Density",
                    fill = "Condition",
                    color = "Condition"
                ) +
                ggplot2::theme(
                    text = ggplot2::element_text(size = 9),
                    strip.text = ggplot2::element_text(size = 7),
                    legend.position = "top"
                )
            
            # サイズを自動計算（パラメータ数に応じて、最大値を制限）
            n_params_in_group <- length(params_subset)
            n_rows <- ceiling(n_params_in_group / 3)
            plot_width <- 210
            plot_height <- min(max(150, 50 * n_rows), 800)
            
            plot_path <- file.path(comparison_output_dir, paste0("0", group_idx, "_posterior_density_group_", group_idx, ".png"))
            ggplot2::ggsave(plot_path, plot = p_group, units = "mm", width = plot_width, height = plot_height, dpi = 300)
            cat("Density plot", group_idx, "saved to:", plot_path, "(", plot_width, "x", plot_height, "mm)\n")
        } else {
            cat("WARNING: No parameters found in draws for", group_name, "\n")
        }
    }
}

# ==============================================================================
#  可視化：信頼区間プロット（全パラメータ）
# ==============================================================================

cat("\nCreating comprehensive confidence interval plots...\n")

# ==============================================================================
#  全パラメータの信頼区間プロット（すべて）
# ==============================================================================

# 実験ごとのランダム効果を除外（見やすくするため）
exclude_patterns <- c("r_Experiment_ID")
displayable_params <- comparison_df %>%
    dplyr::filter(!grepl(paste(exclude_patterns, collapse = "|"), Parameter))

# パラメータ数に応じて複数プロットに分割
n_params_display <- nrow(displayable_params)
n_params_per_plot <- 20
n_plots <- ceiling(n_params_display / n_params_per_plot)

for (plot_idx in 1:n_plots) {
    start_idx <- (plot_idx - 1) * n_params_per_plot + 1
    end_idx <- min(plot_idx * n_params_per_plot, n_params_display)
    
    params_subset <- displayable_params %>%
        dplyr::slice(start_idx:end_idx) %>%
        dplyr::arrange(Diff_Median)
    
    p_ci_full <- ggplot2::ggplot(
        params_subset, 
        ggplot2::aes(x = reorder(Parameter, Diff_Median), y = Diff_Median, color = Diff_Contains_Zero)
    ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dotdash", color = "black", alpha = 0.5) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dotdash", color = "grey", alpha = 0.5) +
        ggplot2::geom_linerange(
            ggplot2::aes(ymin = Diff_Lower_89, ymax = Diff_Upper_89),
            linewidth = 1,
            show.legend = FALSE
        ) +
        ggplot2::geom_point(size = 2, show.legend = TRUE) +
        ggplot2::scale_color_manual(
            values = c("FALSE" = "#0051ba", "TRUE" = "#999999"),
            labels = c("FALSE" = "Significant\n(CI excludes 0)", "TRUE" = "Not Significant"),
            name = "CI Contains Zero?"
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = paste0("Posterior Difference (PH - Control) with 89% CI [", start_idx, "-", end_idx, "/", n_params_display, "]"),
            x = "Parameter",
            y = "Difference in Effect Size (PH - Control)",
            subtitle = "Blue: Significant difference | Grey: Not significant"
        ) +
        ggplot2::theme(
            text = ggplot2::element_text(size = 10),
            axis.text.y = ggplot2::element_text(size = 9),
            legend.position = "right"
        )
    
    # サイズを自動計算（パラメータ数に応じて）
    n_params_in_plot <- nrow(params_subset)
    plot_height_ci <- max(150, 8 * n_params_in_plot)
    
    ci_full_plot_path <- file.path(comparison_output_dir, paste0("05_posterior_difference_ci_all_", plot_idx, ".png"))
    ggplot2::ggsave(ci_full_plot_path, plot = p_ci_full, units = "mm", width = 150, height = plot_height_ci, dpi = 300)
    cat("Full CI plot", plot_idx, "saved to:", ci_full_plot_path, "(150 x", plot_height_ci, "mm)\n")
}

# ==============================================================================
#  有意なパラメータのみのプロット（簡潔版）
# ==============================================================================

significant_params <- comparison_df %>%
    dplyr::filter(!Diff_Contains_Zero) %>%
    dplyr::arrange(Diff_Median)

if (nrow(significant_params) > 0) {
    
    cat("\nCreating plot for significant differences only...\n")
    
    p_ci_sig <- ggplot2::ggplot(
        significant_params, 
        ggplot2::aes(x = reorder(Parameter, Diff_Median), y = Diff_Median)
    ) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dotdash", color = "black", alpha = 0.5) +
        ggplot2::geom_linerange(
            ggplot2::aes(ymin = Diff_Lower_89, ymax = Diff_Upper_89),
            color = "#0051ba",
            linewidth = 1.2
        ) +
        ggplot2::geom_point(color = "#4589ff", size = 2.5) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::labs(
            title = paste0("Significant Posterior Differences (PH - Control) [", nrow(significant_params), " parameters]"),
            x = "Parameter",
            y = "Difference in Effect Size",
            subtitle = "89% Credible Intervals exclude zero"
        ) +
        ggplot2::theme(
            text = ggplot2::element_text(size = 11),
            axis.text.y = ggplot2::element_text(size = 10),
            plot.subtitle = ggplot2::element_text(size = 9, color = "darkred")
        )
    
    # サイズを自動計算（パラメータ数に応じて）
    n_sig_params <- nrow(significant_params)
    plot_height_sig <- max(150, 10 * n_sig_params)
    
    sig_plot_path <- file.path(comparison_output_dir, "06_posterior_difference_ci_significant.png")
    ggplot2::ggsave(sig_plot_path, plot = p_ci_sig, units = "mm", width = 150, height = plot_height_sig, dpi = 300)
    cat("Significant CI plot saved to:", sig_plot_path, "(150 x", plot_height_sig, "mm)\n")
}

# ==============================================================================
#  統計サマリー（詳細版）
# ==============================================================================

cat("\n")
cat("================================================================================\n")
cat("         POSTERIOR COMPARISON SUMMARY: PH vs Control\n")
cat("================================================================================\n")
cat("\n【DATA HANDLING】\n")
cat("✓ All MCMC chains (1-4) have been pooled for comparison\n")
cat("✓ Chain information has been removed to prevent spurious comparisons\n")
cat("✓ Single unified posterior distribution used for both models\n")
cat("✓ This approach is statistically appropriate (standard Bayesian practice)\n\n")
cat("【OVERVIEW】\n")
cat("Total parameters compared:", nrow(comparison_df), "\n")
cat("Parameters with significant difference (CI excludes 0):", 
    sum(!comparison_df$Diff_Contains_Zero), "(",
    round(100 * sum(!comparison_df$Diff_Contains_Zero) / nrow(comparison_df), 1), "%)\n\n")

# パラメータタイプ別集計
fixed_effects <- grep("^b_", comparison_df$Parameter, value = TRUE)
threshold_params <- grep("^b_Intercept", fixed_effects, value = TRUE)
other_fixed <- setdiff(fixed_effects, threshold_params)
random_effects <- grep("^sd_|^r_|^cor_", comparison_df$Parameter, value = TRUE)
intercepts <- grep("^Intercept", comparison_df$Parameter, value = TRUE)

cat("【PARAMETER BREAKDOWN】\n")
cat("- Threshold parameters (b_Intercept):", length(threshold_params), "\n")
cat("  Significant:", sum(!comparison_df[comparison_df$Parameter %in% threshold_params, "Diff_Contains_Zero"]), "\n")
cat("- Other fixed effects:", length(other_fixed), "\n")
cat("  Significant:", sum(!comparison_df[comparison_df$Parameter %in% other_fixed, "Diff_Contains_Zero"]), "\n")
cat("- Intercept (response scale):", length(intercepts), "\n")
cat("  Significant:", sum(!comparison_df[comparison_df$Parameter %in% intercepts, "Diff_Contains_Zero"]), "\n")
cat("- Random effects:", length(random_effects), "\n")
cat("  Significant:", sum(!comparison_df[comparison_df$Parameter %in% random_effects, "Diff_Contains_Zero"]), "\n\n")

# 大きな効果のパラメータ
cat("【TOP DIFFERENCES: Largest PH - Control Effects】\n")
top_diff_positive <- comparison_df %>%
    dplyr::arrange(dplyr::desc(Diff_Mean)) %>%
    dplyr::select(Parameter, Diff_Mean, Diff_SD, Diff_Lower_89, Diff_Upper_89, Diff_Contains_Zero) %>%
    head(5)

cat("Top 5 positive differences (PH > Control):\n")
print(as.data.frame(top_diff_positive))

cat("\nTop 5 negative differences (PH < Control):\n")
top_diff_negative <- comparison_df %>%
    dplyr::arrange(Diff_Mean) %>%
    dplyr::select(Parameter, Diff_Mean, Diff_SD, Diff_Lower_89, Diff_Upper_89, Diff_Contains_Zero) %>%
    head(5)
print(as.data.frame(top_diff_negative))

# 有意な効果
cat("\n【SIGNIFICANT DIFFERENCES (89% CI excludes zero)】\n")
sig_params <- comparison_df %>%
    dplyr::filter(!Diff_Contains_Zero) %>%
    dplyr::arrange(Diff_Mean)

if (nrow(sig_params) > 0) {
    cat("Count:", nrow(sig_params), "\n\n")
    print(as.data.frame(sig_params %>% 
        dplyr::select(Parameter, PH_Mean, Control_Mean, Diff_Mean, Diff_SD, Diff_Lower_89, Diff_Upper_89)))
} else {
    cat("No significant differences found.\n")
}

# 統計量の要約
cat("\n【DISTRIBUTION STATISTICS】\n")
cat("Mean difference (PH - Control):\n")
cat("  Mean:", round(mean(comparison_df$Diff_Mean), 4), "\n")
cat("  SD:", round(sd(comparison_df$Diff_Mean), 4), "\n")
cat("  Min:", round(min(comparison_df$Diff_Mean), 4), "\n")
cat("  Max:", round(max(comparison_df$Diff_Mean), 4), "\n")

cat("\n【EFFECT SIZE INTERPRETATION】\n")
cat("Differences > 0.3 in absolute value (moderate effect):\n")
moderate_effects <- comparison_df %>%
    dplyr::filter(abs(Diff_Mean) > 0.3) %>%
    dplyr::arrange(dplyr::desc(abs(Diff_Mean)))
if (nrow(moderate_effects) > 0) {
    print(as.data.frame(moderate_effects %>% 
        dplyr::select(Parameter, Diff_Mean, Diff_Lower_89, Diff_Upper_89, Diff_Contains_Zero)))
} else {
    cat("None found.\n")
}

cat("\n================================================================================\n")
cat("         Analysis complete. See PNG files for detailed visualizations.\n")
cat("================================================================================\n\n")

# 生成されたファイルのサマリー
cat("Generated files:\n")
cat("- 01_posterior_density_group_1.png: Threshold Parameters比較\n")
cat("- 02_posterior_density_group_2.png: Fixed Effect Parameters比較\n")
cat("- 03_posterior_density_group_3.png: Intercept Parameters比較\n")
cat("- 04_posterior_density_group_4.png: Random Effect Parameters比較\n")
cat("- 05_posterior_difference_ci_all_*.png: 全パラメータの信頼区間\n")
cat("- 06_posterior_difference_ci_significant.png: 有意差パラメータのみ\n")
cat("- posterior_comparison_ph_vs_control.csv: 詳細統計量（CSV形式）\n\n")
