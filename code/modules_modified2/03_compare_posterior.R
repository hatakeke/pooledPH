# ==============================================================================
#  03_compare_posterior.R - 事後分布の比較分析（PH vs Control）
# ==============================================================================
#  概要: PHとControlの事後分布をチェーン区別なくプールして比較し、グラフとCSVで出力
#  特徴: 全てのカテゴリを1枚の画像に集約（分割なし）。チェーン接頭辞問題解決済。
#  依存: 00_setup.R, 01_main_model.R
# ==============================================================================

source("00_setup.R")
library(ggplot2)
library(dplyr)
library(stringr)
library(brms)

cat("\n====== Comparing posteriors: PH vs Control (Single Plot per Category) ======\n")

# 出力ディレクトリ設定
comparison_output_dir <- file.path(output_dir, "Comparison_PH_vs_Control")
if (!dir.exists(comparison_output_dir)) {
    dir.create(comparison_output_dir, recursive = TRUE)
}

# ==============================================================================
#  関数定義：データ抽出とクリーニング
# ==============================================================================

#' モデルから事後分布を抽出し、チェーンを完全に統合したデータフレームを返す
get_flat_draws <- function(model) {
    # as_draws_matrix を使用してチェーン情報を平坦化（Xn_接頭辞防止）
    draws_mat <- as_draws_matrix(model)
    draws_df <- as.data.frame(draws_mat)
    
    # 不要なメタデータ列を除外
    cols_to_remove <- c("lprior", "lp__", ".chain", ".iteration", ".draw")
    draws_df <- draws_df %>% select(-any_of(cols_to_remove))
    
    return(draws_df)
}

# ==============================================================================
#  メイン処理：データの準備
# ==============================================================================

cat("Extracting posterior samples (flattening chains)...\n")

draws_ph <- get_flat_draws(models_all[["PH"]])
draws_ctrl <- get_flat_draws(models_all[["Control"]])

# 共通パラメータの特定
common_params <- intersect(colnames(draws_ph), colnames(draws_ctrl))

if (length(common_params) == 0) {
    stop("No common parameters found between PH and Control models.")
}

cat("  Common parameters found:", length(common_params), "\n")

# ==============================================================================
#  統計量の計算とCSV作成
# ==============================================================================

cat("Calculating differences and statistics...\n")

stats_list <- list()

for (param in common_params) {
    val_ph <- draws_ph[[param]]
    val_ctrl <- draws_ctrl[[param]]
    
    # サンプル数調整
    n_sample <- min(length(val_ph), length(val_ctrl))
    val_ph_s <- sample(val_ph, n_sample)
    val_ctrl_s <- sample(val_ctrl, n_sample)
    
    # 差分の分布 (PH - Control)
    diff_vals <- val_ph_s - val_ctrl_s
    
    # 89%確信区間 (CI)
    ci_diff <- quantile(diff_vals, probs = c(0.055, 0.945))
    
    # 0を含むか判定
    contains_zero <- (ci_diff[1] < 0) & (ci_diff[2] > 0)
    
    stats_list[[param]] <- data.frame(
        Parameter = param,
        PH_Mean = mean(val_ph_s),
        Control_Mean = mean(val_ctrl_s),
        Diff_Mean = mean(diff_vals),
        Diff_Median = median(diff_vals),
        Diff_Lower_89 = ci_diff[1],
        Diff_Upper_89 = ci_diff[2],
        Diff_Contains_Zero = contains_zero,
        row.names = NULL
    )
}

comparison_df <- do.call(rbind, stats_list)

# CSV保存
csv_path <- file.path(comparison_output_dir, "posterior_comparison_ph_vs_control.csv")
readr::write_csv(comparison_df, csv_path)
cat("Saved CSV:", csv_path, "\n")

# ==============================================================================
#  可視化関数定義
# ==============================================================================

# パラメータのカテゴライズ
classify_parameter <- function(param_name) {
    if (str_detect(param_name, "^b_Intercept")) return("Threshold")
    if (str_detect(param_name, "^Intercept")) return("Intercept_Raw") 
    if (str_detect(param_name, "^b_")) return("Fixed_Effect")
    # ハイパーパラメータ（sd や cor：集団の分布を定義するパラメータ）
    if (str_detect(param_name, "^sd_|^cor_")) return("Hyper_Parameters")
    # 個別の偏差（r_）：Experiment_ID は残し、Subject_No は除外する
    if (str_detect(param_name, "^r_")) {
        if (str_detect(param_name, "Subject_No")) return("Excluded_Subject")
        return("Experiment_Offsets")
    }
    return("Other")
}

# 描画用データの作成
make_plot_data <- function(params_subset) {
    plot_data_list <- list()
    for (p in params_subset) {
        d_p <- rbind(
            data.frame(Value = draws_ph[[p]], Group = "PH", Parameter = p),
            data.frame(Value = draws_ctrl[[p]], Group = "Control", Parameter = p)
        )
        plot_data_list[[p]] <- d_p
    }
    do.call(rbind, plot_data_list)
}

# 密度プロット作成・保存関数（分割なし・1枚に集約）
save_density_plot <- function(params, title_suffix, filename) {
    if (length(params) == 0) return(NULL)
    
    cat("  Drawing:", title_suffix, " (", length(params), " parameters)...\n")
    
    plot_data <- make_plot_data(params)
    
    # レイアウト計算
    n_cols <- 3  # 3列固定
    n_rows <- ceiling(length(params) / n_cols)
    
    # 高さの自動計算: 1行あたり50mm、最低150mm
    # パラメータ数が多くても画像が縦に伸びて収容します
    plot_height <- max(150, n_rows * 50)
    
    p <- ggplot(plot_data, aes(x = Value, fill = Group, color = Group)) +
        geom_density(alpha = 0.4) +
        facet_wrap(~Parameter, scales = "free", ncol = n_cols) +
        scale_fill_manual(values = c("PH" = "#4589ff", "Control" = "#d4bbff")) +
        scale_color_manual(values = c("PH" = "#0051ba", "Control" = "#8b7ba8")) +
        theme_minimal() +
        labs(title = paste("Posterior Density:", title_suffix),
             y = "Density", x = "Value") +
        theme(legend.position = "top")
    
    # limitsize = FALSE で巨大な画像でも保存可能にする
    ggsave(file.path(comparison_output_dir, filename), p, 
           width = 210, height = plot_height, units = "mm", dpi = 300, 
           limitsize = FALSE)
}

# ==============================================================================
#  可視化実行：密度プロット（カテゴリ別）
# ==============================================================================

cat("Generating density plots...\n")

all_params <- comparison_df$Parameter
param_types <- sapply(all_params, classify_parameter)

# 各カテゴリを一括描画
save_density_plot(all_params[param_types == "Threshold"], 
                 "Threshold Parameters", "01_Density_Threshold_Parameters.png")

save_density_plot(all_params[param_types == "Fixed_Effect"], 
                 "Fixed Effect Parameters", "02_Density_Fixed_Effect_Parameters.png")

save_density_plot(all_params[param_types == "Intercept_Raw"], 
                 "Intercept Parameters", "03_Density_Intercept_Parameters.png")

# ランダム効果の集団統計量（SDと相関：Student-t分布の実体を定義するもの）のみを描画
save_density_plot(all_params[param_types == "Hyper_Parameters"], 
                 "Group-level SD and Correlation", "04_Density_Hyper_Parameters.png")

# 実験ごとのランダム効果偏差 (Experiment_ID)
save_density_plot(all_params[param_types == "Experiment_Offsets"], 
                 "Experiment-level Random Effects", "05_Density_Experiment_Offsets.png")

# 個別の Subject_No ごとの偏差は数多すぎるため描画対象から除外
# save_density_plot(all_params[param_types == "Excluded_Subject"], ...)

# ==============================================================================
#  可視化実行：差分の確信区間プロット
# ==============================================================================

cat("Generating CI difference plots...\n")

# 5. 全パラメータ（r_Subject_No 除く）の一括プロット
main_params <- comparison_df %>% 
    filter(!str_detect(Parameter, "Subject_No")) %>%
    arrange(Diff_Median)

if (nrow(main_params) > 0) {
    cat("  Drawing: All Parameters Difference...\n")
    
    p_ci <- ggplot(main_params, aes(x = reorder(Parameter, Diff_Median), y = Diff_Median, color = Diff_Contains_Zero)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_pointrange(aes(ymin = Diff_Lower_89, ymax = Diff_Upper_89)) +
        coord_flip() +
        scale_color_manual(values = c("FALSE" = "#0051ba", "TRUE" = "gray70"), 
                           labels = c("Significant", "Not Sig")) +
        theme_minimal() +
        labs(title = "Difference (PH - Control) [All Parameters]",
             subtitle = "89% CI excludes zero for blue items",
             x = "Parameter", y = "Difference")
    
    # 高さをパラメータ数に応じて調整（1項目あたり6mm + 余白）
    h_ci <- max(150, 20 + nrow(main_params) * 6)
    
    ggsave(file.path(comparison_output_dir, "06_CI_Difference_All.png"), 
           p_ci, width = 180, height = h_ci, units = "mm", dpi = 300, 
           limitsize = FALSE)
}

# 6. 有意差パラメータのみ
sig_df <- comparison_df %>% 
    filter(Diff_Contains_Zero == FALSE) %>%
    arrange(Diff_Median)

if (nrow(sig_df) > 0) {
    cat("  Drawing: Significant Differences...\n")
    
    p_sig <- ggplot(sig_df, aes(x = reorder(Parameter, Diff_Median), y = Diff_Median)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_pointrange(aes(ymin = Diff_Lower_89, ymax = Diff_Upper_89), color = "#d14141") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Significant Differences Only (PH - Control)",
             subtitle = "Parameters where 89% CI does not include 0",
             x = "Parameter", y = "Difference")
    
    h_sig <- max(100, 10 + nrow(sig_df)*6)
    
    ggsave(file.path(comparison_output_dir, "07_CI_Difference_Significant.png"), 
           p_sig, width = 180, height = h_sig, units = "mm", dpi = 300, 
           limitsize = FALSE)
} else {
    cat("  (No significant differences found. Skipping 07.)\n")
}

# ==============================================================================
#  終了サマリー
# ==============================================================================

cat("\n================================================================\n")
cat("Analysis Complete. Files are saved in:", comparison_output_dir, "\n")
cat("All plots have been consolidated into single files per category.\n")
cat("================================================================\n")