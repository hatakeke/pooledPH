# ==============================================================================
#  04_probability_analysis.R - スコア確率分析
# ==============================================================================
#  概要: 各条件でスコアが1以上になる確率を計算・可視化
#  依存: 00_setup.R, 01_main_model.R
#  出力: 確率分析CSV, プロット
# ==============================================================================

source("00_setup.R")

cat("\n====== Probability Analysis: Score >= 1 ======\n")

# モデルの読み込み
cat("\nLoading pre-trained models...\n")
models_all <- list()  # 初期化
for (idx in seq_along(analysis_questions)) {
    current_iquest <- analysis_questions[idx]
    question_label <- question_labels[idx]
    question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", question_label))
    model_cache_path <- file.path(question_output_dir, "model_main.rds")
    
    if (file.exists(model_cache_path)) {
        cat("  Loaded:", question_label, "model from", model_cache_path, "\n")
        models_all[[question_label]] <- readRDS(model_cache_path)
    } else {
        cat("  ERROR: Model file not found:", model_cache_path, "\n")
        stop("Models must be built first. Run 01_main_model.R")
    }
}

# 出力ディレクトリ
probability_output_dir <- file.path(output_dir, "Probability_Analysis")
if (!dir.exists(probability_output_dir)) {
    dir.create(probability_output_dir, recursive = TRUE)
    cat("Created probability analysis directory:", probability_output_dir, "\n")
}

# ==============================================================================
#  関数：条件別のスコア≥1確率を計算
# ==============================================================================

#' スコア≥1の確率を各条件で計算
#' @param model brmsモデルオブジェクト（cumulative probit族）
#' @param data 元データ
#' @param model_name モデル名（例：PH, Control）
#' @return 確率をまとめたデータフレーム
calculate_score_probability <- function(model, data, model_name) {
    
    cat("\n--- Calculating P(Score >= 1) for", model_name, "---\n")
    cat("  Model family:", model$family$family, "\n")
    cat("  Using posterior_predict for posterior predictive distribution...\n")
    
    # 各条件での平均的なプロファイルを作成
    # 他の変数は平均値で固定
    conditions <- unique(data$Condition)
    
    results_list <- list()
    
    for (cond in conditions) {
        # その条件のデータを抽出
        cond_data <- data %>%
            filter(Condition == cond) %>%
            select(Condition, Position, Location, Previous_Exposure, 
                   Cognitive_Load, Duration_sec, Age, Gender_IsMale, Experiment_ID) %>%
            distinct()
        
        # 【重要】累積プロビット階層ベイズモデルの事後分布から計算
        # posterior_predict()関数は：
        # 1. モデルの事後分布から パラメータをサンプル
        # 2. 各事後サンプルに対して予測分布を計算
        # 3. 新規データに対してスコア(0-6)を予測
        # 
        # re_formula=NA により：固定効果のみを使用（母集団平均的な予測）
        # re_formula=NULL (デフォルト)なら：ランダム効果を考慮
        
        cat("    Condition:", cond, "| Data points:", nrow(cond_data), "\n")
        
        # 事後予測分布からスコアをサンプル
        # 形状：(n_posterior_samples × n_newdata)
        posterior_samples <- posterior_predict(
            model,
            newdata = cond_data,
            re_formula = NA,    # 固定効果のみで予測
            ndraws = 1000       # より多くのサンプルで精度向上
        )
        
        # 【重要】brmsのcumulative probitモデルは順序因子レベルを1から始めるため
        # 実データのスコア(0-6)に変換する必要があります
        # posterior_predictの出力は "1, 2, 3, 4, 5, 6, 7" だが
        # 実際のスコアは "0, 1, 2, 3, 4, 5, 6" なので1を引く
        posterior_samples <- posterior_samples - 1
        
        cat("    Posterior samples shape:", nrow(posterior_samples), "x", ncol(posterior_samples), "\n")
        
        # デバッグ：最初の条件のスコアサンプルを確認
        if (cond == conditions[1]) {
            cat("    DEBUG - Sample scores from posterior_predict after conversion (first 20):\n")
            print(head(posterior_samples[, 1], 20))
            cat("    Unique scores in posterior samples:", 
                paste(sort(unique(as.vector(posterior_samples))), collapse=", "), "\n")
        }
        
        # 各レコード（新規データポイント）について確率を計算
        # posterior_samples[,i] は i 番目のデータポイントの事後予測サンプル
        
        for (i in 1:nrow(cond_data)) {
            # i 番目のデータポイントについて、事後サンプルのスコア分布
            scores <- posterior_samples[, i]
            
            # スコアカテゴリごとの確率（事後予測分布から推定）
            prob_score_0 <- mean(scores == 0)
            prob_score_1 <- mean(scores == 1)
            prob_score_ge_1 <- mean(scores >= 1)
            prob_score_2_plus <- mean(scores >= 2)
            
            results_list[[paste0(cond, "_", i)]] <- data.frame(
                Condition = cond,
                Position = cond_data$Position[i],
                Location = cond_data$Location[i],
                Previous_Exposure = cond_data$Previous_Exposure[i],
                Cognitive_Load = cond_data$Cognitive_Load[i],
                Duration_sec = cond_data$Duration_sec[i],
                Age_Standardized = cond_data$Age[i],
                Gender = ifelse(cond_data$Gender_IsMale[i] == 1, "Male", "Female"),
                Experiment_ID = cond_data$Experiment_ID[i],
                P_Score_0 = prob_score_0,
                P_Score_1 = prob_score_1,
                P_Score_ge_1 = prob_score_ge_1,
                P_Score_ge_2 = prob_score_2_plus,
                stringsAsFactors = FALSE
            )
        }
    }
    
    results_df <- do.call(rbind, results_list)
    rownames(results_df) <- NULL
    
    return(results_df)
}

# ==============================================================================
#  関数：SyncからAsyncへのスコア上昇確率を計算
# ==============================================================================

#' Syncと比較してAsyncでスコアが上昇する確率を計算
calculate_increase_probability <- function(model, data, model_name) {
    cat("\n--- Calculating P(Async > Sync) for", model_name, "---\n")
    
    # 同一データに対してConditionだけをSync/Asyncに変えた予測用データを作成
    data_sync <- data %>% mutate(Condition = "Sync")
    data_async <- data %>% mutate(Condition = "Async")
    
    # 事後予測分布を取得
    pred_sync <- brms::posterior_predict(model, newdata = data_sync, re_formula = NA, ndraws = 1000)
    pred_async <- brms::posterior_predict(model, newdata = data_async, re_formula = NA, ndraws = 1000)
    
    # Async > Sync となる事後サンプルの割合を計算
    increase_probs <- colMeans(pred_async > pred_sync)
    
    results_df <- data %>%
        mutate(
            P_Increase = increase_probs,
            Model = model_name
        )
    
    return(results_df)
}

# ==============================================================================
#  各モデルの確率分析
# ==============================================================================

# データ読み込みと前処理（セットアップで定義された関数を使用）
filename <- "../../data/PooledData.xlsx"
source("00_setup.R")  # この中でdata_origが生成される

data_full <- data_orig

# Control（Q1）の分析
cat("\n\n========== CONTROL (Q1) ==========\n")
control_model <- models_all[["Control"]]
control_data <- data_full %>%
    filter(!is.na(data_full[[colnames(data_full)[questionToColumn[1]]]])) %>%
    filter(!is.na(Age))
control_data$Age <- (control_data$Age - mean(data_full$Age, na.rm = TRUE)) / sd(data_full$Age, na.rm = TRUE)

control_probs <- calculate_score_probability(control_model, control_data, "Control (Q1)")
control_increase <- calculate_increase_probability(control_model, control_data, "Control (Q1)")

# 質問固有の出力ディレクトリ
question_out_dir_ctrl <- file.path(output_dir, paste0("Q", analysis_questions[1], "_", question_labels[1]))
if (!dir.exists(question_out_dir_ctrl)) dir.create(question_out_dir_ctrl, recursive = TRUE)

# Control 条件ごとの集計
control_summary <- control_probs %>%
    group_by(Condition) %>%
    summarise(
        N = n(),
        P_Score_0_Mean = mean(P_Score_0),
        P_Score_0_SD = sd(P_Score_0),
        P_Score_1_Mean = mean(P_Score_1),
        P_Score_1_SD = sd(P_Score_1),
        P_Score_ge_1_Mean = mean(P_Score_ge_1),
        P_Score_ge_1_SD = sd(P_Score_ge_1),
        P_Score_ge_2_Mean = mean(P_Score_ge_2),
        P_Score_ge_2_SD = sd(P_Score_ge_2),
        .groups = "drop"
    )

cat("\nControl Summary (Score >= 1 Probability):\n")
print(control_summary)

cat("\nControl Increase Probability P(Async > Sync):\n")
cat("  Mean:", mean(control_increase$P_Increase), "\n")

# CSV保存（質問固有フォルダにも保存）
control_probs_path <- file.path(question_out_dir_ctrl, "Score_Probability.csv")
readr::write_csv(control_probs, control_probs_path)
cat("\nControl probabilities saved to:", control_probs_path, "\n")

control_increase_path <- file.path(question_out_dir_ctrl, "Increase_Probability.csv")
readr::write_csv(control_increase, control_increase_path)

# 全体ディレクトリにもバックアップ（既存の動作維持）
readr::write_csv(control_probs, file.path(probability_output_dir, "Control_Q1_Score_Probability.csv"))
readr::write_csv(control_increase, file.path(probability_output_dir, "Control_Q1_Increase_Probability.csv"))

control_summary_path <- file.path(question_out_dir_ctrl, "Score_Probability_Summary.csv")
readr::write_csv(control_summary, control_summary_path)
cat("Control summary saved to:", control_summary_path, "\n")

# PH（Q7）の分析
cat("\n\n========== PH (Q7) ==========\n")
ph_model <- models_all[["PH"]]
ph_data <- data_full %>%
    filter(!is.na(data_full[[colnames(data_full)[questionToColumn[7]]]]))%>%
    filter(!is.na(Age))
ph_data$Age <- (ph_data$Age - mean(data_full$Age, na.rm = TRUE)) / sd(data_full$Age, na.rm = TRUE)

ph_probs <- calculate_score_probability(ph_model, ph_data, "PH (Q7)")
ph_increase <- calculate_increase_probability(ph_model, ph_data, "PH (Q7)")

# 質問固有の出力ディレクトリ
question_out_dir_ph <- file.path(output_dir, paste0("Q", analysis_questions[2], "_", question_labels[2]))
if (!dir.exists(question_out_dir_ph)) dir.create(question_out_dir_ph, recursive = TRUE)

# PH 条件ごとの集計
ph_summary <- ph_probs %>%
    group_by(Condition) %>%
    summarise(
        N = n(),
        P_Score_0_Mean = mean(P_Score_0),
        P_Score_0_SD = sd(P_Score_0),
        P_Score_1_Mean = mean(P_Score_1),
        P_Score_1_SD = sd(P_Score_1),
        P_Score_ge_1_Mean = mean(P_Score_ge_1),
        P_Score_ge_1_SD = sd(P_Score_ge_1),
        P_Score_ge_2_Mean = mean(P_Score_ge_2),
        P_Score_ge_2_SD = sd(P_Score_ge_2),
        .groups = "drop"
    )

cat("\nPH Summary (Score >= 1 Probability):\n")
print(ph_summary)

cat("\nPH Increase Probability P(Async > Sync):\n")
cat("  Mean:", mean(ph_increase$P_Increase), "\n")

# CSV保存（質問固有フォルダにも保存）
ph_probs_path <- file.path(question_out_dir_ph, "Score_Probability.csv")
readr::write_csv(ph_probs, ph_probs_path)
cat("\nPH probabilities saved to:", ph_probs_path, "\n")

ph_increase_path <- file.path(question_out_dir_ph, "Increase_Probability.csv")
readr::write_csv(ph_increase, ph_increase_path)

# 全体ディレクトリにもバックアップ（既存の動作維持）
readr::write_csv(ph_probs, file.path(probability_output_dir, "PH_Q7_Score_Probability.csv"))
readr::write_csv(ph_increase, file.path(probability_output_dir, "PH_Q7_Increase_Probability.csv"))

ph_summary_path <- file.path(question_out_dir_ph, "Score_Probability_Summary.csv")
readr::write_csv(ph_summary, ph_summary_path)
cat("PH summary saved to:", ph_summary_path, "\n")

# ==============================================================================
#  比較サマリー
# ==============================================================================

cat("\n\n================================================================================\n")
cat("                    SCORE >= 1 PROBABILITY COMPARISON\n")
cat("================================================================================\n\n")

cat("【計算方法の詳細】\n")
cat("✓ 累積プロビット階層ベイズモデルの事後分布から計算\n")
cat("✓ 事後予測分布(posterior predictive distribution)を使用\n")
cat("✓ 各条件について1000個の事後サンプルから確率を推定\n")
cat("✓ re_formula=NA により、固定効果のみに基づいた予測（母集団平均）\n\n")

# Control vs PH での確率比較
cat("【CONTROL (Q1: General unease)】\n")
cat("  Sync condition   : P(Score >= 1) = ", 
    round(control_summary$P_Score_ge_1_Mean[control_summary$Condition == "Sync"], 4), 
    " ± ", 
    round(control_summary$P_Score_ge_1_SD[control_summary$Condition == "Sync"], 4),
    "\n", sep = "")
cat("  Async condition  : P(Score >= 1) = ", 
    round(control_summary$P_Score_ge_1_Mean[control_summary$Condition == "Async"], 4),
    " ± ",
    round(control_summary$P_Score_ge_1_SD[control_summary$Condition == "Async"], 4),
    "\n\n", sep = "")

cat("【PH (Q7: Presence hallucination)】\n")
cat("  Sync condition   : P(Score >= 1) = ", 
    round(ph_summary$P_Score_ge_1_Mean[ph_summary$Condition == "Sync"], 4),
    " ± ",
    round(ph_summary$P_Score_ge_1_SD[ph_summary$Condition == "Sync"], 4),
    "\n", sep = "")
cat("  Async condition  : P(Score >= 1) = ", 
    round(ph_summary$P_Score_ge_1_Mean[ph_summary$Condition == "Async"], 4),
    " ± ",
    round(ph_summary$P_Score_ge_1_SD[ph_summary$Condition == "Async"], 4),
    "\n\n", sep = "")

# 差分
control_diff <- control_summary %>%
    pivot_wider(names_from = Condition, values_from = P_Score_ge_1_Mean) %>%
    mutate(Difference = Async - Sync) %>%
    pull(Difference)

ph_diff <- ph_summary %>%
    pivot_wider(names_from = Condition, values_from = P_Score_ge_1_Mean) %>%
    mutate(Difference = Async - Sync) %>%
    pull(Difference)

cat("【DIFFERENCES (Async - Sync)】\n")
cat("  Control (Q1)     : Δ = ", round(control_diff, 4), "\n", sep = "")
cat("  PH (Q7)          : Δ = ", round(ph_diff, 4), "\n\n", sep = "")

cat("【INCREASE PROBABILITY (P(Async > Sync))】\n")
cat("  Control (Q1)     : P = ", round(mean(control_increase$P_Increase), 4), "\n", sep = "")
cat("  PH (Q7)          : P = ", round(mean(ph_increase$P_Increase), 4), "\n\n", sep = "")

cat("================================================================================\n\n")

# ==============================================================================
#  可視化
# ==============================================================================

cat("Creating visualization plots...\n")

# Control と PH の確率比較プロット
p_comparison <- rbind(
    control_summary %>% mutate(Question = "Q1: General Unease (Control)"),
    ph_summary %>% mutate(Question = "Q7: Presence Hallucination (PH)")
) %>%
    ggplot2::ggplot(ggplot2::aes(x = Condition, y = P_Score_ge_1_Mean, fill = Condition)) +
    ggplot2::facet_wrap(~Question) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.7, color = "black") +
    ggplot2::geom_errorbar(
        ggplot2::aes(ymin = P_Score_ge_1_Mean - P_Score_ge_1_SD,
                     ymax = P_Score_ge_1_Mean + P_Score_ge_1_SD),
        width = 0.2, color = "black"
    ) +
    ggplot2::scale_fill_manual(values = c("Sync" = "#4ECDC4", "Async" = "#FF6B6B")) +
    ggplot2::labs(
        title = "Probability of Score >= 1 by Condition",
        x = "Condition",
        y = "Probability",
        fill = "Condition"
    ) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
        text = ggplot2::element_text(size = 12),
        strip.text = ggplot2::element_text(size = 11, face = "bold")
    )

comp_plot_path <- file.path(probability_output_dir, "Score_Probability_Comparison.png")
ggplot2::ggsave(comp_plot_path, plot = p_comparison, units = "mm", width = 210, height = 120, dpi = 300)
cat("Comparison plot saved to:", comp_plot_path, "\n")

# 詳細プロット（各スコアカテゴリの確率）
plot_data_control <- control_summary %>%
    select(Condition, starts_with("P_")) %>%
    pivot_longer(cols = starts_with("P_"), names_to = "Category", values_to = "Probability") %>%
    filter(grepl("_Mean$", Category)) %>%
    mutate(Category = gsub("_Mean$", "", Category)) %>%
    mutate(Question = "Control (Q1)")

plot_data_ph <- ph_summary %>%
    select(Condition, starts_with("P_")) %>%
    pivot_longer(cols = starts_with("P_"), names_to = "Category", values_to = "Probability") %>%
    filter(grepl("_Mean$", Category)) %>%
    mutate(Category = gsub("_Mean$", "", Category)) %>%
    mutate(Question = "PH (Q7)")

plot_data_all <- rbind(plot_data_control, plot_data_ph)

p_detail <- ggplot2::ggplot(
    plot_data_all,
    ggplot2::aes(x = Category, y = Probability, fill = Condition)
) +
    ggplot2::facet_wrap(~Question) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7, color = "black") +
    ggplot2::scale_fill_manual(values = c("Sync" = "#4ECDC4", "Async" = "#FF6B6B")) +
    ggplot2::labs(
        title = "Score Category Probabilities by Condition",
        x = "Score Category",
        y = "Probability",
        fill = "Condition"
    ) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
        text = ggplot2::element_text(size = 11),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        strip.text = ggplot2::element_text(size = 11, face = "bold")
    )

detail_plot_path <- file.path(probability_output_dir, "Score_Probability_Detail.png")
ggplot2::ggsave(detail_plot_path, plot = p_detail, units = "mm", width = 210, height = 140, dpi = 300)
cat("Detail plot saved to:", detail_plot_path, "\n")

# 上昇確率のプロット
plot_data_increase <- rbind(
    control_increase %>% mutate(Question = "Q1: Control"),
    ph_increase %>% mutate(Question = "Q7: PH")
)

p_increase <- ggplot2::ggplot(plot_data_increase, ggplot2::aes(x = Question, y = P_Increase, fill = Question)) +
    ggplot2::geom_boxplot(alpha = 0.5, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.2, alpha = 0.3) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    ggplot2::labs(
        title = "Probability of Score Increase (Async > Sync)",
        subtitle = "Points show variability across covariate profiles",
        x = "Question",
        y = "P(Async > Sync)"
    ) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal()

inc_plot_path <- file.path(probability_output_dir, "Score_Increase_Probability.png")
ggplot2::ggsave(inc_plot_path, plot = p_increase, units = "mm", width = 160, height = 120, dpi = 300)
cat("Increase probability plot saved to:", inc_plot_path, "\n")

cat("\n✓ Probability analysis complete!\n")
