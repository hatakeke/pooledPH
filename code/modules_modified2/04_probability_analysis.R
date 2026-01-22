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
    
    # モデルに含まれる変数を特定
    # brmsformula オブジェクトから標準の formula を抽出して変数取得
    main_formula <- formula(model)$formula
    model_terms <- all.vars(main_formula)
    
    # 目的変数（左辺）を特定
    resp_var <- as.character(main_formula)[2]
    
    # 予測変数（目的変数以外）を抽出
    predictor_vars <- model_terms[model_terms != resp_var]
    
    # ランダム効果のグループ化変数（Experiment_ID等）も追加しておく
    predictor_vars <- unique(c(predictor_vars, "Condition", "Device", "Experiment_ID", "Subject_No"))
    
    results_list <- list()
    
    # 既存のデータから一意な条件組み合わせを抽出
    cond_data <- data %>%
        select(any_of(predictor_vars)) %>%
        distinct() %>%
        na.omit()
    
    cat("    Variables used:", paste(predictor_vars[predictor_vars %in% colnames(cond_data)], collapse=", "), "\n")
    cat("    Total unique condition patterns across predictors:", nrow(cond_data), "\n")
    
    # 事後予測分布からスコアをサンプル
    posterior_samples <- posterior_predict(
        model,
        newdata = cond_data,
        re_formula = NA,    # 固定効果のみで予測
        ndraws = 1000
    )
    
    # スコア変換 (1-7 -> 0-6)
    posterior_samples <- posterior_samples - 1
    
    for (i in 1:nrow(cond_data)) {
        # i 番目のデータパターンについて、事後サンプルのスコア分布
        scores <- posterior_samples[, i]
        
        # スコアカテゴリごとの確率
        prob_score_0 <- mean(scores == 0)
        prob_score_1 <- mean(scores == 1)
        prob_score_ge_1 <- mean(scores >= 1)
        prob_score_ge_2 <- mean(scores >= 2)
        
        # 動的に結果を作成（存在する列を優先）
        res_row <- data.frame(
            Condition = cond_data$Condition[i],
            Device = if("Device" %in% names(cond_data)) cond_data$Device[i] else "Default",
            stringsAsFactors = FALSE
        )
        
        # 他の変数も付与（デバッグ・詳細分析用）
        for(v in setdiff(predictor_vars, c("Condition", "Device"))) {
            if(v %in% names(cond_data)) res_row[[v]] <- cond_data[[v]][i]
        }
        
        res_row$P_Score_0 <- prob_score_0
        res_row$P_Score_1 <- prob_score_1
        res_row$P_Score_ge_1 <- prob_score_ge_1
        res_row$P_Score_ge_2 <- prob_score_ge_2
        
        results_list[[i]] <- res_row
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
    group_by(Condition, Device) %>%
    summarise(
        N = n(),
        P_Score_0_Mean = mean(P_Score_0),
        P_Score_0_SD = sd(P_Score_0),
        P_Score_ge_1_Mean = mean(P_Score_ge_1),
        P_Score_ge_1_SD = sd(P_Score_ge_1),
        .groups = "drop"
    )

cat("\nControl Summary (Score >= 1 Probability by Device):\n")
print(control_summary)

# Deviceごとの効果（Defaultとの差分）を計算
control_device_effect <- control_summary %>%
    group_by(Condition) %>%
    mutate(
        Diff_vs_Default = P_Score_ge_1_Mean - P_Score_ge_1_Mean[Device == "Default"]
    ) %>%
    ungroup()

cat("\nControl Device Effect (Difference in P(Score>=1) vs Default):\n")
print(control_device_effect %>% select(Condition, Device, Diff_vs_Default))

cat("\nControl Increase Probability P(Async > Sync):\n")
cat("  Mean:", mean(control_increase$P_Increase), "\n")

# CSV保存（質問固有フォルダにも保存）
control_probs_path <- file.path(question_out_dir_ctrl, "Score_Probability.csv")
readr::write_csv(control_probs, control_probs_path)
cat("\nControl probabilities saved to:", control_probs_path, "\n")

control_increase_path <- file.path(question_out_dir_ctrl, "Increase_Probability.csv")
readr::write_csv(control_increase, control_increase_path)

# Device効果の保存
control_device_effect_path <- file.path(question_out_dir_ctrl, "Device_Effect_Probability.csv")
readr::write_csv(control_device_effect, control_device_effect_path)

# 全体ディレクトリにもバックアップ（既存の動作維持）
readr::write_csv(control_probs, file.path(probability_output_dir, "Control_Q1_Score_Probability.csv"))
readr::write_csv(control_increase, file.path(probability_output_dir, "Control_Q1_Increase_Probability.csv"))
readr::write_csv(control_device_effect, file.path(probability_output_dir, "Control_Q1_Device_Effect.csv"))

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
    group_by(Condition, Device) %>%
    summarise(
        N = n(),
        P_Score_0_Mean = mean(P_Score_0),
        P_Score_0_SD = sd(P_Score_0),
        P_Score_ge_1_Mean = mean(P_Score_ge_1),
        P_Score_ge_1_SD = sd(P_Score_ge_1),
        .groups = "drop"
    )

cat("\nPH Summary (Score >= 1 Probability by Device):\n")
print(ph_summary)

# Deviceごとの効果（Defaultとの差分）を計算
ph_device_effect <- ph_summary %>%
    group_by(Condition) %>%
    mutate(
        Diff_vs_Default = P_Score_ge_1_Mean - P_Score_ge_1_Mean[Device == "Default"]
    ) %>%
    ungroup()

cat("\nPH Device Effect (Difference in P(Score>=1) vs Default):\n")
print(ph_device_effect %>% select(Condition, Device, Diff_vs_Default))

cat("\nPH Increase Probability P(Async > Sync):\n")
cat("  Mean:", mean(ph_increase$P_Increase), "\n")

# CSV保存（質問固有フォルダにも保存）
ph_probs_path <- file.path(question_out_dir_ph, "Score_Probability.csv")
readr::write_csv(ph_probs, ph_probs_path)
cat("\nPH probabilities saved to:", ph_probs_path, "\n")

ph_increase_path <- file.path(question_out_dir_ph, "Increase_Probability.csv")
readr::write_csv(ph_increase, ph_increase_path)

# Device効果の保存
ph_device_effect_path <- file.path(question_out_dir_ph, "Device_Effect_Probability.csv")
readr::write_csv(ph_device_effect, ph_device_effect_path)

# 全体ディレクトリにもバックアップ（既存의 動作維持）
readr::write_csv(ph_probs, file.path(probability_output_dir, "PH_Q7_Score_Probability.csv"))
readr::write_csv(ph_increase, file.path(probability_output_dir, "PH_Q7_Increase_Probability.csv"))
readr::write_csv(ph_device_effect, file.path(probability_output_dir, "PH_Q7_Device_Effect.csv"))

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
# (注：Device ごとに集計されているため、代表として Default デバイスの Sync/Async を表示)
cat("【CONTROL (Q1: General unease) - Default Device】\n")
control_default <- control_summary %>% filter(Device == "Default")
cat("  Sync condition   : P(Score >= 1) = ", 
    round(control_default$P_Score_ge_1_Mean[control_default$Condition == "Sync"], 4), 
    " ± ", 
    round(control_default$P_Score_ge_1_SD[control_default$Condition == "Sync"], 4),
    "\n", sep = "")
cat("  Async condition  : P(Score >= 1) = ", 
    round(control_default$P_Score_ge_1_Mean[control_default$Condition == "Async"], 4),
    " ± ",
    round(control_default$P_Score_ge_1_SD[control_default$Condition == "Async"], 4),
    "\n\n", sep = "")

cat("【PH (Q7: Presence hallucination) - Default Device】\n")
ph_default <- ph_summary %>% filter(Device == "Default")
cat("  Sync condition   : P(Score >= 1) = ", 
    round(ph_default$P_Score_ge_1_Mean[ph_default$Condition == "Sync"], 4),
    " ± ",
    round(ph_default$P_Score_ge_1_SD[ph_default$Condition == "Sync"], 4),
    "\n", sep = "")
cat("  Async condition  : P(Score >= 1) = ", 
    round(ph_default$P_Score_ge_1_Mean[ph_default$Condition == "Async"], 4),
    " ± ",
    round(ph_default$P_Score_ge_1_SD[ph_default$Condition == "Async"], 4),
    "\n\n", sep = "")

# 差分 (Default基準)
control_diff <- control_default %>%
    pivot_wider(names_from = Condition, values_from = P_Score_ge_1_Mean) %>%
    mutate(Difference = Async - Sync) %>%
    pull(Difference)

ph_diff <- ph_default %>%
    pivot_wider(names_from = Condition, values_from = P_Score_ge_1_Mean) %>%
    mutate(Difference = Async - Sync) %>%
    pull(Difference)

cat("【DIFFERENCES (Async - Sync) - Default Device】\n")
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

# Control と PH の確率比較プロット (Device を facet または color に追加)
p_comparison <- rbind(
    control_summary %>% mutate(Question = "Q1: General Unease (Control)"),
    ph_summary %>% mutate(Question = "Q7: Presence Hallucination (PH)")
) %>%
    ggplot2::ggplot(ggplot2::aes(x = Device, y = P_Score_ge_1_Mean, fill = Condition)) +
    ggplot2::facet_wrap(~Question) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7, color = "black") +
    ggplot2::geom_errorbar(
        ggplot2::aes(ymin = P_Score_ge_1_Mean - P_Score_ge_1_SD,
                     ymax = P_Score_ge_1_Mean + P_Score_ge_1_SD),
        position = ggplot2::position_dodge(0.9), width = 0.2, color = "black"
    ) +
    ggplot2::scale_fill_manual(values = c("Sync" = "#4ECDC4", "Async" = "#FF6B6B")) +
    ggplot2::labs(
        title = "Probability of Score >= 1 by Device and Condition",
        x = "Device",
        y = "Probability",
        fill = "Condition"
    ) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
        text = ggplot2::element_text(size = 12),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        strip.text = ggplot2::element_text(size = 11, face = "bold")
    )

comp_plot_path <- file.path(probability_output_dir, "Score_Probability_Comparison_by_Device.png")
ggplot2::ggsave(comp_plot_path, plot = p_comparison, units = "mm", width = 210, height = 120, dpi = 300)
cat("Comparison plot saved to:", comp_plot_path, "\n")

# 詳細プロット（各スコアカテゴリの確率）
plot_data_control <- control_summary %>%
    select(Condition, Device, starts_with("P_")) %>%
    pivot_longer(cols = starts_with("P_"), names_to = "Metric", values_to = "Value") %>%
    filter(grepl("_Mean$|_SD$", Metric)) %>%
    mutate(
        Category = gsub("_Mean$|_SD$", "", Metric),
        Type = ifelse(grepl("_Mean$", Metric), "Probability", "SD")
    ) %>%
    tidyr::pivot_wider(names_from = Type, values_from = Value) %>%
    mutate(Question = "Control (Q1)")

plot_data_ph <- ph_summary %>%
    select(Condition, Device, starts_with("P_")) %>%
    pivot_longer(cols = starts_with("P_"), names_to = "Metric", values_to = "Value") %>%
    filter(grepl("_Mean$|_SD$", Metric)) %>%
    mutate(
        Category = gsub("_Mean$|_SD$", "", Metric),
        Type = ifelse(grepl("_Mean$", Metric), "Probability", "SD")
    ) %>%
    tidyr::pivot_wider(names_from = Type, values_from = Value) %>%
    mutate(Question = "PH (Q7)")

plot_data_all <- rbind(plot_data_control, plot_data_ph)

p_detail <- ggplot2::ggplot(
    plot_data_all,
    ggplot2::aes(x = Device, y = Probability, fill = Condition)
) +
    ggplot2::facet_grid(Question ~ Category) +
    ggplot2::geom_bar(stat = "identity", position = "dodge", alpha = 0.7, color = "black") +
    ggplot2::geom_errorbar(
        ggplot2::aes(ymin = Probability - SD, ymax = Probability + SD),
        position = ggplot2::position_dodge(0.9), width = 0.2, color = "black"
    ) +
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

p_increase <- ggplot2::ggplot(plot_data_increase, ggplot2::aes(x = Device, y = P_Increase, fill = Device)) +
    ggplot2::facet_wrap(~Question) +
    ggplot2::geom_boxplot(alpha = 0.5, outlier.shape = NA) +
    ggplot2::geom_jitter(width = 0.2, alpha = 0.1) +
    ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
    ggplot2::labs(
        title = "Probability of Score Increase (Async > Sync) by Device",
        subtitle = "Points show variability across subjects",
        x = "Device",
        y = "P(Async > Sync)"
    ) +
    ggplot2::ylim(0, 1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )

inc_plot_path <- file.path(probability_output_dir, "Score_Increase_Probability.png")
ggplot2::ggsave(inc_plot_path, plot = p_increase, units = "mm", width = 160, height = 120, dpi = 300)
cat("Increase probability plot saved to:", inc_plot_path, "\n")

cat("\n✓ Probability analysis complete!\n")
