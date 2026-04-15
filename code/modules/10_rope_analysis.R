# ==============================================================================
#  10_rope_analysis.R - ROPE分析
# ==============================================================================
#  概要: HDI-ROPE（Region of Practical Equivalence）比較分析
#  依存: 00_setup.R, 01_main_model.R, 05_additional_models.R
#  出力: ROPE_analysis.csv, ROPE_summary.csv（質問ごとのディレクトリに保存）
# ==============================================================================

library(bayestestR)
library(dplyr)
library(tibble)
library(posterior)

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

#' 効果が「変わったとはいえない」最大変化量を事後分布から評価
#' @param model brmsモデルオブジェクト
#' @param rope_range ROPE範囲（実用的差の閾値）
#' @param ci 使用する信用質量（例: 0.89）
#' @param raw_sd_map 生尺度換算用SDの名前付きベクトル
#' @return 効果不変幅サマリー
compute_effect_invariance_ranges <- function(
    model,
    rope_range = c(-0.1, 0.1),
    ci = 0.89,
    raw_sd_map = c(Age = NA_real_, Duration_sec = NA_real_)
    ) {

    draws_df <- as_draws_df(model) %>% as_tibble()
    beta_cols <- colnames(draws_df)[grepl("^b_", colnames(draws_df))]

    rope_half <- min(abs(rope_range))

    if (length(beta_cols) == 0) {
        return(tibble())
    }

    infer_base_variable <- function(param_name) {
        term <- sub("^b_", "", param_name)
        tokens <- strsplit(term, ":", fixed = TRUE)[[1]]
        if ("Duration_sec" %in% tokens) return("Duration_sec")
        if ("Age" %in% tokens) return("Age")
        return(NA_character_)
    }

    out <- lapply(beta_cols, function(param) {
        vals <- draws_df[[param]]
        vals <- vals[is.finite(vals)]

        if (length(vals) == 0) {
            return(tibble(
                Parameter = param,
                Base_Variable = NA_character_,
                ROPE_Lower = rope_range[1],
                ROPE_Upper = rope_range[2],
                Credible_Mass = ci,
                AbsBeta_Quantile_for_CI = NA_real_,
                Max_Invariant_Change_PredictorScale = NA_real_,
                Raw_SD = NA_real_,
                Max_Invariant_Change_RawUnit = NA_real_,
                Unit_Label = NA_character_,
                Interpretation = "No finite posterior draws"
            ))
        }

        abs_beta <- abs(vals)
        abs_beta_q <- as.numeric(quantile(abs_beta, probs = ci, na.rm = TRUE, type = 8))

        if (is.na(abs_beta_q) || abs_beta_q <= 0) {
            max_delta_pred <- Inf
        } else {
            max_delta_pred <- rope_half / abs_beta_q
        }

        base_var <- infer_base_variable(param)
        raw_sd <- if (!is.na(base_var) && base_var %in% names(raw_sd_map)) raw_sd_map[[base_var]] else NA_real_
        max_delta_raw <- if (!is.na(raw_sd) && is.finite(max_delta_pred)) max_delta_pred * raw_sd else NA_real_

        unit_label <- dplyr::case_when(
            base_var == "Age" ~ "years",
            base_var == "Duration_sec" ~ "seconds",
            TRUE ~ NA_character_
        )

        interp <- if (is.infinite(max_delta_pred)) {
            "Posterior near-zero: invariance width is effectively unbounded"
        } else if (!is.na(max_delta_raw)) {
            "Within this raw-unit change, >= CI posterior mass stays inside ROPE on latent scale"
        } else {
            "Within this predictor-scale change, >= CI posterior mass stays inside ROPE on latent scale"
        }

        tibble(
            Parameter = param,
            Base_Variable = base_var,
            ROPE_Lower = rope_range[1],
            ROPE_Upper = rope_range[2],
            Credible_Mass = ci,
            AbsBeta_Quantile_for_CI = abs_beta_q,
            Max_Invariant_Change_PredictorScale = max_delta_pred,
            Raw_SD = raw_sd,
            Max_Invariant_Change_RawUnit = max_delta_raw,
            Unit_Label = unit_label,
            Interpretation = interp
        )
    })

    bind_rows(out)
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

    # 効果不変幅（変わったとはいえない最大変化量）を推定しCSV保存
    response_col <- colnames(data_orig)[questionToColumn[current_iquest]]
    analysis_data_raw <- data_orig %>%
        dplyr::filter(!is.na(Age)) %>%
        dplyr::filter(!is.na(.data[[response_col]]))

    raw_sd_map <- c(
        Age = stats::sd(analysis_data_raw$Age, na.rm = TRUE),
        Duration_sec = if ("Duration_sec_raw" %in% colnames(analysis_data_raw)) {
            stats::sd(analysis_data_raw$Duration_sec_raw, na.rm = TRUE)
        } else {
            NA_real_
        }
    )

    invariance_df <- compute_effect_invariance_ranges(
        current_model,
        rope_range = c(-0.1, 0.1),
        ci = 0.89,
        raw_sd_map = raw_sd_map
    )

    invariance_path <- file.path(question_output_dir, "Effect_Invariance_Range.csv")
    write.csv(invariance_df, invariance_path, row.names = FALSE)
    cat("Effect invariance range saved to:", invariance_path, "\n")
    
    cat("\n====== ROPE Analysis completed ======\n")
} else {
    cat("Warning: Model not found for question", current_iquest, ". Skipping ROPE analysis.\n")
}
