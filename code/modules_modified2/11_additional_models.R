# ==============================================================================
#  11_additional_models.R - 追加モデル（Order, EHI, PDI, Force）
# ==============================================================================
#  概要: 追加パラメータを含むサブモデルの構築と実行
#  依存: 00_setup.R, utils_priors.R
#  出力: 各サブモデルの .rds ファイルおよび分析結果
# ==============================================================================

# サブモデルを実行するかどうか
use_sub_model <- TRUE
# use_sub_model <- FALSE

# ユーティリティ読み込み
source("utils_priors.R")

# ==============================================================================
#  ヘルパー関数：式の動的調整
# ==============================================================================

#' データの水準数に応じて回帰式を調整する
#' @param data 抽出されたデータセット
#' @param base_formula_rhs 基本となる式の文字列（右辺のみ）
#' @param response responses変数名
#' @return 調整後の公式オブジェクト
adjust_formula_by_levels <- function(data, base_formula_rhs, response) {
    
    # 補助関数：カッコ内の+を無視して分割
    split_terms <- function(x) {
        chars <- strsplit(x, "")[[1]]
        depth <- 0
        indices <- c(0)
        for (i in seq_along(chars)) {
            if (chars[i] == "(") depth <- depth + 1
            if (chars[i] == ")") depth <- depth - 1
            if (chars[i] == "+" && depth == 0) indices <- c(indices, i)
        }
        indices <- c(indices, length(chars) + 1)
        res <- c()
        for (j in 1:(length(indices)-1)) {
            term <- substr(x, indices[j]+1, indices[j+1]-1)
            res <- c(res, trimws(term))
        }
        res[res != ""]
    }
    
    terms <- split_terms(base_formula_rhs)
    valid_terms <- c()
    
    for (term in terms) {
        if (grepl("\\|", term)) {
            # ランダム効果 "( slope | group )"
            inner <- gsub("^\\((.*)\\)$", "\\1", term)
            parts <- strsplit(inner, "\\|")[[1]]
            slope_part <- trimws(parts[1])
            group_part <- trimws(parts[2])
            
            # slope部分も分割してチェック
            slopes <- split_terms(slope_part)
            valid_slopes <- c()
            for (s in slopes) {
                if (s == "1") {
                    valid_slopes <- c(valid_slopes, s)
                    next
                }
                clean_s <- trimws(gsub("scale\\((.*)\\)", "\\1", s))
                if (clean_s %in% colnames(data)) {
                    if (length(unique(na.omit(data[[clean_s]]))) >= 2) {
                        valid_slopes <- c(valid_slopes, s)
                    }
                } else {
                    valid_slopes <- c(valid_slopes, s)
                }
            }
            if (length(valid_slopes) > 0) {
                valid_terms <- c(valid_terms, paste0("(", paste(valid_slopes, collapse = " + "), " | ", group_part, ")"))
            }
            
        } else if (grepl(":", term)) {
            # 交互作用
            components <- strsplit(term, ":")[[1]]
            all_valid <- TRUE
            for (comp in components) {
                clean_comp <- trimws(gsub("scale\\((.*)\\)", "\\1", comp))
                if (clean_comp %in% colnames(data)) {
                    if (length(unique(na.omit(data[[clean_comp]]))) < 2) all_valid <- FALSE
                }
            }
            if (all_valid) valid_terms <- c(valid_terms, term)
        } else {
            # 単一項
            clean_term <- trimws(gsub("scale\\((.*)\\)", "\\1", term))
            if (clean_term %in% colnames(data)) {
                if (length(unique(na.omit(data[[clean_term]]))) >= 2) {
                    valid_terms <- c(valid_terms, term)
                }
            } else {
                valid_terms <- c(valid_terms, term)
            }
        }
    }
    
    formula_str <- paste(response, "~", paste(valid_terms, collapse = " + "))
    return(as.formula(formula_str))
}

# ==============================================================================
#  サブモデル定義（Order）
# ==============================================================================

#' Order（順序効果）を含むモデルの構築
#' @param data_orig 前処理済みデータ
#' @param iquest 質問インデックス
#' @param questionToColumn 質問列マッピング
#' @param posterior_draws_path 事前分布用の事後ドローCSVパス
#' @param current_output_dir 出力ディレクトリ
#' @return リスト（model, selectedData）
build_order_model <- function(data_orig, iquest, questionToColumn, posterior_draws_path, current_output_dir) {
    
    response_col <- colnames(data_orig)[questionToColumn[iquest]]
    
    # 予測変数リスト
    predictors <- c("Condition", "Device", "Previous_Exposure", "Cognitive_Load", "Duration_sec", "Age", "Gender_IsMale", "Order")
    
    # NAを除外（brmsの内部処理と一致させるため）
    selectedData <- data_orig %>% 
        dplyr::filter(if_all(all_of(c(response_col, predictors)), ~ !is.na(.)))

    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    # 基本の右辺
    base_formula_rhs <- "Condition + Device + Condition:Device + 
                         Previous_Exposure + Condition:Previous_Exposure + Cognitive_Load + Condition:Cognitive_Load + 
                         Duration_sec + Duration_sec:Condition + Age + Age:Condition + Gender_IsMale + 
                         Order + Order:Condition + (1+Condition|Experiment_ID)"
    
    # 水準不足によるエラー回避のために式を調整
    formula_order <- adjust_formula_by_levels(selectedData, base_formula_rhs, response_col)

    family_type <- cumulative("probit", threshold = "flexible")

    cat("\n[module_sub] Building priors from main model for Order...\n")
    priors_order <- make_priors_from_posterior_draws(
        posterior_draws_path = posterior_draws_path,
        formula = formula_order,
        data = selectedData,
        family = family_type,
        output_dir = current_output_dir
    )
    
    model_order <- brm(
        formula_order, 
        data = selectedData,
        family = family_type,
        prior = priors_order,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13),
        init = 0
    )
    
    return(list(model = model_order, selectedData = selectedData))
}

# ==============================================================================
#  サブモデル定義（EHI）
# ==============================================================================

#' EHI（Edinburgh Handedness Inventory）を含むモデルの構築
#' @param data_orig 前処理済みデータ
#' @param iquest 質問インデックス
#' @param questionToColumn 質問列マッピング
#' @param posterior_draws_path 事前分布用の事後ドローCSVパス
#' @param current_output_dir 出力ディレクトリ
#' @return リスト（model, selectedData）
build_ehi_model <- function(data_orig, iquest, questionToColumn, posterior_draws_path, current_output_dir) {
    
    response_col <- colnames(data_orig)[questionToColumn[iquest]]
    
    # 予測変数リスト
    predictors <- c("Condition", "Device", "Previous_Exposure", "Cognitive_Load", "Duration_sec", "Age", "Gender_IsMale", "EHI")
    
    # NAを除外
    selectedData <- data_orig %>% 
        dplyr::filter(if_all(all_of(c(response_col, predictors)), ~ !is.na(.)))

    # 応答変数の水準チェック
    if (length(unique(na.omit(selectedData[[response_col]]))) < 2) {
        cat("\nSkipping sub-model EHI: Response variable has only one level.\n")
        return(list(model = NULL, selectedData = selectedData))
    }

    selectedData$EHI <- (selectedData$EHI - mean(selectedData$EHI)) / sd(selectedData$EHI)
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    # 基本の右辺
    base_formula_rhs <- "Condition + Device + Condition:Device + 
                         Previous_Exposure + Condition:Previous_Exposure + Cognitive_Load + Condition:Cognitive_Load + 
                         Duration_sec + Duration_sec:Condition + Age + Age:Condition + Gender_IsMale + 
                         EHI + EHI:Condition + (1+Condition|Experiment_ID)"
    
    # 水準不足によるエラー回避のために式を調整
    formula_ehi <- adjust_formula_by_levels(selectedData, base_formula_rhs, response_col)

    family_type <- cumulative("probit", threshold = "flexible")
    
    cat("\n[module_sub] Building priors from main model for EHI...\n")
    priors_ehi <- make_priors_from_posterior_draws(
        posterior_draws_path = posterior_draws_path,
        formula = formula_ehi,
        data = selectedData,
        family = family_type,
        output_dir = current_output_dir
    )
    
    model_ehi <- brm(
        formula_ehi, 
        data = selectedData,
        family = family_type,
        prior = priors_ehi,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13),
        init = 0
    )
    
    return(list(model = model_ehi, selectedData = selectedData))
}

# ==============================================================================
#  サブモデル定義（PDI）
# ==============================================================================

#' PDI（Peters et al. Delusions Inventory）を含むモデルの構築
#' @param data_orig 前処理済みデータ
#' @param iquest 質問インデックス
#' @param questionToColumn 質問列マッピング
#' @param posterior_draws_path 事前分布用の事後ドローCSVパス
#' @param current_output_dir 出力ディレクトリ
#' @return リスト（model, selectedData）
build_pdi_model <- function(data_orig, iquest, questionToColumn, posterior_draws_path, current_output_dir) {
    
    response_col <- colnames(data_orig)[questionToColumn[iquest]]
    
    # 予測変数リスト
    predictors <- c("Condition", "Device", "Previous_Exposure", "Cognitive_Load", "Duration_sec", "Age", "Gender_IsMale", "PDI")
    
    # NAを除外
    selectedData <- data_orig %>% 
        dplyr::filter(if_all(all_of(c(response_col, predictors)), ~ !is.na(.)))

    # 応答変数の水準チェック
    if (length(unique(na.omit(selectedData[[response_col]]))) < 2) {
        cat("\nSkipping sub-model PDI: Response variable has only one level.\n")
        return(list(model = NULL, selectedData = selectedData))
    }

    selectedData$PDI <- (selectedData$PDI - mean(selectedData$PDI)) / sd(selectedData$PDI)
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    # 基本の右辺
    base_formula_rhs <- "Condition + Device + Condition:Device + 
                         Previous_Exposure + Condition:Previous_Exposure + Cognitive_Load + Condition:Cognitive_Load + 
                         Duration_sec + Duration_sec:Condition + Age + Age:Condition + Gender_IsMale + 
                         PDI + PDI:Condition + (1+Condition|Experiment_ID)"
    
    # 水準不足によるエラー回避のために式を調整
    formula_pdi <- adjust_formula_by_levels(selectedData, base_formula_rhs, response_col)
    
    family_type <- cumulative("probit", threshold = "flexible")
    
    cat("\n[module_sub] Building priors from main model for PDI...\n")
    priors_pdi <- make_priors_from_posterior_draws(
        posterior_draws_path = posterior_draws_path,
        formula = formula_pdi,
        data = selectedData,
        family = family_type,
        output_dir = current_output_dir
    )
    
    model_pdi <- brm(
        formula_pdi, 
        data = selectedData,
        family = family_type,
        prior = priors_pdi,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13), 
        init = 0
    )
    
    return(list(model = model_pdi, selectedData = selectedData))
}

# ==============================================================================
#  サブモデル定義（Force）
# ==============================================================================

#' Force_field を含むモデルの構築
build_force_model <- function(data_orig, iquest, questionToColumn, posterior_draws_path, current_output_dir) {

    response_col <- colnames(data_orig)[questionToColumn[iquest]]

    # 予測変数リスト
    predictors <- c("Condition", "Device", "Previous_Exposure", "Cognitive_Load", "Duration_sec", "Age", "Gender_IsMale", "Force_field")

    # NAを除外
    selectedData <- data_orig %>% 
        dplyr::filter(if_all(all_of(c(response_col, predictors)), ~ !is.na(.)))

    # 応答変数の水準チェック
    if (length(unique(na.omit(selectedData[[response_col]]))) < 2) {
        cat("\nSkipping sub-model Force: Response variable has only one level.\n")
        return(list(model = NULL, selectedData = selectedData))
    }

    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)

    # 基本の右辺
    base_formula_rhs <- "Condition + Device + Condition:Device + 
                         Previous_Exposure + Condition:Previous_Exposure + Cognitive_Load + Condition:Cognitive_Load + 
                         Duration_sec + Duration_sec:Condition + Age + Age:Condition + Gender_IsMale + 
                         Force_field + Force_field:Condition + (1+Condition|Experiment_ID)"

    # 水準不足によるエラー回避のために式を調整
    formula_force <- adjust_formula_by_levels(selectedData, base_formula_rhs, response_col)

    family_force <- brms::cumulative("probit", threshold = "flexible")

    cat("\n[module_sub] Building priors from main model for Force...\n")
    priors_force <- make_priors_from_posterior_draws(
        posterior_draws_path = posterior_draws_path,
        formula = formula_force,
        data = selectedData,
        family = family_force,
        output_dir = current_output_dir
    )

    model_force <- brms::brm(
        formula_force,
        data = selectedData,
        family = family_force,
        prior = priors_force,
        warmup = 2000,
        iter = 8000,
        cores = 4,
        chains = 4,
        control = list(max_treedepth = 13),
        init = 0
    )

    return(list(model = model_force, selectedData = selectedData))
}

# ==============================================================================
#  オーケストレーション（一括実行ロジック）
# ==============================================================================

if (use_sub_model) {
    
    # 分析対象のサブモデル定義
    sub_models <- list(
        list(name = "Order", build_fn = build_order_model, data_check_col = "Order"),
        list(name = "EHI", build_fn = build_ehi_model, data_check_col = "EHI"),
        list(name = "PDI", build_fn = build_pdi_model, data_check_col = "PDI"),
        list(name = "Force", build_fn = build_force_model, data_check_col = "Force_field")
    )
    
    for (q_idx in seq_along(analysis_questions)) {
        current_iquest <- analysis_questions[q_idx]
        current_question_label_orig <- question_labels[q_idx]
        
        # メインモデルの事後分布パス（事前分布として必要）
        main_post_path <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label_orig), "posterior_draws_main_model.csv")
        
        if (!file.exists(main_post_path)) {
            cat("Warning: Main model posterior CSV not found for Q", current_iquest, ". Skipping sub-models.\n")
            next
        }
        
        for (sub in sub_models) {
            current_question_label <- paste0(current_question_label_orig, "_", sub$name)
            
            # 出力ディレクトリ
            question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label))
            if (!dir.exists(question_output_dir)) dir.create(question_output_dir, recursive = TRUE)
            
            model_cache_path <- file.path(question_output_dir, paste0("model_", tolower(sub$name), ".rds"))
            
            # データ存在チェック（サブモデル特有の列があるか）
            # NAを除外して行数が残るか確認
            sub_col <- sub$data_check_col
            valid_rows <- data_orig %>% 
                dplyr::filter(!is.na(.data[[sub_col]]), !is.na(Age))
            
            if (nrow(valid_rows) == 0) {
                cat("\nSkipping sub-model", sub$name, "for Q", current_iquest, "(No valid data for", sub_col, ")\n")
                next
            }
            
            # 推定をスキップするか（キャッシュ利用）
            if (use_model_cache && file.exists(model_cache_path)) {
                cat("\nLoading cached sub-model:", sub$name, "for Q", current_iquest, "\n")
                model_res <- list(
                    model = readRDS(model_cache_path),
                    selectedData = valid_rows
                )
                # 年齢の標準化（推定時と同じ処理を再現）
                model_res$selectedData$Age <- (model_res$selectedData$Age - mean(model_res$selectedData$Age)) / sd(model_res$selectedData$Age)
            } else {
                cat("\nEstimating sub-model:", sub$name, "for Q", current_iquest, "...\n")
                model_res <- sub$build_fn(
                    data_orig = data_orig,
                    iquest = current_iquest,
                    questionToColumn = questionToColumn,
                    posterior_draws_path = main_post_path,
                    current_output_dir = question_output_dir
                )
                if (!is.null(model_res$model)) {
                    saveRDS(model_res$model, model_cache_path)
                    cat("Sub-model saved to:", model_cache_path, "\n")
                }
            }
            
            # 分析スクリプト用のコンテキスト設定
            if (!is.null(model_res$model)) {
                current_model <- model_res$model
                selectedData_current <- model_res$selectedData
                
                # 各種分析の実行
                cat("Running analysis for", current_question_label, "...\n")
                source("05_forest_plot.R")
                source("06_funnel_plot.R")
                source("07_posterior_check.R")
                source("08_convergence_diagnostics.R")
                source("09_coefficient_plots.R")
                source("10_rope_analysis.R")
            } else {
                cat("Skipping further analysis for", sub$name, "(Model estimation was skipped or failed)\n")
            }
        }
    }
} else {
    cat("====== Additional models module loaded (build functions only) ======\n")
    cat("Available: build_order_model(), build_ehi_model(), build_pdi_model(), build_force_model()\n")
}

