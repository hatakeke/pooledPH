# ==============================================================================
#  01_main_model.R - メインフルモデル（複数質問対応）
# ==============================================================================
#  概要: brmsによるフルモデルの定義と実行（複数質問に対応）
#  依存: 00_setup.R
#  出力: models_all（複数質問のモデルリスト）
# ==============================================================================

# セットアップ読み込み
source("00_setup.R")

# 推定を省略するか（推定済みモデルの利用）
# use_model_cache <- FALSE
use_model_cache <- TRUE   

# ==============================================================================
#  メインフルモデル定義
# ==============================================================================

#' フルモデルの構築と実行
#' @param data_orig 前処理済みデータ
#' @param iquest 質問インデックス
#' @param questionToColumn 質問列マッピング
#' @return brmsモデルオブジェクト
build_main_model <- function(data_orig, iquest, questionToColumn) {
    
    # 年齢欠損を除外（約4名）
    selectedData <- filter(data_orig, !is.na(Age))
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    # フルモデルの公式
    formula_full_dem <- as.formula(paste(
        colnames(data_orig)[questionToColumn[iquest]],
        "~ Condition +
        Position + Condition:Position +
        Location + Condition:Location +
        Previous_Exposure + Condition:Previous_Exposure +
        Cognitive_Load + Condition:Cognitive_Load +
        Duration_sec + Condition:Duration_sec +
        Age + Age:Condition +
        Gender_IsMale + (1+Condition|Experiment_ID)"
    ))
    
    print(formula_full_dem)
    print(paste0("Selected question ", iquest, ": ", questionsDescription[iquest]))
    
    # brmsモデル実行
    model_fullExperimentalParameters_dem <- brm(
        formula_full_dem, 
        data = selectedData,
        family = cumulative("probit", threshold = "flexible"),
        prior = c(
            set_prior("normal(0,5)", class = "b"),
            set_prior("student_t(3, 0, 2)", class = "sd")
        ),
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13), 
        init = 0
    )
    
    return(list(
        model = model_fullExperimentalParameters_dem,
        selectedData = selectedData
    ))
}

# ==============================================================================
#  SoA用モデル（代替公式）
# ==============================================================================

#' SoA（Sense of Agency）用モデルの構築
#' @description Cognitive_Load と Location を除外したモデル
build_soa_model <- function(data_orig, iquest, questionToColumn) {
    
    selectedData <- filter(data_orig, !is.na(Age))
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    formula_soa <- as.formula(paste(
        colnames(data_orig)[questionToColumn[iquest]],
        "~ Condition + 
        Position + Condition:Position + 
        Previous_Exposure + Condition:Previous_Exposure +
        Duration_sec + Condition:Duration_sec +
        Age + Age:Condition +
        Gender_IsMale + (1+Condition|Experiment_ID)"
    ))
    
    model_soa <- brm(
        formula_soa, 
        data = selectedData,
        family = cumulative("probit", threshold = "flexible"),
        prior = c(
            set_prior("normal(0,5)", class = "b"),
            set_prior("student_t(3, 0, 2)", class = "sd")
        ),
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13), 
        init = 0
    )
    
    return(list(
        model = model_soa,
        selectedData = selectedData
    ))
}

# ==============================================================================
#  モデル実行
# ==============================================================================

cat("\n====== Building models for multiple questions ======\n")

# 複数質問に対するモデル保存
models_all <- list()
selectedData_all <- list()

# use_complete_pairs_for_comparison が TRUE の場合、両質問で有効なデータを先に抽出
if (use_complete_pairs_for_comparison) {
    cat("\n[NA HANDLING] Strict mode: Using only rows with valid data for BOTH Q1 and Q7\n")
    
    # 両質問の列インデックス
    q_cols <- questionToColumn[analysis_questions]
    q_col_names <- colnames(data_orig)[q_cols]
    
    # 両質問でNAでない行を取得
    data_for_analysis <- data_orig %>%
        filter(!is.na(data_orig[[q_col_names[1]]]) & !is.na(data_orig[[q_col_names[2]]]))
    
    cat("  Original data:", nrow(data_orig), "rows\n")
    cat("  After removing NA in Q1 & Q7:", nrow(data_for_analysis), "rows\n")
    cat("  Lost:", nrow(data_orig) - nrow(data_for_analysis), "rows\n\n")
} else {
    cat("\n[NA HANDLING] Lenient mode: Each question uses available data\n")
    cat("  Q1 (Control) and Q7 (PH) may have different sample sizes\n\n")
    data_for_analysis <- data_orig
}

# analysis_questions ループで複数の質問を処理
for (idx in seq_along(analysis_questions)) {
    current_iquest <- analysis_questions[idx]
    question_label <- question_labels[idx]
    
    cat("\n--- Processing question", current_iquest, ":", questionsDescription[current_iquest], "---\n")
    
    # 質問ごとの出力ディレクトリ
    question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", question_label))
    if (!dir.exists(question_output_dir)) {
        dir.create(question_output_dir, recursive = TRUE)
        cat("Created output directory:", question_output_dir, "\n")
    }
    
    # モデルキャッシュパス（質問ごと）
    model_cache_path <- file.path(question_output_dir, "model_main.rds")
    
    if (file.exists(model_cache_path) && use_model_cache) {
        cat("Found cached model. Loading:", model_cache_path, "\n")
        model_fullExperimentalParameters_dem <- readRDS(model_cache_path)
        
        # selectedData も再作成
        selectedData <- filter(data_for_analysis, !is.na(Age))
        selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    } else {
        # メインモデル実行（時間がかかります）
        result <- build_main_model(data_for_analysis, current_iquest, questionToColumn)
        model_fullExperimentalParameters_dem <- result$model
        selectedData <- result$selectedData
        
        # キャッシュ保存
        saveRDS(model_fullExperimentalParameters_dem, model_cache_path)
        cat("Cached model saved to:", model_cache_path, "\n")
    }
    
    # リストに保存
    models_all[[question_label]] <- model_fullExperimentalParameters_dem
    selectedData_all[[question_label]] <- selectedData
    
    cat("\n========== Model summary for Q", current_iquest, " (", question_label, ") ==========\n", sep="")
    print(summary(model_fullExperimentalParameters_dem))
}

# メイン用途向けのグローバル変数（デフォルト値：PH）
model <- models_all[["PH"]]
model_fullExperimentalParameters_dem <- model
selectedData <- selectedData_all[["PH"]]
model_Control <- models_all[["Control"]]
selectedData_Control <- selectedData_all[["Control"]]

cat("\n====== All models completed ======\n")
cat("[Info] Posterior CSV export is handled by export_posterior.R\n")
cat("[Info] Posterior comparison is handled by compare_posterior.R\n")
