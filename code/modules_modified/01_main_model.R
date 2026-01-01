# ==============================================================================
#  01_main_model.R - メインフルモデル
# ==============================================================================
#  概要: brmsによるフルモデルの定義と実行
#  依存: 00_setup.R
#  出力: model_fullExperimentalParameters_dem
# ==============================================================================

# セットアップ読み込み
source("00_setup.R")

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
    # ランダム効果: 実験間変動 + 個人差
    formula_full_dem <- as.formula(paste(
        colnames(data_orig)[questionToColumn[iquest]],
        "~ Condition +
        Position + Condition:Position +
        Location + Condition:Location +
        Previous_Exposure + Condition:Previous_Exposure +
        Cognitive_Load + Condition:Cognitive_Load +
        Duration_sec + Condition:Duration_sec +
        Force_field + Condition:Force_field +
        Age + Age:Condition +
        Gender_IsMale + 
        (1 + Condition | Experiment_ID) +
        (1 | Subject_No)"
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
        Force_field + Condition:Force_field +
        Age + Age:Condition +
        Gender_IsMale + 
        (1 + Condition | Experiment_ID) +
        (1 | Subject_No)"
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

cat("\n====== Building main model ======\n")

# メインモデル実行
result <- build_main_model(data_orig, iquest, questionToColumn)
model_fullExperimentalParameters_dem <- result$model
selectedData <- result$selectedData

# 短いエイリアス（後のプロットコードで使用）
model <- model_fullExperimentalParameters_dem

cat("\n====== Main model completed ======\n")
print(summary(model))
