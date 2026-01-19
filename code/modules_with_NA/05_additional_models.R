# ==============================================================================
#  05_additional_models.R - 追加モデル（Order, EHI, PDI）
# ==============================================================================
#  概要: 追加パラメータを含むモデルの構築と実行
#  依存: 00_setup.R
#  出力: model_fullExperimentalParameters_dem_order, _EHI, _PDI
# ==============================================================================

# セットアップ読み込み
# source("./R/pooledPH/code/modules/00_setup.R")

# ==============================================================================
#  事前分布定義（共通）
# ==============================================================================

# 情報的事前分布（先行研究から推定）
informative_priors <- c(
    set_prior("normal(-0.4220, 0.1367)", class = "Intercept", coef = "1"),
    set_prior("normal(-0.0520, 0.1365)", class = "Intercept", coef = "2"),
    set_prior("normal(0.2973, 0.1366)", class = "Intercept", coef = "3"),
    set_prior("normal(0.5935, 0.1371)", class = "Intercept", coef = "4"),
    set_prior("normal(1.0518, 0.1388)", class = "Intercept", coef = "5"),
    set_prior("normal(1.5836, 0.1429)", class = "Intercept", coef = "6"),
    set_prior("normal(0.3406, 0.0908)", coef = "ConditionAsync"),
    set_prior("normal(-0.6545, 0.2808)", coef = "PositionLaying"),
    set_prior("normal(-0.6079, 0.1740)", coef = "LocationHand"),
    set_prior("normal(-0.0983, 0.1524)", coef = "Previous_ExposureRobotmanipulation"),
    set_prior("normal(0.1719, 0.4473)", coef = "Cognitive_LoadYes"),
    set_prior("normal(-0.0299, 0.1619)", coef = "Duration_sec"),
    set_prior("normal(-0.0846, 0.0470)", coef = "Age"),
    set_prior("normal(-0.0732, 0.0640)", coef = "Gender_IsMaleMale"),
    set_prior("normal(0.0651, 0.2104)", coef = "ConditionAsync:PositionLaying"),
    set_prior("normal(0.0023, 0.2130)", coef = "ConditionAsync:LocationHand"),
    set_prior("normal(-0.1607, 0.1542)", coef = "ConditionAsync:Previous_ExposureRobotmanipulation"),
    set_prior("normal(-0.2214, 0.2986)", coef = "ConditionAsync:Cognitive_LoadYes"),
    set_prior("normal(0.0018, 0.1146)", coef = "ConditionAsync:Duration_sec"),
    set_prior("normal(-0.0840, 0.0641)", coef = "ConditionAsync:Age")
)

# ==============================================================================
#  Orderモデル
# ==============================================================================

#' Order（順序効果）を含むモデルの構築
#' @param data_orig 前処理済みデータ
#' @param iquest 質問インデックス
#' @param questionToColumn 質問列マッピング
#' @return brmsモデルオブジェクト
build_order_model <- function(data_orig, iquest, questionToColumn) {
    
    selectedData <- filter(data_orig, !is.na(Age))
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    formula_full_dem_order <- as.formula(paste(
        colnames(data_orig)[questionToColumn[iquest]],
        "~ Condition +
        Position + Condition:Position + 
        Location + Condition:Location + 
        Previous_Exposure + Condition:Previous_Exposure +
        Cognitive_Load + Condition:Cognitive_Load +
        Duration_sec + Duration_sec:Condition +
        Age + Age:Condition +
        Gender_IsMale + 
        Order + Order:Condition + 
        (1+Condition|Experiment_ID)"
    ))
    
    model_order <- brm(
        formula_full_dem_order, 
        data = selectedData,
        family = cumulative("probit", threshold = "flexible"),
        prior = informative_priors,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13)
    )
    
    return(model_order)
}

# ==============================================================================
#  EHIモデル
# ==============================================================================

#' EHI（Edinburgh Handedness Inventory）を含むモデルの構築
#' @param data_orig 前処理済みデータ
#' @param iquest 質問インデックス
#' @param questionToColumn 質問列マッピング
#' @return brmsモデルオブジェクト
build_ehi_model <- function(data_orig, iquest, questionToColumn) {
    
    selectedData <- filter(data_orig, !is.na(Age), !is.na(EHI))
    selectedData$EHI <- (selectedData$EHI - mean(selectedData$EHI)) / sd(selectedData$EHI)
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    formula_full_dem_EHI <- as.formula(paste(
        colnames(data_orig)[questionToColumn[iquest]],
        "~ Condition + 
        Position + Condition:Position + 
        Location + Condition:Location +
        Previous_Exposure + Condition:Previous_Exposure +
        Cognitive_Load + Condition:Cognitive_Load +
        Duration_sec + Duration_sec:Condition +
        Age + Age:Condition +
        Gender_IsMale +
        EHI + EHI:Condition + 
        (1+Condition|Experiment_ID)"
    ))
    
    model_EHI <- brm(
        formula_full_dem_EHI, 
        data = selectedData,
        family = cumulative("probit", threshold = "flexible"),
        prior = informative_priors,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13)
    )
    
    return(model_EHI)
}

# ==============================================================================
#  PDIモデル
# ==============================================================================

#' PDI（Peters et al. Delusions Inventory）を含むモデルの構築
#' @param data_orig 前処理済みデータ
#' @param iquest 質問インデックス
#' @param questionToColumn 質問列マッピング
#' @return brmsモデルオブジェクト
build_pdi_model <- function(data_orig, iquest, questionToColumn) {
    
    selectedData <- filter(data_orig, !is.na(Age), !is.na(PDI))
    selectedData$PDI <- (selectedData$PDI - mean(selectedData$PDI)) / sd(selectedData$PDI)
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    formula_full_dem_PDI <- as.formula(paste(
        colnames(data_orig)[questionToColumn[iquest]],
        "~ Condition + 
        Position + Condition:Position + 
        Location + Condition:Location + 
        Previous_Exposure + Condition:Previous_Exposure +
        Cognitive_Load + Condition:Cognitive_Load +
        Duration_sec + Duration_sec:Condition +
        Age + Age:Condition +
        Gender_IsMale +
        PDI + PDI:Condition + 
        (1+Condition|Experiment_ID)"
    ))
    
    model_PDI <- brm(
        formula_full_dem_PDI, 
        data = selectedData,
        family = cumulative("probit", threshold = "flexible"),
        prior = informative_priors,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13), 
        init = 0
    )
    
    return(model_PDI)
}

# ==============================================================================
#  実行（コメントアウト - 必要に応じて実行）
# ==============================================================================

# 以下のコードは時間がかかるため、必要に応じて個別に実行してください

# cat("\n====== Building Order Model ======\n")
# model_fullExperimentalParameters_dem_order <- build_order_model(data_orig, iquest, questionToColumn)

# cat("\n====== Building EHI Model ======\n")
# model_fullExperimentalParameters_dem_EHI <- build_ehi_model(data_orig, iquest, questionToColumn)

# cat("\n====== Building PDI Model ======\n")
# model_fullExperimentalParameters_dem_PDI <- build_pdi_model(data_orig, iquest, questionToColumn)

cat("====== Additional models module loaded ======\n")
cat("Available functions: build_order_model(), build_ehi_model(), build_pdi_model()\n")
