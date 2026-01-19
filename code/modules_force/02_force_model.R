# ==============================================================================
#  02_force_model.R - Force_field を含むモデル（main_modelの流儀に合わせる）
# ==============================================================================
#  概要:
#    - modules のメインモデル事後draw CSVを事前分布として使用
#    - Force_field を固定効果＆交互作用として追加
#    - model_force.rds にキャッシュ
# ==============================================================================

source("00_setup.R")
source("01_priors_from_posterior.R")

# ==============================================================================
#  Force モデル定義
# ==============================================================================

build_force_model <- function(data_orig, iquest, questionToColumn, posterior_draws_path, output_dir) {

    response_col <- colnames(data_orig)[questionToColumn[iquest]]

    # Force_field のデータが存在する人（行）を使用
    # ※ main_model と同様に Age は標準化する
    selectedData <- dplyr::filter(data_orig, !is.na(Age), !is.na(Force_field))
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)

    cat("\n[module_force] Data check\n")
    cat("  response:", response_col, "\n")
    cat("  rows:", nrow(selectedData), "\n")
    cat("  unique subjects:", dplyr::n_distinct(selectedData$Subject_No), "\n")
    cat("  missing response:", sum(is.na(selectedData[[response_col]])), "\n")

    formula_force <- as.formula(paste(
        response_col,
        "~ Condition +
        Position + Condition:Position +
        Location + Condition:Location +
        Previous_Exposure + Condition:Previous_Exposure +
        Cognitive_Load + Condition:Cognitive_Load +
        Duration_sec + Condition:Duration_sec +
        Force_field + Condition:Force_field +
        Age + Age:Condition +
        Gender_IsMale + (1+Condition|Experiment_ID)"
    ))

    family_force <- brms::cumulative("probit", threshold = "flexible")

    cat("\n[module_force] Building priors from modules posterior CSV...\n")
    priors_force <- make_priors_from_posterior_draws(
        posterior_draws_path = posterior_draws_path,
        formula = formula_force,
        data = selectedData,
        family = family_force,
        output_dir = output_dir
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

    return(list(
        model = model_force,
        selectedData = selectedData
    ))
}

# ==============================================================================
#  モデル実行（キャッシュ優先）
# ==============================================================================

cat("\n====== Building force model ======\n")

model_cache_path <- file.path(output_dir, "model_force.rds")

if (file.exists(model_cache_path)) {
    cat("Found cached force model. Loading:", model_cache_path, "\n")
    model_force <- readRDS(model_cache_path)

    # 後続モジュール用に selectedData も作成（main_modelと同様）
    response_col <- colnames(data_orig)[questionToColumn[iquest]]
    selectedData <- dplyr::filter(data_orig, !is.na(Age), !is.na(Force_field))
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)

    cat("\n[module_force] Data check (cached model)\n")
    cat("  response:", response_col, "\n")
    cat("  rows:", nrow(selectedData), "\n")
    cat("  unique subjects:", dplyr::n_distinct(selectedData$Subject_No), "\n")
    cat("  missing response:", sum(is.na(selectedData[[response_col]])), "\n")
} else {
    result <- build_force_model(
        data_orig = data_orig,
        iquest = iquest,
        questionToColumn = questionToColumn,
        posterior_draws_path = posterior_draws_path,
        output_dir = output_dir
    )
    model_force <- result$model
    selectedData <- result$selectedData

    saveRDS(model_force, model_cache_path)
    cat("Cached force model saved to:", model_cache_path, "\n")
}

# modules の可視化モジュール互換（再利用のためのエイリアス）
model_fullExperimentalParameters_dem <- model_force
model <- model_force

cat("\n====== Force model completed ======\n")
print(summary(model))

# module_force 側でも事後drawを保存（modulesと同じ出力パターン）
post_draws <- brms::posterior_samples(model_force) %>%
    dplyr::mutate(iter = seq_len(n()))

out_draws_path <- file.path(output_dir, "posterior_draws_force_model.csv")
readr::write_csv(post_draws, out_draws_path)
cat("[module_force] Posterior draws saved to:", out_draws_path, "\n")

sum_mat <- brms::posterior_summary(model_force, probs = c(0.055, 0.5, 0.945))
sum_df <- as.data.frame(sum_mat)
sum_df$parameter <- rownames(sum_df)
rownames(sum_df) <- NULL
sum_df <- sum_df %>% dplyr::select(parameter, dplyr::everything())

out_sum_path <- file.path(output_dir, "posterior_summary_force_model.csv")
readr::write_csv(sum_df, out_sum_path)
cat("[module_force] Posterior summary saved to:", out_sum_path, "\n")
