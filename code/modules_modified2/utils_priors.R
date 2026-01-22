# ==============================================================================
#  utils_priors.R - 事後draw CSV -> brms事前分布 ユーティリティ
# ==============================================================================
#  目的:
#    メインモデルの事後draw（posterior_samplesのCSV）を読み込み、
#    サブモデルで使用する情報的事前分布を組み立てる。
# ==============================================================================

library(readr)
library(dplyr)
library(brms)

#' 事後抽出データの読み込み
read_posterior_draws <- function(path) {
    if (!file.exists(path)) {
        stop(paste0("Posterior draws CSV not found: ", path))
    }
    readr::read_csv(path, show_col_types = FALSE)
}

.find_first_existing <- function(candidates, columns) {
    for (nm in candidates) {
        if (nm %in% columns) return(nm)
    }
    NULL
}

.param_column_for_prior_row <- function(draw_cols, class, coef = NA_character_, group = NA_character_) {
    if (is.na(class) || class == "") return(NULL)

    if (class == "b") {
        if (is.na(coef) || coef == "") return(NULL)
        return(paste0("b_", coef))
    }

    if (class == "Intercept") {
        if (is.na(coef) || coef == "") return(NULL)
        k <- coef
        candidates <- c(
            paste0("b_Intercept[", k, "]"),
            paste0("Intercept[", k, "]"),
            paste0("b_Intercept_", k),
            paste0("Intercept_", k)
        )
        return(.find_first_existing(candidates, draw_cols))
    }

    if (class == "sd") {
        if (is.na(group) || group == "" || is.na(coef) || coef == "") return(NULL)
        return(paste0("sd_", group, "__", coef))
    }

    NULL
}

#' 事後分布から事前分布を作成（モデル式に合わせたフィルタリング版）
make_priors_from_posterior_draws <- function(
    posterior_draws_path,
    formula,
    data,
    family,
    output_dir = "outputs",
    fallback_b = "normal(0,5)",
    fallback_intercept = "normal(0,5)",
    fallback_sd = "student_t(3, 0, 2)"
) {
    # メインモデルの事後ドローを読み込み
    draws <- read_posterior_draws(posterior_draws_path)
    draw_cols <- colnames(draws)

    # 現在のサブモデルが必要としているパラメータの一覧を取得
    # (adjust_formula_by_levels で調整済みの式を使用)
    current_priors <- brms::get_prior(formula, data = data, family = family)
    
    priors_list <- list()
    audit_log <- list()

    for (i in 1:nrow(current_priors)) {
        p_row <- current_priors[i, ]
        p_class <- p_row$class
        p_coef  <- p_row$coef
        p_group <- p_row$group
        
        # モデル全体や特定のグループ全体に対する事前分布（個別係数でないもの）はスキップ
        if (p_coef == "" && p_group == "" && p_class != "Intercept") next
        if (p_class == "cor") next # 相関パラメータは今回は省略
        
        # CSV内の対応する列名を特定
        col_name <- .param_column_for_prior_row(draw_cols, p_class, p_coef, p_group)
        
        if (!is.null(col_name) && col_name %in% draw_cols) {
            # 事後分布が存在する場合：その統計量を使用して事前分布を作成
            val_vec <- draws[[col_name]]
            
            if (p_class == "sd") {
                # sd は lognormal で近似
                est_meanlog <- mean(log(val_vec), na.rm = TRUE)
                est_sdlog <- sd(log(val_vec), na.rm = TRUE)
                dist <- paste0("lognormal(", round(est_meanlog, 4), ",", round(est_sdlog, 4), ")")
            } else {
                # b や Intercept は normal で近似
                est_mean <- mean(val_vec, na.rm = TRUE)
                est_sd <- sd(val_vec, na.rm = TRUE)
                dist <- paste0("normal(", round(est_mean, 4), ",", round(est_sd, 4), ")")
            }
            
            priors_list[[length(priors_list) + 1]] <- brms::set_prior(dist, class = p_class, coef = p_coef, group = p_group)
            
            audit_log[[length(audit_log) + 1]] <- data.frame(
                class = p_class, coef = p_coef, group = p_group, 
                prior = dist, used_posterior = TRUE
            )
        } else {
            # 事後分布に存在しない場合（サブモデル固有の変数など）：弱情報事前分布を使用
            dist <- switch(p_class,
                           "b" = fallback_b,
                           "Intercept" = fallback_intercept,
                           "sd" = fallback_sd,
                           "normal(0,10)") # default
            
            priors_list[[length(priors_list) + 1]] <- brms::set_prior(dist, class = p_class, coef = p_coef, group = p_group)
            
            audit_log[[length(audit_log) + 1]] <- data.frame(
                class = p_class, coef = p_coef, group = p_group, 
                prior = dist, used_posterior = FALSE
            )
        }
    }

    # 監査ログの保存
    audit_df <- do.call(rbind, audit_log)
    write.csv(audit_df, file.path(output_dir, "priors_audit.csv"), row.names = FALSE)

    return(do.call(c, priors_list))
}
