# ==============================================================================
#  01_priors_from_posterior.R - 事後draw CSV -> brms事前分布
# ==============================================================================
#  目的:
#    modules の main model 事後draw（posterior_samplesのCSV）を読み込み、
#    module_force のモデルで使用する情報的事前分布を組み立てる。
#
#  実装方針:
#    - b / Intercept: 正規分布 N(mean, sd)
#    - sd: ログ正規 lognormal(meanlog, sdlog)
#    - それ以外（cor等）はデフォルトに任せる（CSVには保存済み）
# ==============================================================================

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
        # brmsの ordinal threshold は coef が "1","2",... になることが多い
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
        # 例: sd_Experiment_ID__Intercept
        return(paste0("sd_", group, "__", coef))
    }

    NULL
}

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
    draws <- read_posterior_draws(posterior_draws_path)
    draw_cols <- colnames(draws)

    # 対象モデルのパラメータ一覧（brmsが受け付けるpriorの枠）
    prior_tbl <- brms::get_prior(formula = formula, data = data, family = family)

    .set_prior_safe <- function(dist, class, coef, group) {
        args <- list(prior = dist, class = class)
        if (!is.na(coef) && coef != "") args$coef <- coef
        if (!is.na(group) && group != "") args$group <- group
        do.call(brms::set_prior, args)
    }

    priors_list <- list()
    audit <- list()

    for (i in seq_len(nrow(prior_tbl))) {
        row <- prior_tbl[i, ]
        class <- row$class
        coef <- row$coef
        group <- row$group

        # ここでは主に b / Intercept / sd を扱う
        if (!(class %in% c("b", "Intercept", "sd"))) {
            next
        }

        col <- .param_column_for_prior_row(draw_cols, class, coef, group)

        used_posterior <- FALSE
        dist <- NULL
        est_mean <- NA_real_
        est_sd <- NA_real_

        if (!is.null(col) && col %in% draw_cols) {
            x <- draws[[col]]
            x <- x[is.finite(x)]

            if (length(x) >= 10) {
                used_posterior <- TRUE

                if (class %in% c("b", "Intercept")) {
                    est_mean <- mean(x)
                    est_sd <- stats::sd(x)
                    if (!is.finite(est_sd) || est_sd <= 0) est_sd <- 1e-6
                    dist <- sprintf("normal(%0.8f, %0.8f)", est_mean, est_sd)
                } else if (class == "sd") {
                    # sdは正の値なのでlognormalで近似
                    x_pos <- x[x > 0]
                    if (length(x_pos) >= 10) {
                        mu_log <- mean(log(x_pos))
                        sd_log <- stats::sd(log(x_pos))
                        if (!is.finite(sd_log) || sd_log <= 0) sd_log <- 1e-6
                        dist <- sprintf("lognormal(%0.8f, %0.8f)", mu_log, sd_log)
                        est_mean <- mean(x_pos)
                        est_sd <- stats::sd(x_pos)
                    } else {
                        used_posterior <- FALSE
                    }
                }
            }
        }

        if (!used_posterior || is.null(dist)) {
            if (class == "b") dist <- fallback_b
            if (class == "Intercept") dist <- fallback_intercept
            if (class == "sd") dist <- fallback_sd
        }

        priors_list[[length(priors_list) + 1]] <- .set_prior_safe(dist, class = class, coef = coef, group = group)

        audit[[length(audit) + 1]] <- data.frame(
            class = class,
            coef = ifelse(is.na(coef), "", coef),
            group = ifelse(is.na(group), "", group),
            posterior_column = ifelse(is.null(col), "", col),
            used_posterior = used_posterior,
            approx_mean = est_mean,
            approx_sd = est_sd,
            prior_spec = dist,
            stringsAsFactors = FALSE
        )
    }

    if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
    audit_df <- dplyr::bind_rows(audit)
    audit_path <- file.path(output_dir, "priors_from_modules_posterior.csv")
    readr::write_csv(audit_df, audit_path)
    cat("[module_force] Prior audit saved to:", audit_path, "\n")

    if (length(priors_list) == 0) {
        return(brms::prior())
    }

    priors <- do.call(c, priors_list)
    return(priors)
}
