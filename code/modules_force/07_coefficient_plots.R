# ==============================================================================
#  07_coefficient_plots.R - 係数プロット（modules版を再利用）
# ==============================================================================

if (!exists("model_fullExperimentalParameters_dem") && file.exists(file.path(output_dir, "model_force.rds"))) {
    model_force <- readRDS(file.path(output_dir, "model_force.rds"))
    model_fullExperimentalParameters_dem <- model_force
    model <- model_force
}

source("../modules/07_coefficient_plots.R")

# ==============================================================================
#  Force_field の効果も同様にグラフ化（modules_force追加分）
# ==============================================================================

if (exists("model_fullExperimentalParameters_dem")) {
    post <- brms::posterior_samples(model_fullExperimentalParameters_dem) %>%
        dplyr::mutate(iter = seq_len(n()))

    summarize_beta <- function(post, beta_name) {
        if (!(beta_name %in% colnames(post))) {
            return(NULL)
        }
        x <- post[[beta_name]]
        data.frame(
            beta = beta_name,
            median = median(x),
            lower_89 = as.numeric(stats::quantile(x, 0.055)),
            upper_89 = as.numeric(stats::quantile(x, 0.945)),
            lower_66 = as.numeric(stats::quantile(x, 0.17)),
            upper_66 = as.numeric(stats::quantile(x, 0.83)),
            stringsAsFactors = FALSE
        )
    }

    save_beta_plot <- function(beta_name, x_title, out_png) {
        if (!(beta_name %in% colnames(post))) {
            cat("[modules_force] Warning: beta not found in posterior draws:", beta_name, "\n")
            return(invisible(NULL))
        }
        p <- create_coefficient_plot(
            post,
            beta_name,
            x_title,
            colors = colors_paper,
            fontsize = fontsize_paper
        )
        ggsave(out_png, plot = p, units = "mm", width = 31, height = 40, dpi = 300)
        cat("[modules_force] Force effect plot saved to:", out_png, "\n")
        return(invisible(p))
    }

    # brms係数名（Force_field は No/Yes の2水準想定）
    beta_force <- "b_Force_fieldYes"
    beta_force_int <- "b_ConditionAsync:Force_fieldYes"

    # 係数プロット（Force）
    save_beta_plot(
        beta_force,
        "Force field\n(Yes)",
        file.path(output_dir, "PH_ForceField.png")
    )
    save_beta_plot(
        beta_force_int,
        "Asynchrony : Force field\n(Async, Yes)",
        file.path(output_dir, "PH_Async_ForceField.png")
    )

    # 条件付き効果（Force_field）
    try({
        create_conditional_effects_plot(
            model_fullExperimentalParameters_dem,
            effect = "Force_field",
            output_file = file.path(output_dir, "ConditionalEffects_ForceField")
        )
    }, silent = TRUE)

    # 数値要約もCSV保存
    force_sum <- dplyr::bind_rows(
        summarize_beta(post, beta_force),
        summarize_beta(post, beta_force_int)
    )
    if (!is.null(force_sum) && nrow(force_sum) > 0) {
        readr::write_csv(force_sum, file.path(output_dir, "ForceField_effects_summary.csv"))
        cat("[modules_force] Force effect summary saved to:", file.path(output_dir, "ForceField_effects_summary.csv"), "\n")
    }
}
