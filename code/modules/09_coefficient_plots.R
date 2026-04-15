# ==============================================================================
#  09_coefficient_plots.R - 係数プロット
# ==============================================================================
#  概要: 各効果の係数推定値の可視化
#  依存: 00_setup.R, 01_main_model.R, 05_additional_models.R
#  出力: 各種PNGファイル、Coefficient_summary.csv（質問ごとのディレクトリに保存）
# ==============================================================================

library(ggplot2)
library(tidybayes)
library(extrafont)

# ==============================================================================
#  質問ごとの出力ディレクトリ設定
# ==============================================================================
if (!exists("current_question_label")) {
    stop("Error: current_question_label not defined. This script must be called from main.R with question context.")
}

question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label))

# ==============================================================================
#  プロット設定
# ==============================================================================

# 図のファイル名定義
figure_names <- c(
    "Effect_Async.png",
    "Effect_BodyPosition.png",
    "Effect_BodyLocation.png",
    "Effect_Previousexposure.png",
    "Effect_CognitiveLoad.png",
    "Effect_Duration.png",
    "Effect_Age.png",
    "Effect_Async_BodyPosition.png",
    "Effect_Async_BodyLocation.png",
    "Effect_Async_PreviousExposure.png",
    "Effect_Async_CognitiveLoad.png",
    "Effect_Async_Duration.png",
    "Effect_Async_Age.png",
    "Effect_SexAtBirth.png",
    "Effect_ForceField.png",
    "Effect_Async_ForceField.png",
    "Effect_Order.png",
    "Effect_Async_Order.png",
    "Effect_EHI.png",
    "Effect_Async_EHI.png",
    "Effect_PDI.png",
    "Effect_Async_PDI.png"
)

# ベータ係数名
betas <- c(
    "b_ConditionAsync", 
    "b_PositionLaying", 
    "b_LocationHand",
    "b_Previous_ExposureRobotmanipulation", 
    "b_Cognitive_LoadYes", 
    "b_Duration_sec", 
    "b_Age",
    "b_ConditionAsync:PositionLaying",
    "b_ConditionAsync:LocationHand",
    "b_ConditionAsync:Previous_ExposureRobotmanipulation", 
    "b_ConditionAsync:Cognitive_LoadYes",
    "b_ConditionAsync:Duration_sec",
    "b_ConditionAsync:Age",
    "b_Gender_IsMaleMale",
    "b_Force_fieldYes",
    "b_ConditionAsync:Force_fieldYes",
    "b_Order2",
    "b_ConditionAsync:Order2",
    "b_EHI",
    "b_ConditionAsync:EHI",
    "b_PDI",
    "b_ConditionAsync:PDI"
)

# X軸タイトル
xtitles <- c(
    "Asynchrony\n(async)", 
    "Body Position\n(supine)",
    "Body Location\n(hand)",
    "Previous Exposure\n(+)",
    "Cognitive Load\n(+)", 
    "Duration\n(increasing)",
    "Age\n(increasing in age)",
    "Asynchrony : Body Position\n(Async, supine)",
    "Asynchrony : Body Location\n(Async, hand)",
    "Asynchrony : Prev. Exp.\n(Async, +)", 
    "Asynchrony : Cog. Load\n(Async, +)",
    "Asynchrony : Duration\n(Async, Increasing)",
    "Asynchrony : Age\n(Async, Increasing)",
    "Sex at birth\n(Male)",
    "Force field\n(Yes)",
    "Asynchrony : Force field\n(Async, Yes)",
    "Order\n(2nd)",
    "Asynchrony : Order\n(Async, 2nd)",
    "EHI\n(L -> R handed)",
    "Asynchrony : EHI\n(Async, L -> R)",
    "PDI\n(increasing)",
    "Asynchrony : PDI\n(Async, increasing)"
)

# ==============================================================================
#  係数プロット関数
# ==============================================================================

#' 単一係数のHalf-eyeプロット作成
#' @param post 事後サンプルデータフレーム
#' @param beta_name 係数名
#' @param x_title X軸タイトル
#' @param colors 色設定
#' @param fontsize フォントサイズ
#' @param rope_range ROPE範囲
#' @return ggplotオブジェクト
create_coefficient_plot <- function(
    post, 
    beta_name, 
    x_title,
    colors = c("#d4bbff", "#4589ff"),
    fontsize = 7,
    rope_range = 0.1
    ) {
    
    # 手動で統計量を計算
    vals <- post[[beta_name]]
    summary_df <- data.frame(
        median = median(vals),
        lower_89 = quantile(vals, 0.055),
        upper_89 = quantile(vals, 0.945),
        lower_66 = quantile(vals, 0.17),
        upper_66 = quantile(vals, 0.83),
        y = 0
    )
    
    p <- ggplot(summary_df, aes(x = median, y = y)) +
        geom_linerange(aes(xmin = lower_89, xmax = upper_89), color = colors[1], linewidth = 0.8) +
        geom_linerange(aes(xmin = lower_66, xmax = upper_66), color = colors[1], linewidth = 1.5) +
        geom_point(color = colors[2], size = 1.5) +
        geom_vline(xintercept = rope_range, linetype = "dashed") +
        geom_vline(xintercept = -rope_range, linetype = "dashed") +
        theme_minimal() + 
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            text = element_text(size = fontsize, )
        ) +
        xlab(x_title)
    
    return(p)
}

#' 交互作用係数のプロット作成
#' @param post 事後サンプルデータフレーム    
#' @param interaction_var 交互作用変数名
#' @param x_title X軸タイトル
#' @param colors 色設定
#' @param fontsize フォントサイズ
#' @return ggplotオブジェクト
create_interaction_plot <- function(
    post, 
    interaction_var,
    x_title,
    colors = c("#d4bbff", "#4589ff"),
    fontsize = 7
    ) {
    
    # 手動で統計量を計算
    vals <- post[[interaction_var]]
    summary_df <- data.frame(
        median = median(vals),
        lower_89 = quantile(vals, 0.055),
        upper_89 = quantile(vals, 0.945),
        lower_66 = quantile(vals, 0.17),
        upper_66 = quantile(vals, 0.83),
        y = 0
    )
    
    p <- ggplot(summary_df, aes(x = median, y = y)) +
        geom_linerange(aes(xmin = lower_89, xmax = upper_89), color = colors[1], linewidth = 0.8) +
        geom_linerange(aes(xmin = lower_66, xmax = upper_66), color = colors[1], linewidth = 1.5) +
        geom_point(color = colors[2], size = 1.5) +
        geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
        theme_minimal() + 
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            text = element_text(size = fontsize, )
        ) +
        xlab(x_title)
    
    return(p)
}

#' 主効果のバッチプロット生成
#' @param model brmsモデルオブジェクト
#' @param indices プロットする効果のインデックス
#' @param save_files ファイル保存するか
generate_main_effect_plots <- function(
    model, 
    indices = c(2:7),
    save_files = FALSE,
    output_dir = NULL
    ) {
    
    post <- as_draws_df(model) %>%
        as_tibble() %>%
        mutate(iter = 1:n())
    
    plots <- list()
    
    for (i in indices) {
        p <- create_coefficient_plot(
            post, 
            betas[i], 
            xtitles[i],
            colors = colors_paper,
            fontsize = fontsize_paper
        )
        
        plots[[i]] <- p
        
        if (save_files && !is.null(output_dir)) {
            out_file <- file.path(output_dir, figure_names[i])
            if (i == 1) {
                ggsave(out_file, plot = p, units = "mm", width = 61.35, height = 40, dpi = 300)
            } else if (i == 7) {
                ggsave(out_file, plot = p, units = "mm", width = 42, height = 48, dpi = 300)
            } else {
                ggsave(out_file, plot = p, units = "mm", width = 31, height = 40, dpi = 300)
            }
            cat("Coefficient plot saved to:", out_file, "\n")
        }
    }
    
    return(plots)
}

#' 条件付き効果プロット
#' @param model brmsモデルオブジェクト
#' @param effect 効果名
#' @param output_file 出力ファイル名
#' @return ggplotオブジェクト
create_conditional_effects_plot <- function(
    model, 
    effect = "Age",
    output_file = NULL
    ) {
    
    ce <- conditional_effects(model, categorical = TRUE, effects = effect)
    ce_plot <- plot(ce, plot = FALSE)[[1]]  # plot = FALSE でデバイスへの描画を防止
    
    p <- ce_plot + 
        xlab(effect) +
        theme_minimal() +
        theme(
            text = element_text(size = fontsize_paper)
        )
    
    if (!is.null(output_file)) {
        png_file <- paste0(output_file, ".png")
        ggsave(png_file, plot = p, units = "mm", width = 70, height = 60, dpi = 300)
        cat("Conditional effects plot saved to:", png_file, "\n")
    }
    
    return(p)
}

# ==============================================================================
#  Intercept プロット
# ==============================================================================

#' インターセプト分布プロット
#' @param model brmsモデルオブジェクト
#' @param colors 色設定
#' @param fontsize フォントサイズ
#' @param output_file 出力ファイル名（拡張子なし）
#' @return ggplotオブジェクトのリスト
create_intercept_plots <- function(
    model, 
    colors = c("#d4bbff", "#4589ff"),
    fontsize = 7,
    rating_labels = NULL,
    output_file = NULL
    ) {
    
    intercept_cols <- as_draws_df(model) %>%
        as_tibble() %>%
        dplyr::select(dplyr::matches("^b_Intercept\\[[0-9]+\\]$")) %>%
        colnames()

    if (length(intercept_cols) == 0) {
        stop("No intercept parameters found in model.")
    }

    post_predictions <- as_draws_df(model) %>%
        as_tibble() %>%
        select(all_of(intercept_cols)) %>%
        mutate(iter = 1:n())

    n_thresholds <- length(intercept_cols)
    n_categories <- n_thresholds + 1
    if (is.null(rating_labels) || length(rating_labels) != n_categories) {
        rating_labels <- as.character(0:(n_categories - 1))
    }
    
    # 確率分布プロット
    cumulative_prob <- post_predictions %>%
        select(-iter) %>%
        mutate_all(.funs = ~ pnorm(., 0, 1)) %>%
        as.matrix()

    category_prob <- matrix(NA_real_, nrow = nrow(cumulative_prob), ncol = n_categories)
    category_prob[, 1] <- cumulative_prob[, 1]
    if (n_categories > 2) {
        for (j in 2:(n_categories - 1)) {
            category_prob[, j] <- cumulative_prob[, j] - cumulative_prob[, j - 1]
        }
    }
    category_prob[, n_categories] <- 1 - cumulative_prob[, n_thresholds]

    p1_data <- as_tibble(category_prob) %>%
        set_names(rating_labels) %>%
        pivot_longer(everything(), names_to = "Rating", values_to = "value")
    
    # 手動で統計量を計算
    p1_summary <- p1_data %>%
        group_by(Rating) %>%
        summarise(
            median = median(value),
            lower_89 = quantile(value, 0.055),
            upper_89 = quantile(value, 0.945),
            lower_66 = quantile(value, 0.17),
            upper_66 = quantile(value, 0.83),
            .groups = "drop"
        )
    
    p1 <- ggplot(p1_summary, aes(x = median, y = factor(Rating, levels = rev(rating_labels)))) +
        geom_linerange(aes(xmin = lower_89, xmax = upper_89), color = colors[1], linewidth = 0.8) +
        geom_linerange(aes(xmin = lower_66, xmax = upper_66), color = colors[1], linewidth = 1.5) +
        geom_point(color = colors[2], size = 1.5) +
        scale_x_continuous(
            expression(italic(p)*"("*italic(rating)*")"), 
            expand = c(0, 0), 
            limits = c(0, 0.65)
        ) + 
        labs(y = "Rating") +
        theme_minimal() + 
        theme(
            legend.position = "none",
            text = element_text(size = fontsize, )
        )
    
    # 正規分布 + 閾値プロット
    fixef_intercepts <- fixef(model)[grepl("^Intercept\\[", rownames(fixef(model))), 1]

    p2 <- tibble(x = seq(from = -3.5, to = 3.5, by = .01)) %>%
        mutate(d = dnorm(x)) %>% 
        ggplot(aes(x = x, ymin = 0, ymax = d)) +
        geom_ribbon(fill = "black") +
        geom_vline(
            xintercept = fixef_intercepts,
            color = colors[1], 
            linetype = 2, 
            size = 0.75
        ) +
        scale_x_continuous(
            "Posterior modes for the rating scale intercepts",
            breaks = fixef_intercepts,
            labels = parse(text = str_c("theta[", seq_along(fixef_intercepts), "]"))
        ) +
        scale_y_continuous(NULL, breaks = NULL, expand = expansion(mult = c(0, 0.05))) +
        coord_cartesian(xlim = c(-3, 5)) + 
        theme_minimal() +
        theme(text = element_text(size = fontsize, ))
    
    # ファイル保存
    if (!is.null(output_file)) {
        # 確率プロット
        png_file1 <- paste0(output_file, "_probability.png")
        ggsave(png_file1, plot = p1, units = "mm", width = 80, height = 60, dpi = 300)
        cat("Intercept probability plot saved to:", png_file1, "\n")
        
        # 閾値プロット
        png_file2 <- paste0(output_file, "_threshold.png")
        ggsave(png_file2, plot = p2, units = "mm", width = 100, height = 60, dpi = 300)
        cat("Intercept threshold plot saved to:", png_file2, "\n")
        
        # CSVで数値データも保存
        csv_file <- paste0(output_file, "_data.csv")
        write.csv(p1_summary, csv_file, row.names = FALSE)
        cat("Intercept data saved to:", csv_file, "\n")
    }
    
    return(list(probability_plot = p1, threshold_plot = p2, data = p1_summary))
}

# ==============================================================================
#  実行
# ==============================================================================

if (exists("current_model")) {
    cat("\n====== Creating Coefficient Plots for Q", current_iquest, " (", current_question_label, ") ======\n", sep="")
    
    # モデルに含まれるパラメータを確認してプロット対象を決定
    model_params <- variables(current_model)
    
    # 基本のプロット対象（Async, Position, Location, PrevExp, CogLoad, Duration, Age, Sex）
    # Index 1: Async, 2:7: Position~Age, 14: Gender
    plot_indices <- c(1, 2:7, 14) 
    
    # 交互作用も含める場合
    plot_indices <- c(plot_indices, 8:13)
    
    # Force_field が含まれる場合
    if ("b_Force_fieldYes" %in% model_params) {
        plot_indices <- c(plot_indices, 15, 16)
    }

    # Order が含まれる場合
    if ("b_Order2" %in% model_params) {
        plot_indices <- c(plot_indices, 17, 18)
    }
    
    # EHI が含まれる場合
    if ("b_EHI" %in% model_params) {
        plot_indices <- c(plot_indices, 19, 20)
    }
    
    # PDI が含まれる場合
    if ("b_PDI" %in% model_params) {
        plot_indices <- c(plot_indices, 21, 22)
    }

    # 主効果プロット（save_files = TRUEでoutputsフォルダに保存）
    main_plots <- generate_main_effect_plots(
        current_model, 
        indices = plot_indices,
        save_files = TRUE,
        output_dir = question_output_dir
    )
    
    # インターセプトプロット
    intercept_plots <- create_intercept_plots(
        current_model,
        colors = colors_paper,
        fontsize = fontsize_paper,
        rating_labels = levels(selectedData_current[[colnames(selectedData_current)[questionToColumn[current_iquest]]]]),
        output_file = file.path(question_output_dir, "Intercept")
    )
    
    # 条件付き効果プロット（Age）
    ce_age <- create_conditional_effects_plot(
        current_model, 
        effect = "Age",
        output_file = file.path(question_output_dir, "ConditionalEffects_Age")
    )
    
    # 条件付き効果プロット（各サブモデル用）
    sub_effects <- c("Force_field", "Order", "EHI", "PDI")
    for (eff in sub_effects) {
        # モデル式にその効果が含まれているかチェック
        if (any(grepl(eff, model_params))) {
            try({
                create_conditional_effects_plot(
                    current_model, 
                    effect = eff,
                    output_file = file.path(question_output_dir, paste0("ConditionalEffects_", eff))
                )
            }, silent = TRUE)
        }
    }
    
    cat("====== Coefficient Plots completed ======\n")
} else {
    cat("Warning: Model not found for question", current_iquest, ". Skipping coefficient plots.\n")
}
