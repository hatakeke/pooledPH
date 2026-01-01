# ==============================================================================
#  07_coefficient_plots.R - 係数プロット
# ==============================================================================
#  概要: 各効果の係数推定値の可視化
#  依存: 00_setup.R, 01_main_model.R, 05_additional_models.R
#  出力: 各種PNGファイル、Coefficient_summary.csv
# ==============================================================================

library(ggplot2)
library(tidybayes)
library(extrafont)

# ==============================================================================
#  プロット設定
# ==============================================================================

# 図のファイル名定義
figure_names <- c(
    "PH_Async.png",
    "PH_BodyPosition.png",
    "PH_BodyLocation.png",
    "PH_Previousexposure.png",
    "PH_CognitiveLoad.png",
    "PH_Duration.png",
    "PH_ForceField.png",
    "PH_Age.png",
    "PH_Async_BodyPosition.png",
    "PH_Async_BodyLocation.png",
    "PH_Async_PreviousExposure.png",
    "PH_Async_CognitiveLoad.png",
    "PH_Async_Duration.png",
    "PH_Async_ForceField.png",
    "PH_Async_Age.png",
    "PH_SexAtBirth.png"
)

# ベータ係数名
betas <- c(
    "b_ConditionAsync", 
    "b_PositionLaying", 
    "b_LocationHand",
    "b_Previous_ExposureRobotmanipulation", 
    "b_Cognitive_LoadYes", 
    "b_Duration_sec", 
    "b_Force_fieldYes",
    "b_Age",
    "b_ConditionAsync:PositionLaying",
    "b_ConditionAsync:LocationHand",
    "b_ConditionAsync:Previous_ExposureRobotmanipulation", 
    "b_ConditionAsync:Cognitive_LoadYes",
    "b_ConditionAsync:Duration_sec",
    "b_ConditionAsync:Force_fieldYes",
    "b_ConditionAsync:Age",
    "b_Gender_IsMaleMale"
)

# X軸タイトル
xtitles <- c(
    "Asynchrony\n(async)", 
    "Body Position\n(supine)",
    "Body Location\n(hand)",
    "Previous Exposure\n(+)",
    "Cognitive Load\n(+)", 
    "Duration\n(increasing)",
    "Force Field\n(+)",
    "Age\n(increasing in age)",
    "Asynchrony : Body Position\n(Async, supine)",
    "Asynchrony : Body Location\n(Async, hand)",
    "Asynchrony : Prev. Exp.\n(Async, +)", 
    "Asynchrony : Cog. Load\n(Async, +)",
    "Asynchrony : Duration\n(Async, Increasing)",
    "Asynchrony : Force Field\n(Async, +)",
    "Asynchrony : Age\n(Async, Increasing)",
    "Sex at birth\n(Male)"
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

#' フォレストプロット生成（複数係数を1枚に）
#' @param model brmsモデルオブジェクト
#' @param indices プロットする効果のインデックス
#' @param colors 色設定
#' @param fontsize フォントサイズ
#' @param output_file 出力ファイル名（拡張子なし）
#' @return ggplotオブジェクト
create_forest_plot <- function(
    model, 
    indices = c(1:16),
    colors = c("#d4bbff", "#4589ff"),
    fontsize = 7,
    output_file = NULL
    ) {
    
    post <- posterior_samples(model) %>%
        mutate(iter = 1:n())
    
    # 複数係数のデータを集約
    forest_data <- tibble()
    
    for (i in indices) {
        vals <- post[[betas[i]]]
        summary_df <- tibble(
            effect = xtitles[i],
            effect_label = paste0(i),
            median = median(vals),
            lower_89 = quantile(vals, 0.055),
            upper_89 = quantile(vals, 0.945),
            lower_66 = quantile(vals, 0.17),
            upper_66 = quantile(vals, 0.83)
        )
        forest_data <- bind_rows(forest_data, summary_df)
    }
    
    # Y軸の順序を逆にして下から上へ効果を表示
    forest_data <- forest_data %>%
        mutate(effect = factor(effect, levels = rev(xtitles[indices])))
    
    # フォレストプロット
    p <- ggplot(forest_data, aes(x = median, y = effect)) +
        geom_linerange(
            aes(xmin = lower_89, xmax = upper_89), 
            color = colors[1], 
            linewidth = 0.8
        ) +
        geom_linerange(
            aes(xmin = lower_66, xmax = upper_66), 
            color = colors[1], 
            linewidth = 1.5
        ) +
        geom_point(color = colors[2], size = 2) +
        geom_vline(xintercept = 0, linetype = "solid", color = "black", linewidth = 0.5) +
        geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed", color = "gray50", linewidth = 0.5) +
        theme_minimal() + 
        theme(
            panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            axis.title.y = element_blank(),
            text = element_text(size = fontsize),
            plot.margin = margin(5, 5, 5, 5, "mm")
        ) +
        xlab("Posterior estimate")
    
    if (!is.null(output_file)) {
        # 係数数に応じて図のサイズを調整
        n_effects <- length(indices)
        height <- 20 + n_effects * 8
        
        png_file <- paste0(output_file, ".png")
        ggsave(png_file, plot = p, units = "mm", width = 100, height = height, dpi = 300)
        cat("Forest plot saved to:", png_file, "\n")
    }
    
    return(p)
}

#' 主効果のバッチプロット生成
#' @param model brmsモデルオブジェクト
#' @param indices プロットする効果のインデックス
#' @param save_files ファイル保存するか
generate_main_effect_plots <- function(
    model, 
    indices = c(2:7),
    save_files = FALSE
    ) {
    
    post <- posterior_samples(model) %>%
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
        
        if (save_files) {
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
    output_file = NULL
    ) {
    
    post_predictions <- posterior_samples(model) %>%
        select(`b_Intercept[1]`:`b_Intercept[6]`) %>%
        mutate(iter = 1:n())
    
    # 確率分布プロット
    p1_data <- post_predictions %>% 
        select(-iter) %>% 
        mutate_all(.funs = ~pnorm(., 0, 1)) %>% 
        transmute(
            `p[Q7==0]` = `b_Intercept[1]`,
            `p[Q7==1]` = `b_Intercept[2]` - `b_Intercept[1]`,
            `p[Q7==2]` = `b_Intercept[3]` - `b_Intercept[2]`,
            `p[Q7==3]` = `b_Intercept[4]` - `b_Intercept[3]`,
            `p[Q7==4]` = `b_Intercept[5]` - `b_Intercept[4]`,
            `p[Q7==5]` = `b_Intercept[6]` - `b_Intercept[5]`,
            `p[Q7==6]` = 1 - `b_Intercept[6]`
        ) %>% 
        set_names(0:6) %>% 
        pivot_longer(everything(), names_to = "Q7", values_to = "value")
    
    # 手動で統計量を計算
    p1_summary <- p1_data %>%
        group_by(Q7) %>%
        summarise(
            median = median(value),
            lower_89 = quantile(value, 0.055),
            upper_89 = quantile(value, 0.945),
            lower_66 = quantile(value, 0.17),
            upper_66 = quantile(value, 0.83),
            .groups = "drop"
        )
    
    p1 <- ggplot(p1_summary, aes(x = median, y = Q7)) +
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
    p2 <- tibble(x = seq(from = -3.5, to = 3.5, by = .01)) %>%
        mutate(d = dnorm(x)) %>% 
        ggplot(aes(x = x, ymin = 0, ymax = d)) +
        geom_ribbon(fill = "black") +
        geom_vline(
            xintercept = fixef(model)[1:6, 1], 
            color = colors[1], 
            linetype = 2, 
            size = 0.75
        ) +
        scale_x_continuous(
            "Posterior modes for the rating scale intercepts",
            breaks = fixef(model)[1:6, 1],
            labels = parse(text = str_c("theta[", 1:6, "]"))
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

if (exists("model_fullExperimentalParameters_dem")) {
    cat("\n====== Creating Coefficient Plots ======\n")
    
    # ===== フォレストプロット（全係数を1枚に） =====
    forest_all <- create_forest_plot(
        model_fullExperimentalParameters_dem,
        indices = c(1:16),
        colors = colors_paper,
        fontsize = fontsize_paper,
        output_file = file.path(output_dir, "ForestPlot_AllCoefficients")
    )
    
    # ===== フォレストプロット（主効果のみ） =====
    forest_main <- create_forest_plot(
        model_fullExperimentalParameters_dem,
        indices = c(1:8),
        colors = colors_paper,
        fontsize = fontsize_paper,
        output_file = file.path(output_dir, "ForestPlot_MainEffects")
    )
    
    # ===== フォレストプロット（交互作用のみ） =====
    forest_interaction <- create_forest_plot(
        model_fullExperimentalParameters_dem,
        indices = c(9:16),
        colors = colors_paper,
        fontsize = fontsize_paper,
        output_file = file.path(output_dir, "ForestPlot_Interactions")
    )
    
    # 主効果プロット（save_files = TRUEでoutputsフォルダに保存）
    # indices: 2=Position, 3=Location, 4=Previous_Exposure, 5=Cognitive_Load, 
    #          6=Duration, 7=Force_field, 8=Age
    main_plots <- generate_main_effect_plots(
        model_fullExperimentalParameters_dem, 
        indices = c(2:8),
        save_files = TRUE
    )
    
    # インターセプトプロット
    intercept_plots <- create_intercept_plots(
        model_fullExperimentalParameters_dem,
        colors = colors_paper,
        fontsize = fontsize_paper,
        output_file = file.path(output_dir, "Intercept")
    )
    
    # 条件付き効果プロット（Age）
    ce_age <- create_conditional_effects_plot(
        model_fullExperimentalParameters_dem, 
        effect = "Age",
        output_file = file.path(output_dir, "ConditionalEffects_Age")
    )
    
    # 条件付き効果プロット（Force_field）
    ce_forcefield <- create_conditional_effects_plot(
        model_fullExperimentalParameters_dem, 
        effect = "Force_field",
        output_file = file.path(output_dir, "ConditionalEffects_ForceField")
    )
    
    cat("====== Coefficient Plots completed ======\n")
} else {
    cat("Warning: Model not found. Please run 01_main_model.R first.\n")
}
