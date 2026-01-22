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
#  表示用ラベルとファイル名のマッピング定義
# ==============================================================================

# パラメータ成分ごとの表示用ラベル
term_labels <- c(
    "ConditionSync" = "Synchrony\n(Sync vs Async)",
    "ConditionAsync" = "Asynchrony\n(Async vs Sync)",
    "DeviceforMRI" = "Device: for MRI\n(vs Default)",
    "DeviceforHand" = "Device: for Hand\n(vs Default)",
    "DeviceWearable" = "Device: Wearable\n(vs Default)",
    "Previous_ExposureRobotmanipulation" = "Prev. Exp.: Robot\n(vs None)",
    "Cognitive_LoadYes" = "Cog. Load\n(Yes vs No)",
    "Duration_sec" = "Duration\n(std.)",
    "Age" = "Age\n(std.)",
    "Gender_IsMaleMale" = "Gender: Male\n(vs Female)",
    "Force_fieldYes" = "Force Field\n(Yes vs No)",
    "Order2" = "Order\n(2nd vs 1st)",
    "EHI" = "EHI (L->R)",
    "PDI" = "PDI"
)

#' パラメータ名から表示用ラベルを取得する
get_pretty_label <- function(beta_name) {
    # 'b_' 接頭辞を削除
    clean_name <- gsub("^b_", "", beta_name)
    
    # Intercept[n] の処理（nが含まれる場合のみ）
    if (grepl("Intercept\\[\\d+\\]", clean_name)) {
        num <- gsub("Intercept\\[(\\d+)\\]", "\\1", clean_name)
        return(paste0("Threshold ", num, "\n(", (as.numeric(num)-1), " vs ", num, ")"))
    } else if (clean_name == "Intercept") {
        return("Global Intercept")
    }
    
    # 交互作用（:）で分割して、それぞれの成分を変換
    parts <- unlist(strsplit(clean_name, ":"))
    pretty_parts <- sapply(parts, function(p) {
        if (p %in% names(term_labels)) term_labels[p] else p
    })
    
    # 結合（交互作用の場合は ":" で繋ぐ）
    return(paste(pretty_parts, collapse = " :\n"))
}

#' パラメータ名から安全なファイル名を生成する
get_safe_filename <- function(beta_name) {
    # 'b_' を削除し、特殊文字を置換
    name <- gsub("^b_", "Effect_", beta_name)
    name <- gsub("\\[", "_", name)
    name <- gsub("\\]", "", name)
    name <- gsub(":", "_x_", name)
    return(paste0(name, ".png"))
}

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
    
    # パラメータが存在しないか、空の場合の処理
    if (is.null(vals) || length(vals) == 0) {
        cat("Warning: parameter", beta_name, "is empty or missing in draws.\n")
        return(NULL)
    }

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
            text = element_text(size = fontsize)
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
#' @param save_files ファイル保存するか
#' @param output_dir 保存先ディレクトリ
generate_main_effect_plots <- function(
    model, 
    save_files = FALSE,
    output_dir = NULL
    ) {
    
    # 修正：モデル内の全ベータ係数（b_）を自動取得
    model_params <- variables(model)
    betas_in_model <- model_params[grep("^b_", model_params)]
    
    # as_draws_df を使用して事後分布を取得
    post <- as_draws_df(model)
    
    plots <- list()
    
    for (beta_name in betas_in_model) {
        # 表示用ラベルとファイル名の生成
        x_title <- get_pretty_label(beta_name)
        file_name <- get_safe_filename(beta_name)

        p <- create_coefficient_plot(
            post, 
            beta_name, 
            x_title,
            colors = colors_paper,
            fontsize = fontsize_paper
        )
        
        if (is.null(p)) next
        
        plots[[beta_name]] <- p
        
        if (save_files && !is.null(output_dir)) {
            out_file <- file.path(output_dir, file_name)
            
            # 条件付きのサイズ調整（交互作用などラベルが長い場合）
            is_interaction <- grepl(":", beta_name)
            w <- if (is_interaction) 45 else 35
            h <- 40
            
            ggsave(out_file, plot = p, units = "mm", width = w, height = h, dpi = 300)
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
            `p[Score==0]` = `b_Intercept[1]`,
            `p[Score==1]` = `b_Intercept[2]` - `b_Intercept[1]`,
            `p[Score==2]` = `b_Intercept[3]` - `b_Intercept[2]`,
            `p[Score==3]` = `b_Intercept[4]` - `b_Intercept[3]`,
            `p[Score==4]` = `b_Intercept[5]` - `b_Intercept[4]`,
            `p[Score==5]` = `b_Intercept[6]` - `b_Intercept[5]`,
            `p[Score==6]` = 1 - `b_Intercept[6]`
        ) %>% 
        set_names(0:6) %>% 
        pivot_longer(everything(), names_to = "Score", values_to = "value")
    
    # 手動で統計量を計算
    p1_summary <- p1_data %>%
        group_by(Score) %>%
        summarise(
            median = median(value),
            lower_89 = quantile(value, 0.055),
            upper_89 = quantile(value, 0.945),
            lower_66 = quantile(value, 0.17),
            upper_66 = quantile(value, 0.83),
            .groups = "drop"
        )
    
    p1 <- ggplot(p1_summary, aes(x = median, y = Score)) +
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

if (exists("current_model")) {
    cat("\n====== Creating Coefficient Plots for Q", current_iquest, " (", current_question_label, ") ======\n", sep="")
    
    # 主効果プロット（save_files = TRUEでoutputsフォルダに保存）
    # モデル内の全母数を自動検出してプロット
    main_plots <- generate_main_effect_plots(
        current_model, 
        save_files = TRUE,
        output_dir = question_output_dir
    )
    
    # モデルに含まれる全てのパラメータ名を取得
    model_params <- variables(current_model)
    
    # インターセプトプロット
    intercept_plots <- create_intercept_plots(
        current_model,
        colors = colors_paper,
        fontsize = fontsize_paper,
        output_file = file.path(question_output_dir, "Intercept")
    )
    
    # モデルに含まれる説明変数を取得
    # fixef(model) の行名からInterceptを除いたものをベースにする
    fixed_effects_names <- rownames(fixef(current_model))
    
    # 条件付き効果プロット（モデルに含まれている変数を自動判定）
    # Age や Duration_sec などの数値変数
    for (v in c("Age", "Duration_sec")) {
        if (any(grepl(v, fixed_effects_names))) {
            try({
                create_conditional_effects_plot(
                    current_model, 
                    effect = v,
                    output_file = file.path(question_output_dir, paste0("ConditionalEffects_", v))
                )
            }, silent = TRUE)
        }
    }
    
    # 条件付き効果プロット（各サブモデル用）
    sub_effects <- c("Force_field", "Order", "EHI", "PDI")
    for (eff in sub_effects) {
        if (any(grepl(eff, fixed_effects_names))) {
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
