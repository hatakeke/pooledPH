# ==============================================================================
#  09_coefficient_plots.R - Coefficient Plots
# ==============================================================================
#  Summary: Visualizes coefficient estimates for each effect.
#  Dependencies: 00_setup.R, 01_main_model.R, 11_additional_models.R
#  Output: Various PNG files, Coefficient_summary.csv 
#          (saved in question-specific directories)
# ==============================================================================

library(ggplot2)
library(tidybayes)
library(extrafont)

# ==============================================================================
#  Output Directory Configuration per Question
# ==============================================================================
if (!exists("current_question_label")) {
    stop("Error: current_question_label not defined. This script must be called from main.R with question context.")
}

question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label))

# ==============================================================================
#  Plot Configuration
# ==============================================================================

# Figure filename definitions
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

# Beta coefficient names
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

# X-axis titles
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
#  Coefficient Plot Function
# ==============================================================================

#' Create a Half-eye plot for a single coefficient
#' @param post Posterior samples dataframe
#' @param beta_name Coefficient name
#' @param x_title X-axis title
#' @param colors Color settings
#' @param fontsize Font size
#' @param rope_range ROPE range
#' @return ggplot object
create_coefficient_plot <- function(
    post, 
    beta_name, 
    x_title,
    colors = c("#d4bbff", "#4589ff"),
    fontsize = 7,
    rope_range = 0.1
    ) {
    
    # Calculate statistics manually
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

#' Create a plot for interaction coefficients
#' @param post Posterior samples dataframe    
#' @param interaction_var Interaction variable name
#' @param x_title X-axis title
#' @param colors Color settings
#' @param fontsize Font size
#' @return ggplot object
create_interaction_plot <- function(
    post, 
    interaction_var,
    x_title,
    colors = c("#d4bbff", "#4589ff"),
    fontsize = 7
    ) {
    
    # Calculate statistics manually
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

#' Generate batch plots for main effects
#' @param model brms model object
#' @param indices Indices of effects to plot
#' @param save_files Whether to save files
#' @param output_dir Output directory
#' @return List of ggplot objects
generate_main_effect_plots <- function(
    model, 
    indices = c(2:7),
    save_files = FALSE,
    output_dir = NULL
    ) {
    
    post <- brms::posterior_samples(model) %>%
        dplyr::mutate(iter = 1:n())
    
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

#' Conditional effects plot
#' @param model brms model object
#' @param effect Effect name
#' @param output_file Output filename
#' @return ggplot object
create_conditional_effects_plot <- function(
    model, 
    effect = "Age",
    output_file = NULL
    ) {
    
    ce <- conditional_effects(model, categorical = TRUE, effects = effect)
    ce_plot <- plot(ce, plot = FALSE)[[1]]  # Prevent rendering to device
    
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
#  Intercept Plots
# ==============================================================================

#' Create intercept distribution plots
#' @param model brms model object
#' @param colors Color settings
#' @param fontsize Font size
#' @param output_file Output filename (without extension)
#' @return List of ggplot objects
create_intercept_plots <- function(
    model, 
    colors = c("#d4bbff", "#4589ff"),
    fontsize = 7,
    output_file = NULL
    ) {
    
    post_predictions <- brms::posterior_samples(model) %>%
        dplyr::select(`b_Intercept[1]`:`b_Intercept[6]`) %>%
        dplyr::mutate(iter = 1:n())
    
    # Probability distribution plot
    p1_data <- post_predictions %>% 
        dplyr::select(-iter) %>% 
        dplyr::mutate_all(.funs = ~pnorm(., 0, 1)) %>% 
        dplyr::transmute(
            `p[Q7==0]` = `b_Intercept[1]`,
            `p[Q7==1]` = `b_Intercept[2]` - `b_Intercept[1]`,
            `p[Q7==2]` = `b_Intercept[3]` - `b_Intercept[2]`,
            `p[Q7==3]` = `b_Intercept[4]` - `b_Intercept[3]`,
            `p[Q7==4]` = `b_Intercept[5]` - `b_Intercept[4]`,
            `p[Q7==5]` = `b_Intercept[6]` - `b_Intercept[5]`,
            `p[Q7==6]` = 1 - `b_Intercept[6]`
        ) %>% 
        set_names(0:6) %>% 
        tidyr::pivot_longer(everything(), names_to = "Q7", values_to = "value")
    
    # Calculate statistics manually
    p1_summary <- p1_data %>%
        dplyr::group_by(Q7) %>%
        dplyr::summarise(
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
    
    # Normal distribution + threshold plot
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
    
    # Save files
    if (!is.null(output_file)) {
        # Probability plot
        png_file1 <- paste0(output_file, "_probability.png")
        ggsave(png_file1, plot = p1, units = "mm", width = 80, height = 60, dpi = 300)
        cat("Intercept probability plot saved to:", png_file1, "\n")
        
        # Threshold plot
        png_file2 <- paste0(output_file, "_threshold.png")
        ggsave(png_file2, plot = p2, units = "mm", width = 100, height = 60, dpi = 300)
        cat("Intercept threshold plot saved to:", png_file2, "\n")
        
        # Save numeric data in CSV
        csv_file <- paste0(output_file, "_data.csv")
        write.csv(p1_summary, csv_file, row.names = FALSE)
        cat("Intercept data saved to:", csv_file, "\n")
    }
    
    return(list(probability_plot = p1, threshold_plot = p2, data = p1_summary))
}

# ==============================================================================
#  Execution
# ==============================================================================

if (exists("current_model")) {
    cat("\n====== Creating Coefficient Plots for Q", current_iquest, " (", current_question_label, ") ======\n", sep="")
    
    # Confirm parameters included in the model to determine plotting targets
    model_params <- brms::variables(current_model)
    
    # Targets for basic plots (Async, Position, Location, PrevExp, CogLoad, Duration, Age, Sex)
    # Index 1: Async, 2:7: Position~Age, 14: Gender
    plot_indices <- c(1, 2:7, 14) 
    
    # Include interactions
    plot_indices <- c(plot_indices, 8:13)
    
    # If Force_field is included
    if ("b_Force_fieldYes" %in% model_params) {
        plot_indices <- c(plot_indices, 15, 16)
    }

    # If Order is included
    if ("b_Order2" %in% model_params) {
        plot_indices <- c(plot_indices, 17, 18)
    }
    
    # If EHI is included
    if ("b_EHI" %in% model_params) {
        plot_indices <- c(plot_indices, 19, 20)
    }
    
    # If PDI is included
    if ("b_PDI" %in% model_params) {
        plot_indices <- c(plot_indices, 21, 22)
    }

    # Main effect plots (saves to outputs folder when save_files = TRUE)
    main_plots <- generate_main_effect_plots(
        current_model, 
        indices = plot_indices,
        save_files = TRUE,
        output_dir = question_output_dir
    )
    
    # Intercept plots
    intercept_plots <- create_intercept_plots(
        current_model,
        colors = colors_paper,
        fontsize = fontsize_paper,
        output_file = file.path(question_output_dir, "Intercept")
    )
    
    # Conditional effects plot (Age)
    ce_age <- create_conditional_effects_plot(
        current_model, 
        effect = "Age",
        output_file = file.path(question_output_dir, "ConditionalEffects_Age")
    )
    
    # Conditional effects plots (for each sub-model)
    sub_effects <- c("Force_field", "Order", "EHI", "PDI")
    for (eff in sub_effects) {
        # Check if the effect is in the model formula
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
