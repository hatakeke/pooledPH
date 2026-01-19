# ==============================================================================
#  06_funnel_plot.R - Funnel Plot
# ==============================================================================
#  Summary: Funnel plots for evaluating publication bias.
#  Dependencies: 00_setup.R, 01_main_model.R
#  Output: FunnelPlot_Intercept.png, FunnelPlot_Slope.png, FunnelPlot_data.csv 
#          (saved in question-specific directories)
# ==============================================================================

library(dplyr)
library(ggplot2)
library(ggmcmc)

# ==============================================================================
#  Output Directory Configuration per Question
# ==============================================================================
if (!exists("current_question_label")) {
    stop("Error: current_question_label not defined. This script must be called from main.R with question context.")
}

question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label))

# ==============================================================================
# Funnel Plot Creation Function (Intercept)
# ==============================================================================

#' Create a funnel plot for intercepts
#' @param model brms model object
#' @param output_file Output filename (without extension)
#' @return list(plot, data)
create_funnel_plot_intercept <- function(model, output_file = NULL) {
    
    # Extract random effects
    ranef_df <- as.data.frame(ranef(model)$Experiment_ID[, , "Intercept"]) %>%
        mutate(Experiment_ID = rownames(ranef(model)$Experiment_ID))
    
    # Calculate Precision
    ranef_df <- ranef_df %>%
        mutate(Precision = 1 / Est.Error)
    
    # Funnel shading data (expected 95% CI around zero)
    se_range <- seq(min(ranef_df$Est.Error), max(ranef_df$Est.Error), length.out = 100)
    funnel_df <- data.frame(
        SE = se_range,
        Precision = 1 / se_range,
        Lower = -1.96 * se_range,
        Upper = 1.96 * se_range
    )
    
    # Create plot
    p <- ggplot(ranef_df, aes(x = Estimate, y = Precision)) +
        geom_ribbon(
            data = funnel_df,
            aes(xmin = Lower, xmax = Upper, y = Precision),
            inherit.aes = FALSE,
            fill = "grey80",
            alpha = 0.5
        ) +
        geom_point(alpha = 0.7) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
        labs(
            title = "Funnel Plot Analogue (Random Intercepts)",
            x = "Deviation from Overall Intercept (Latent Scale)",
            y = "Precision (1 / SE)"
        ) +
        theme_minimal() +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )
    
    # Save files
    if (!is.null(output_file)) {
        png_file <- paste0(output_file, ".png")
        ggsave(png_file, plot = p, units = "mm", width = 120, height = 80, dpi = 300)
        cat("Funnel plot (Intercept) saved to:", png_file, "\n")
        
        csv_file <- paste0(output_file, "_data.csv")
        write.csv(ranef_df, csv_file, row.names = FALSE)
        cat("Funnel plot (Intercept) data saved to:", csv_file, "\n")
    }
    
    return(list(plot = p, data = ranef_df))
}

# ==============================================================================
#  Funnel Plot Creation Function (Condition Slope)
# ==============================================================================

#' Create a funnel plot for condition slopes
#' @param model brms model object
#' @param output_file Output filename (without extension)
#' @return list(plot, data)
create_funnel_plot_slope <- function(model, output_file = NULL) {
    
    # Extract random slopes
    slope_df <- as.data.frame(ranef(model)$Experiment_ID[, , "ConditionAsync"]) %>%
        mutate(Experiment_ID = rownames(.)) %>%
        rename(Estimate = Estimate, SE = Est.Error) %>%
        mutate(Precision = 1 / SE)
    
    # Create funnel shape
    se_range <- seq(min(slope_df$SE), max(slope_df$SE), length.out = 100)
    funnel <- data.frame(
        SE = se_range,
        Precision = 1 / se_range,
        Lower = -1.96 * se_range,
        Upper = 1.96 * se_range
    )
    
    # Create plot
    p <- ggplot(slope_df, aes(x = Estimate, y = Precision)) +
        geom_ribbon(
            data = funnel,
            aes(xmin = Lower, xmax = Upper, y = Precision),
            inherit.aes = FALSE,
            fill = "grey80", 
            alpha = 0.5
        ) +
        geom_point(alpha = 0.8) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
        labs(
            title = "Funnel Plot Analogue: Random Slopes for Condition (Async vs. Sync)",
            x = "Random Slope Estimate for ConditionAsync",
            y = "Precision (1 / SE)"
        ) +
        theme_minimal() +
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
        )
    
    # Save files
    if (!is.null(output_file)) {
        png_file <- paste0(output_file, ".png")
        ggsave(png_file, plot = p, units = "mm", width = 120, height = 80, dpi = 300)
        cat("Funnel plot (Slope) saved to:", png_file, "\n")
        
        csv_file <- paste0(output_file, "_data.csv")
        write.csv(slope_df, csv_file, row.names = FALSE)
        cat("Funnel plot (Slope) data saved to:", csv_file, "\n")
    }
    
    return(list(plot = p, data = slope_df))
}

# ==============================================================================
#  Execution
# ==============================================================================

if (exists("current_model")) {
    cat("\n====== Creating Funnel Plots for Q", current_iquest, " (", current_question_label, ") ======\n", sep="")
    
    # Funnel plot for Intercept
    funnel_intercept <- create_funnel_plot_intercept(current_model, output_file = file.path(question_output_dir, "FunnelPlot_Intercept"))
    
    # Funnel plot for Condition Slope
    funnel_slope <- create_funnel_plot_slope(current_model, output_file = file.path(question_output_dir, "FunnelPlot_Slope"))
    
    cat("====== Funnel Plots completed ======\n")
} else {
    cat("Warning: Model not found for question", current_iquest, ". Skipping funnel plots.\n")
}
