# ==============================================================================
#  05_forest_plot.R - Forest Plot
# ==============================================================================
#  Summary: Visualizes random effects per experiment using a forest plot.
#  Dependencies: 00_setup.R, 01_main_model.R
#  Output: ForestPlot.png, ForestPlot_data.csv (saved in question-specific directories)
# ==============================================================================

# Load setup and model if needed
# source("./R/pooledPH/code/modules/01_main_model.R")

library(tidyverse)
library(tidybayes)

# ==============================================================================
#  Output Directory Configuration per Question
# ==============================================================================
# Note: current_question_label, current_model, and selectedData_current
#       are provided by the loop in main.R
if (!exists("current_question_label")) {
    stop("Error: current_question_label not defined. This script must be called from main.R with question context.")
}

question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label))

# ==============================================================================
#  Forest Plot Creation Function
# ==============================================================================

#' Create a Forest Plot
#' @param model brms model object
#' @param colors Color settings (vector of 2 colors)
#' @param output_file Output filename (without extension)
#' @return ggplot object
create_forest_plot <- function(
    model, 
    colors = c("#d4bbff", "#4589ff"),
    output_file = "ForestPlot"
    ) {
    
    # Extract draws from posterior distribution
    A <- spread_draws(model, b_ConditionAsync, r_Experiment_ID[Experiment_ID, ])
    
    # Add grand mean to group-specific deviations
    B <- mutate(A, mu = b_ConditionAsync + r_Experiment_ID)
    
    C <- ungroup(B)
    
    D <- mutate(C, outcome = str_replace_all(Experiment_ID, "[.]", " "))
    
    # Calculate summary statistics (instead of median_qi)
    D_summary <- D %>%
        group_by(outcome) %>%
        summarise(
            median = median(mu),
            lower_89 = quantile(mu, 0.055),
            upper_89 = quantile(mu, 0.945),
            lower_66 = quantile(mu, 0.17),
            upper_66 = quantile(mu, 0.83),
            .groups = "drop"
        )
    
    # Create plot (supports ggplot2 4.0: uses geom_pointrange)
    p <- ggplot(D_summary, aes(x = median, y = reorder(outcome, median))) +
        geom_vline(xintercept = 0, linetype = "dotdash", color = "black") +
        geom_vline(xintercept = -0.5, linetype = "dotdash", color = "grey") +
        geom_vline(xintercept = 0.5, linetype = "dotdash", color = "grey") +
        geom_vline(xintercept = -1.0, linetype = "dotdash", color = "grey") +
        geom_vline(xintercept = 1.0, linetype = "dotdash", color = "grey") +
        geom_linerange(aes(xmin = lower_89, xmax = upper_89), color = colors[1], linewidth = 0.8) +
        geom_linerange(aes(xmin = lower_66, xmax = upper_66), color = colors[1], linewidth = 1.5) +
        geom_point(color = colors[2], size = 1.5) +
        scale_x_continuous(limits = c(-1.25, 1.3)) +
        labs(
            x = expression(italic("as Cohen's d")),
            y = NULL
        ) +
        theme_minimal() +
        theme(
            panel.grid = element_blank(),
            axis.ticks.y = element_blank(),
            axis.text.y = element_text(hjust = 0.95),
            text = element_text(size = 7)
        )
    
    # Save files
    if (!is.null(output_file)) {
        # Save PNG
        png_file <- paste0(output_file, ".png")
        ggsave(png_file, plot = p, units = "mm", width = 90, height = 130, dpi = 300)
        cat("Forest plot saved to:", png_file, "\n")
        
        # Save CSV (numeric data)
        csv_file <- paste0(output_file, "_data.csv")
        write.csv(D_summary, csv_file, row.names = FALSE)
        cat("Forest plot data saved to:", csv_file, "\n")
    }
    
    return(list(plot = p, data = D_summary))
}

# ==============================================================================
#  Execution
# ==============================================================================

# Execute only if model exists
if (exists("current_model")) {
    cat("\n====== Creating Forest Plot for Q", current_iquest, " (", current_question_label, ") ======\n", sep="")
    
    forest_result <- create_forest_plot(
        current_model,
        colors = colors_paper,
        output_file = file.path(question_output_dir, "ForestPlot")
    )
    forest_plot <- forest_result$plot
    
    cat("====== Forest Plot completed ======\n")
} else {
    cat("Warning: Model not found for question", current_iquest, ". Skipping forest plot.\n")
}
