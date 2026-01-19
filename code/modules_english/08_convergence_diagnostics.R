# ==============================================================================
#  08_convergence_diagnostics.R - Convergence Diagnostics
# ==============================================================================
#  Summary: Convergence diagnostics for MCMC chains (e.g., Caterpillar plots).
#  Dependencies: 00_setup.R, 01_main_model.R
#  Output: Caterpillar.png, Rhat_diagnostics.csv, ESS_diagnostics.csv 
#          (saved in question-specific directories)
# ==============================================================================

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
#  Caterpillar Plot Function
# ==============================================================================

#' Create Caterpillar plots (trace plots)
#' @param model brms model object
#' @param params Indices of parameters to plot
#' @param y_limits Range for the Y-axis (default: c(-3, 3))
#' @param burnin Vertical line position for the burn-in period
#' @param output_file Output filename (without extension)
#' @return ggplot object
create_caterpillar_plot <- function(
    model, 
    params = c(4, 5, 6), 
    y_limits = c(-3, 3),
    burnin = 1000,
    output_file = NULL
    ) {
    
    # Convert model using ggmcmc
    modelADPT_full <- ggs(model)
    
    # Get parameters
    betas <- unique(modelADPT_full$Parameter)
    
    # Filter only selected parameters
    selected_params <- betas[params]
    
    p <- ggplot(
        filter(modelADPT_full, Parameter %in% selected_params),
        aes(x = Iteration, y = value, col = as.factor(Chain))
    ) +
        geom_line() +
        scale_y_continuous(limits = y_limits) + 
        geom_vline(xintercept = burnin) +
        facet_grid(Parameter ~ ., scale = 'free_y', switch = 'y') +
        labs(
            title = "Caterpillar Plots", 
            col = "Chains"
        ) +
        theme_minimal()
    
    # Save file
    if (!is.null(output_file)) {
        png_file <- paste0(output_file, ".png")
        ggsave(png_file, plot = p, units = "mm", width = 150, height = 100, dpi = 300)
        cat("Caterpillar plot saved to:", png_file, "\n")
    }
    
    return(p)
}

#' Convert model to ggs data for all parameters
#' @param model brms model object
#' @return ggmcmc converted data
get_ggs_data <- function(model) {
    ggs(model)
}

#' Summary of R-hat convergence diagnostics
#' @param model brms model object
#' @param output_file Output filename (without extension)
#' @return Summary table
check_rhat <- function(model, output_file = NULL) {
    summary_df <- as.data.frame(summary(model)$fixed)
    
    if ("Rhat" %in% colnames(summary_df)) {
        rhat_check <- summary_df[, c("Estimate", "Est.Error", "Rhat")]
        rhat_check$converged <- rhat_check$Rhat < 1.1
        rhat_check$Parameter <- rownames(rhat_check)
        
        if (!is.null(output_file)) {
            csv_file <- paste0(output_file, ".csv")
            write.csv(rhat_check, csv_file, row.names = FALSE)
            cat("R-hat diagnostics saved to:", csv_file, "\n")
        }
        
        return(rhat_check)
    } else {
        cat("Rhat column not found in model summary.\n")
        return(summary(model)$fixed)
    }
}

#' Check effective sample size
#' @param model brms model object
#' @param output_file Output filename (without extension)
#' @return Summary
check_effective_sample_size <- function(model, output_file = NULL) {
    summary_df <- as.data.frame(summary(model)$fixed)
    
    if ("Bulk_ESS" %in% colnames(summary_df)) {
        ess_check <- summary_df[, c("Estimate", "Bulk_ESS", "Tail_ESS")]
        ess_check$adequate_bulk <- ess_check$Bulk_ESS > 400
        ess_check$adequate_tail <- ess_check$Tail_ESS > 400
        ess_check$Parameter <- rownames(ess_check)
        
        if (!is.null(output_file)) {
            csv_file <- paste0(output_file, ".csv")
            write.csv(ess_check, csv_file, row.names = FALSE)
            cat("ESS diagnostics saved to:", csv_file, "\n")
        }
        
        return(ess_check)
    } else {
        cat("ESS columns not found. Model may use different naming convention.\n")
        return(summary(model)$fixed)
    }
}

# ==============================================================================
#  Execution
# ==============================================================================

if (exists("current_model")) {
    cat("\n====== Running Convergence Diagnostics for Q", current_iquest, " (", current_question_label, ") ======\n", sep="")
    
    # Caterpillar plots
    caterpillar <- create_caterpillar_plot(
        current_model, 
        params = c(4, 5, 6),
        output_file = file.path(question_output_dir, "Caterpillar")
    )
    
    # R-hat check
    cat("\n--- R-hat Diagnostics ---\n")
    rhat_results <- check_rhat(current_model, output_file = file.path(question_output_dir, "Rhat_diagnostics"))
    
    # Effective sample size check
    cat("\n--- Effective Sample Size Diagnostics ---\n")
    ess_results <- check_effective_sample_size(current_model, output_file = file.path(question_output_dir, "ESS_diagnostics"))
    
    cat("\n====== Convergence Diagnostics completed ======\n")
} else {
    cat("Warning: Model not found for question", current_iquest, ". Skipping convergence diagnostics.\n")
}
