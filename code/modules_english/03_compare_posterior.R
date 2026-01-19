# ==============================================================================
#  03_compare_posterior.R - Comparative Analysis of Posterior Distributions (PH vs Control)
# ==============================================================================
#  Summary: Compares pooled posterior distributions of PH and Control models,
#           outputting graphs and CSVs.
#  Features: Consolidates all categories into a single image (no splitting). 
#            Chain prefix issue resolved.
#  Dependencies: 00_setup.R, 01_main_model.R
# ==============================================================================

source("00_setup.R")
library(ggplot2)
library(dplyr)
library(stringr)
library(brms)

cat("\n====== Comparing posteriors: PH vs Control (Single Plot per Category) ======\n")

# Output directory configuration
comparison_output_dir <- file.path(output_dir, "Comparison_PH_vs_Control")
if (!dir.exists(comparison_output_dir)) {
    dir.create(comparison_output_dir, recursive = TRUE)
}

# ==============================================================================
#  Function Definitions: Data Extraction and Cleaning
# ==============================================================================

#' Extract posterior distributions from model and return a dataframe with fully integrated chains
get_flat_draws <- function(model) {
    # Use as_draws_matrix to flatten chain information (prevents Xn_ prefix)
    draws_mat <- as_draws_matrix(model)
    draws_df <- as.data.frame(draws_mat)
    
    # Exclude unnecessary metadata columns
    cols_to_remove <- c("lprior", "lp__", ".chain", ".iteration", ".draw")
    draws_df <- draws_df %>% select(-any_of(cols_to_remove))
    
    return(draws_df)
}

# ==============================================================================
#  Main Process: Data Preparation
# ==============================================================================

cat("Extracting posterior samples (flattening chains)...\n")

draws_ph <- get_flat_draws(models_all[["PH"]])
draws_ctrl <- get_flat_draws(models_all[["Control"]])

# Identify common parameters
common_params <- intersect(colnames(draws_ph), colnames(draws_ctrl))

if (length(common_params) == 0) {
    stop("No common parameters found between PH and Control models.")
}

cat("  Common parameters found:", length(common_params), "\n")

# ==============================================================================
#  Statistic Calculation and CSV Creation
# ==============================================================================

cat("Calculating differences and statistics...\n")

stats_list <- list()

for (param in common_params) {
    val_ph <- draws_ph[[param]]
    val_ctrl <- draws_ctrl[[param]]
    
    # Adjust sample size
    n_sample <- min(length(val_ph), length(val_ctrl))
    val_ph_s <- sample(val_ph, n_sample)
    val_ctrl_s <- sample(val_ctrl, n_sample)
    
    # Distribution of difference (PH - Control)
    diff_vals <- val_ph_s - val_ctrl_s
    
    # 89% Credible Interval (CI)
    ci_diff <- quantile(diff_vals, probs = c(0.055, 0.945))
    
    # Determine if it contains zero
    contains_zero <- (ci_diff[1] < 0) & (ci_diff[2] > 0)
    
    stats_list[[param]] <- data.frame(
        Parameter = param,
        PH_Mean = mean(val_ph_s),
        Control_Mean = mean(val_ctrl_s),
        Diff_Mean = mean(diff_vals),
        Diff_Median = median(diff_vals),
        Diff_Lower_89 = ci_diff[1],
        Diff_Upper_89 = ci_diff[2],
        Diff_Contains_Zero = contains_zero,
        row.names = NULL
    )
}

comparison_df <- do.call(rbind, stats_list)

# Save CSV
csv_path <- file.path(comparison_output_dir, "posterior_comparison_ph_vs_control.csv")
readr::write_csv(comparison_df, csv_path)
cat("Saved CSV:", csv_path, "\n")

# ==============================================================================
#  Visualization Function Definitions
# ==============================================================================

# Categorize parameters
classify_parameter <- function(param_name) {
    if (str_detect(param_name, "^b_Intercept")) return("Threshold")
    if (str_detect(param_name, "^Intercept")) return("Intercept_Raw") 
    if (str_detect(param_name, "^b_")) return("Fixed_Effect")
    if (str_detect(param_name, "^sd_|^cor_|^r_")) return("Random_Effect")
    return("Other")
}

# Create data for plotting
make_plot_data <- function(params_subset) {
    plot_data_list <- list()
    for (p in params_subset) {
        d_p <- rbind(
            data.frame(Value = draws_ph[[p]], Group = "PH", Parameter = p),
            data.frame(Value = draws_ctrl[[p]], Group = "Control", Parameter = p)
        )
        plot_data_list[[p]] <- d_p
    }
    do.call(rbind, plot_data_list)
}

# Function to create and save density plots (consolidated into one image)
save_density_plot <- function(params, title_suffix, filename) {
    if (length(params) == 0) return(NULL)
    
    cat("  Drawing:", title_suffix, " (", length(params), " parameters)...\n")
    
    plot_data <- make_plot_data(params)
    
    # Calculate layout
    n_cols <- 3  # Fixed at 3 columns
    n_rows <- ceiling(length(params) / n_cols)
    
    # Automatic height calculation: 50mm per row, minimum 150mm
    plot_height <- max(150, n_rows * 50)
    
    p <- ggplot(plot_data, aes(x = Value, fill = Group, color = Group)) +
        geom_density(alpha = 0.4) +
        facet_wrap(~Parameter, scales = "free", ncol = n_cols) +
        scale_fill_manual(values = c("PH" = "#4589ff", "Control" = "#d4bbff")) +
        scale_color_manual(values = c("PH" = "#0051ba", "Control" = "#8b7ba8")) +
        theme_minimal() +
        labs(title = paste("Posterior Density:", title_suffix),
             y = "Density", x = "Value") +
        theme(legend.position = "top")
    
    # Save image with limitsize = FALSE for large files
    ggsave(file.path(comparison_output_dir, filename), p, 
           width = 210, height = plot_height, units = "mm", dpi = 300, 
           limitsize = FALSE)
}

# ==============================================================================
#  Execute Visualization: Density Plots (by Category)
# ==============================================================================

cat("Generating density plots...\n")

all_params <- comparison_df$Parameter
param_types <- sapply(all_params, classify_parameter)

# Batch draw each category
save_density_plot(all_params[param_types == "Threshold"], 
                 "Threshold Parameters", "01_Density_Threshold_Parameters.png")

save_density_plot(all_params[param_types == "Fixed_Effect"], 
                 "Fixed Effect Parameters", "02_Density_Fixed_Effect_Parameters.png")

save_density_plot(all_params[param_types == "Intercept_Raw"], 
                 "Intercept Parameters", "03_Density_Intercept_Parameters.png")

save_density_plot(all_params[param_types == "Random_Effect"], 
                 "Random Effect Parameters", "04_Density_Random_Effect_Parameters.png")

# ==============================================================================
#  Execute Visualization: CI Difference Plots
# ==============================================================================

cat("Generating CI difference plots...\n")

# 5. Batch plot for all parameters (excluding r_)
main_params <- comparison_df %>% 
    filter(!str_detect(Parameter, "^r_")) %>%
    arrange(Diff_Median)

if (nrow(main_params) > 0) {
    cat("  Drawing: All Parameters Difference...\n")
    
    p_ci <- ggplot(main_params, aes(x = reorder(Parameter, Diff_Median), y = Diff_Median, color = Diff_Contains_Zero)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_pointrange(aes(ymin = Diff_Lower_89, ymax = Diff_Upper_89)) +
        coord_flip() +
        scale_color_manual(values = c("FALSE" = "#0051ba", "TRUE" = "gray70"), 
                           labels = c("Significant", "Not Sig")) +
        theme_minimal() +
        labs(title = "Difference (PH - Control) [All Parameters]",
             subtitle = "89% CI excludes zero for blue items",
             x = "Parameter", y = "Difference")
    
    # Adjust height based on number of parameters (6mm per item + margin)
    h_ci <- max(150, 20 + nrow(main_params) * 6)
    
    ggsave(file.path(comparison_output_dir, "05_CI_Difference_All.png"), 
           p_ci, width = 180, height = h_ci, units = "mm", dpi = 300, 
           limitsize = FALSE)
}

# 6. Significant parameters only
sig_df <- comparison_df %>% 
    filter(Diff_Contains_Zero == FALSE) %>%
    arrange(Diff_Median)

if (nrow(sig_df) > 0) {
    cat("  Drawing: Significant Differences...\n")
    
    p_sig <- ggplot(sig_df, aes(x = reorder(Parameter, Diff_Median), y = Diff_Median)) +
        geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
        geom_pointrange(aes(ymin = Diff_Lower_89, ymax = Diff_Upper_89), color = "#d14141") +
        coord_flip() +
        theme_minimal() +
        labs(title = "Significant Differences Only (PH - Control)",
             subtitle = "Parameters where 89% CI does not include 0",
             x = "Parameter", y = "Difference")
    
    h_sig <- max(100, 10 + nrow(sig_df)*6)
    
    ggsave(file.path(comparison_output_dir, "06_CI_Difference_Significant.png"), 
           p_sig, width = 180, height = h_sig, units = "mm", dpi = 300, 
           limitsize = FALSE)
} else {
    cat("  (No significant differences found. Skipping 06.)\n")
}

# ==============================================================================
#  Closing Summary
# ==============================================================================

cat("\n================================================================\n")
cat("Analysis Complete. Files are saved in:", comparison_output_dir, "\n")
cat("All plots have been consolidated into single files per category.\n")
cat("================================================================\n")
