# ==============================================================================
#  02_export_posterior.R - CSV Export of Posterior Distributions (Supports Multiple Questions)
# ==============================================================================
#  Summary:
#    - Saves posterior draws and summaries for all parameters from models calculated
#      in 01_main_model.R into CSV files.
#  Dependencies: 00_setup.R, 01_main_model.R
# ==============================================================================

source("00_setup.R")

#' Function to export posterior distributions to CSV
#' @param model brms model object
#' @param output_dir Output directory
#' @param file_stub Filename stub
#' @return List of saved file paths
export_posterior_to_csv <- function(model, output_dir, file_stub = "main_model") {
    if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
    }

    post_draws <- brms::posterior_samples(model) %>%
        dplyr::mutate(iter = seq_len(n()))

    draws_path <- file.path(output_dir, paste0("posterior_draws_", file_stub, ".csv"))
    readr::write_csv(post_draws, draws_path)
    cat("Posterior draws saved to:", draws_path, "\n")

    sum_mat <- brms::posterior_summary(model, probs = c(0.055, 0.5, 0.945))
    sum_df <- as.data.frame(sum_mat)
    sum_df$parameter <- rownames(sum_df)
    rownames(sum_df) <- NULL
    sum_df <- sum_df %>%
        dplyr::select(parameter, dplyr::everything())

    summary_path <- file.path(output_dir, paste0("posterior_summary_", file_stub, ".csv"))
    readr::write_csv(sum_df, summary_path)
    cat("Posterior summary saved to:", summary_path, "\n")

    invisible(list(draws_path = draws_path, summary_path = summary_path))
}

# ==============================================================================
#  Export Posterior Distributions for Multiple Questions
# ==============================================================================

cat("\n====== Exporting posteriors for all questions ======\n")

for (idx in seq_along(analysis_questions)) {
    question_label <- question_labels[idx]
    
    cat("\n--- Exporting", question_label, "---\n")
    
    # Output directory per question
    question_output_dir <- file.path(output_dir, paste0("Q", analysis_questions[idx], "_", question_label))
    
    # Get model
    model <- models_all[[question_label]]
    
    # Export to CSV
    export_posterior_to_csv(model, output_dir = question_output_dir, file_stub = "main_model")
}

cat("\n====== Export completed ======\n")
