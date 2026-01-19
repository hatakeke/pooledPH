# ==============================================================================
#  10_rope_analysis.R - ROPE Analysis
# ==============================================================================
#  Summary: HDI-ROPE (Region of Practical Equivalence) comparative analysis.
#  Dependencies: 00_setup.R, 01_main_model.R, 11_additional_models.R
#  Output: ROPE_analysis.csv, ROPE_summary.csv 
#          (saved in question-specific directories)
# ==============================================================================

library(bayestestR)

# ==============================================================================
#  Output Directory Configuration per Question
# ==============================================================================
if (!exists("current_question_label")) {
    stop("Error: current_question_label not defined. This script must be called from main.R with question context.")
}

question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label))

# ==============================================================================
#  ROPE Analysis Functions
# ==============================================================================

#' Execute ROPE Analysis
#' @param model brms model object
#' @param rope_range ROPE range (default: c(-0.1, 0.1))
#' @param ci Credible interval (default: 1)
#' @return ROPE analysis result
run_rope_analysis <- function(
    model, 
    rope_range = c(-0.1, 0.1), 
    ci = 1
    ) {
    
    result <- rope(model, range = rope_range, ci = ci)
    return(result)
}

#' Compare ROPE Across Multiple Models
#' @param models Named list of models
#' @param rope_range ROPE range
#' @param ci Credible interval
#' @return List of comparison results
compare_rope_multiple <- function(
    models, 
    rope_range = c(-0.1, 0.1), 
    ci = 1
    ) {
    
    results <- lapply(names(models), function(name) {
        cat("\n--- ROPE Analysis for", name, "---\n")
        result <- run_rope_analysis(models[[name]], rope_range, ci)
        print(result)
        return(list(name = name, result = result))
    })
    
    return(results)
}

#' Create Summary for ROPE Results
#' @param rope_result result from rope()
#' @param output_file Output filename (without extension)
#' @return Summary dataframe
summarize_rope <- function(rope_result, output_file = NULL) {
    
    df <- as.data.frame(rope_result)
    
    # Interpretation of practical equivalence
    df$interpretation <- ifelse(
        df$ROPE_Percentage > 97.5, "Practically equivalent to zero",
        ifelse(
            df$ROPE_Percentage < 2.5, 
            "Practically different from zero",
            "Undecided"
        )
    )
    
    # Save CSV
    if (!is.null(output_file)) {
        csv_file <- paste0(output_file, ".csv")
        write.csv(df, csv_file, row.names = FALSE)
        cat("ROPE summary saved to:", csv_file, "\n")
    }
    
    return(df)
}

#' Evaluation of Practical Significance for an Effect
#' @param post Posterior samples dataframe
#' @param param_name Parameter name
#' @param rope_range ROPE range
#' @return Evaluation result
evaluate_practical_significance <- function(
    post, 
    param_name, 
    rope_range = c(-0.1, 0.1)
    ) {
    
    samples <- post[[param_name]]
    
    # Percentage within ROPE
    in_rope <- mean(samples > rope_range[1] & samples < rope_range[2])
    
    # Percentage above ROPE (positive direction)
    above_rope <- mean(samples >= rope_range[2])
    
    # Percentage below ROPE (negative direction)
    below_rope <- mean(samples <= rope_range[1])
    
    result <- list(
        parameter = param_name,
        mean = mean(samples),
        sd = sd(samples),
        hdi_89 = bayestestR::hdi(samples, ci = 0.89),
        in_rope_pct = in_rope * 100,
        above_rope_pct = above_rope * 100,
        below_rope_pct = below_rope * 100,
        interpretation = ifelse(
            in_rope > 0.975, 
            "Practically equivalent to zero",
            ifelse(
                in_rope < 0.025, 
                "Practically different from zero",
                "Undecided"
            )
        )
    )
    
    return(result)
}

# ==============================================================================
#  Execution
# ==============================================================================

if (exists("current_model")) {
    cat("\n====== Running ROPE Analysis for Q", current_iquest, " (", current_question_label, ") ======\n", sep="")
    
    # ROPE analysis for main model
    cat("\n--- Main Model ROPE Analysis ---\n")
    rope_main <- run_rope_analysis(current_model)
    print(rope_main)
    
    # Summary (saves as CSV)
    rope_summary <- summarize_rope(rope_main, output_file = file.path(question_output_dir, "ROPE_summary"))
    cat("\n--- ROPE Summary with Interpretation ---\n")
    print(rope_summary[, c("Parameter", "ROPE_Percentage", "interpretation")])
    
    cat("\n====== ROPE Analysis completed ======\n")
} else {
    cat("Warning: Model not found for question", current_iquest, ". Skipping ROPE analysis.\n")
}
