# ==============================================================================
#  01_main_model.R - Main Full Model (Supports Multiple Questions)
# ==============================================================================
#  Summary: Definition and execution of full model using brms (supports multiple questions)
#  Dependencies: 00_setup.R
#  Output: models_all (List of models for multiple questions)
# ==============================================================================

# Load setup
source("00_setup.R")

# Whether to skip estimation (use pre-estimated models)
# use_model_cache <- FALSE
use_model_cache <- TRUE   

# ==============================================================================
#  Main Full Model Definition
# ==============================================================================

#' Build and run the full model
#' @param data_orig Preprocessed data
#' @param iquest Question index
#' @param questionToColumn Mapping of questions to columns
#' @return brms model object
build_main_model <- function(data_orig, iquest, questionToColumn) {
    
    # Exclude missing age (approx. 4 subjects)
    selectedData <- filter(data_orig, !is.na(Age))
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    # Formula for the full model
    formula_full_dem <- as.formula(paste(
        colnames(data_orig)[questionToColumn[iquest]],
        "~ Condition +
        Position + Condition:Position +
        Location + Condition:Location +
        Previous_Exposure + Condition:Previous_Exposure +
        Cognitive_Load + Condition:Cognitive_Load +
        Duration_sec + Condition:Duration_sec +
        Age + Age:Condition +
        Gender_IsMale + (1+Condition|Experiment_ID)"
    ))
    
    print(formula_full_dem)
    print(paste0("Selected question ", iquest, ": ", questionsDescription[iquest]))
    
    # Run brms model
    model_fullExperimentalParameters_dem <- brm(
        formula_full_dem, 
        data = selectedData,
        family = cumulative("probit", threshold = "flexible"),
        prior = c(
            set_prior("normal(0,5)", class = "b"),
            set_prior("student_t(3, 0, 2)", class = "sd")
        ),
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13), 
        init = 0
    )
    
    return(list(
        model = model_fullExperimentalParameters_dem,
        selectedData = selectedData
    ))
}

# ==============================================================================
#  Model for SoA (Sense of Agency) (Alternative Formula)
# ==============================================================================

#' Build model for SoA (Sense of Agency)
#' @description Model excluding Cognitive_Load and Location
build_soa_model <- function(data_orig, iquest, questionToColumn) {
    
    selectedData <- filter(data_orig, !is.na(Age))
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    formula_soa <- as.formula(paste(
        colnames(data_orig)[questionToColumn[iquest]],
        "~ Condition + 
        Position + Condition:Position + 
        Previous_Exposure + Condition:Previous_Exposure +
        Duration_sec + Condition:Duration_sec +
        Age + Age:Condition +
        Gender_IsMale + (1+Condition|Experiment_ID)"
    ))
    
    model_soa <- brm(
        formula_soa, 
        data = selectedData,
        family = cumulative("probit", threshold = "flexible"),
        prior = c(
            set_prior("normal(0,5)", class = "b"),
            set_prior("student_t(3, 0, 2)", class = "sd")
        ),
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13), 
        init = 0
    )
    
    return(list(
        model = model_soa,
        selectedData = selectedData
    ))
}

# ==============================================================================
#  Run Model
# ==============================================================================

cat("\n====== Building models for multiple questions ======\n")

# Save models for multiple questions
models_all <- list()
selectedData_all <- list()

# If use_complete_pairs_for_comparison is TRUE, extract data valid for both questions beforehand
if (use_complete_pairs_for_comparison) {
    cat("\n[NA HANDLING] Strict mode: Using only rows with valid data for BOTH Q1 and Q7\n")
    
    # Column indices for both questions
    q_cols <- questionToColumn[analysis_questions]
    q_col_names <- colnames(data_orig)[q_cols]
    
    # Get rows non-NA in both questions
    data_for_analysis <- data_orig %>%
        filter(!is.na(data_orig[[q_col_names[1]]]) & !is.na(data_orig[[q_col_names[2]]]))
    
    cat("  Original data:", nrow(data_orig), "rows\n")
    cat("  After removing NA in Q1 & Q7:", nrow(data_for_analysis), "rows\n")
    cat("  Lost:", nrow(data_orig) - nrow(data_for_analysis), "rows\n\n")
} else {
    cat("\n[NA HANDLING] Lenient mode: Each question uses available data\n")
    cat("  Q1 (Control) and Q7 (PH) may have different sample sizes\n\n")
    data_for_analysis <- data_orig
}

# Process multiple questions in analysis_questions loop
for (idx in seq_along(analysis_questions)) {
    current_iquest <- analysis_questions[idx]
    question_label <- question_labels[idx]
    
    cat("\n--- Processing question", current_iquest, ":", questionsDescription[current_iquest], "---\n")
    
    # Output directory per question
    question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", question_label))
    if (!dir.exists(question_output_dir)) {
        dir.create(question_output_dir, recursive = TRUE)
        cat("Created output directory:", question_output_dir, "\n")
    }
    
    # Model cache path (per question)
    model_cache_path <- file.path(question_output_dir, "model_main.rds")
    
    if (file.exists(model_cache_path) && use_model_cache) {
        cat("Found cached model. Loading:", model_cache_path, "\n")
        model_fullExperimentalParameters_dem <- readRDS(model_cache_path)
        
        # Re-create selectedData
        selectedData <- filter(data_for_analysis, !is.na(Age))
        selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    } else {
        # Run main model (this takes time)
        result <- build_main_model(data_for_analysis, current_iquest, questionToColumn)
        model_fullExperimentalParameters_dem <- result$model
        selectedData <- result$selectedData
        
        # Save cache
        saveRDS(model_fullExperimentalParameters_dem, model_cache_path)
        cat("Cached model saved to:", model_cache_path, "\n")
    }
    
    # Save to list
    models_all[[question_label]] <- model_fullExperimentalParameters_dem
    selectedData_all[[question_label]] <- selectedData
    
    cat("\n========== Model summary for Q", current_iquest, " (", question_label, ") ==========\n", sep="")
    print(summary(model_fullExperimentalParameters_dem))
}

# Global variables for main use (default values: PH)
model <- models_all[["PH"]]
model_fullExperimentalParameters_dem <- model
selectedData <- selectedData_all[["PH"]]
model_Control <- models_all[["Control"]]
selectedData_Control <- selectedData_all[["Control"]]

cat("\n====== All models completed ======\n")
cat("[Info] Posterior CSV export is handled by export_posterior.R\n")
cat("[Info] Posterior comparison is handled by compare_posterior.R\n")
