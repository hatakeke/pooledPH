# ==============================================================================
#  11_additional_models.R - Additional Models (Order, EHI, PDI, Force)
# ==============================================================================
#  Summary: Building and executing sub-models with additional parameters.
#  Dependencies: 00_setup.R, utils_priors.R
#  Output: .rds files for each sub-model and analysis results.
# ==============================================================================

# Whether to run sub-models
use_sub_model <- TRUE
# use_sub_model <- FALSE

# Load utilities
source("utils_priors.R")

# ==============================================================================
#  Helper Function: Dynamic Formula Adjustment
# ==============================================================================

#' Adjust the regression formula based on the number of levels in the data
#' @param data Extracted dataset
#' @param base_formula_rhs String for the base formula (right side only)
#' @param response Name of the response variable
#' @return Adjusted formula object
adjust_formula_by_levels <- function(data, base_formula_rhs, response) {
    
    # Helper: Split items inside parentheses properly (ignoring + inside)
    split_terms <- function(x) {
        chars <- strsplit(x, "")[[1]]
        depth <- 0
        indices <- c(0)
        for (i in seq_along(chars)) {
            if (chars[i] == "(") depth <- depth + 1
            if (chars[i] == ")") depth <- depth - 1
            if (chars[i] == "+" && depth == 0) indices <- c(indices, i)
        }
        indices <- c(indices, length(chars) + 1)
        res <- c()
        for (j in 1:(length(indices)-1)) {
            term <- substr(x, indices[j]+1, indices[j+1]-1)
            res <- c(res, trimws(term))
        }
        res[res != ""]
    }
    
    terms <- split_terms(base_formula_rhs)
    valid_terms <- c()
    
    for (term in terms) {
        if (grepl("\\|", term)) {
            # Random effects "( slope | group )"
            inner <- gsub("^\\((.*)\\)$", "\\1", term)
            parts <- strsplit(inner, "\\|")[[1]]
            slope_part <- trimws(parts[1])
            group_part <- trimws(parts[2])
            
            # Check slopes as well
            slopes <- split_terms(slope_part)
            valid_slopes <- c()
            for (s in slopes) {
                if (s == "1") {
                    valid_slopes <- c(valid_slopes, s)
                    next
                }
                clean_s <- trimws(gsub("scale\\((.*)\\)", "\\1", s))
                if (clean_s %in% colnames(data)) {
                    if (length(unique(na.omit(data[[clean_s]]))) >= 2) {
                        valid_slopes <- c(valid_slopes, s)
                    }
                } else {
                    valid_slopes <- c(valid_slopes, s)
                }
            }
            if (length(valid_slopes) > 0) {
                valid_terms <- c(valid_terms, paste0("(", paste(valid_slopes, collapse = " + "), " | ", group_part, ")"))
            }
            
        } else if (grepl(":", term)) {
            # Interaction
            components <- strsplit(term, ":")[[1]]
            all_valid <- TRUE
            for (comp in components) {
                clean_comp <- trimws(gsub("scale\\((.*)\\)", "\\1", comp))
                if (clean_comp %in% colnames(data)) {
                    if (length(unique(na.omit(data[[clean_comp]]))) < 2) all_valid <- FALSE
                }
            }
            if (all_valid) valid_terms <- c(valid_terms, term)
        } else {
            # Single term
            clean_term <- trimws(gsub("scale\\((.*)\\)", "\\1", term))
            if (clean_term %in% colnames(data)) {
                if (length(unique(na.omit(data[[clean_term]]))) >= 2) {
                    valid_terms <- c(valid_terms, term)
                }
            } else {
                valid_terms <- c(valid_terms, term)
            }
        }
    }
    
    formula_str <- paste(response, "~", paste(valid_terms, collapse = " + "))
    return(as.formula(formula_str))
}

# ==============================================================================
#  Sub-model Definition (Order)
# ==============================================================================

#' Build model including Order (sequence effect)
#' @param data_orig Preprocessed data
#' @param iquest Question index
#' @param questionToColumn Question-to-column mapping
#' @param posterior_draws_path Path to posterior draws CSV for prior construction
#' @param current_output_dir Output directory
#' @return List (model, selectedData)
build_order_model <- function(data_orig, iquest, questionToColumn, posterior_draws_path, current_output_dir) {
    
    response_col <- colnames(data_orig)[questionToColumn[iquest]]
    
    # Predictor list
    predictors <- c("Condition", "Position", "Location", "Previous_Exposure", "Cognitive_Load", "Duration_sec", "Age", "Gender_IsMale", "Order")
    
    # Exclude NA (to match brms internal processing)
    selectedData <- data_orig %>% 
        dplyr::filter(if_all(all_of(c(response_col, predictors)), ~ !is.na(.)))

    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    # Base RHS formula
    base_formula_rhs <- "Condition + Position + Condition:Position + Location + Condition:Location + 
                         Previous_Exposure + Condition:Previous_Exposure + Cognitive_Load + Condition:Cognitive_Load + 
                         Duration_sec + Duration_sec:Condition + Age + Age:Condition + Gender_IsMale + 
                         Order + Order:Condition + (1+Condition|Experiment_ID)"
    
    # Adjust formula to avoid errors from insufficient levels
    formula_order <- adjust_formula_by_levels(selectedData, base_formula_rhs, response_col)

    family_type <- cumulative("probit", threshold = "flexible")

    cat("\n[module_sub] Building priors from main model for Order...\n")
    priors_order <- make_priors_from_posterior_draws(
        posterior_draws_path = posterior_draws_path,
        formula = formula_order,
        data = selectedData,
        family = family_type,
        output_dir = current_output_dir
    )
    
    model_order <- brm(
        formula_order, 
        data = selectedData,
        family = family_type,
        prior = priors_order,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13),
        init = 0
    )
    
    return(list(model = model_order, selectedData = selectedData))
}

# ==============================================================================
#  Sub-model Definition (EHI)
# ==============================================================================

#' Build model including EHI (Edinburgh Handedness Inventory)
#' @param data_orig Preprocessed data
#' @param iquest Question index
#' @param questionToColumn Question-to-column mapping
#' @param posterior_draws_path Path to posterior draws CSV for prior construction
#' @param current_output_dir Output directory
#' @return List (model, selectedData)
build_ehi_model <- function(data_orig, iquest, questionToColumn, posterior_draws_path, current_output_dir) {
    
    response_col <- colnames(data_orig)[questionToColumn[iquest]]
    
    # Predictor list
    predictors <- c("Condition", "Position", "Location", "Previous_Exposure", "Cognitive_Load", "Duration_sec", "Age", "Gender_IsMale", "EHI")
    
    # Exclude NA
    selectedData <- data_orig %>% 
        dplyr::filter(if_all(all_of(c(response_col, predictors)), ~ !is.na(.)))

    # Check levels of response variable
    if (length(unique(na.omit(selectedData[[response_col]]))) < 2) {
        cat("\nSkipping sub-model EHI: Response variable has only one level.\n")
        return(list(model = NULL, selectedData = selectedData))
    }

    selectedData$EHI <- (selectedData$EHI - mean(selectedData$EHI)) / sd(selectedData$EHI)
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    # Base RHS formula
    base_formula_rhs <- "Condition + Position + Condition:Position + Location + Condition:Location + 
                         Previous_Exposure + Condition:Previous_Exposure + Cognitive_Load + Condition:Cognitive_Load + 
                         Duration_sec + Duration_sec:Condition + Age + Age:Condition + Gender_IsMale + 
                         EHI + EHI:Condition + (1+Condition|Experiment_ID)"
    
    # Adjust formula to avoid errors from insufficient levels
    formula_ehi <- adjust_formula_by_levels(selectedData, base_formula_rhs, response_col)

    family_type <- cumulative("probit", threshold = "flexible")
    
    cat("\n[module_sub] Building priors from main model for EHI...\n")
    priors_ehi <- make_priors_from_posterior_draws(
        posterior_draws_path = posterior_draws_path,
        formula = formula_ehi,
        data = selectedData,
        family = family_type,
        output_dir = current_output_dir
    )
    
    model_ehi <- brm(
        formula_ehi, 
        data = selectedData,
        family = family_type,
        prior = priors_ehi,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13),
        init = 0
    )
    
    return(list(model = model_ehi, selectedData = selectedData))
}

# ==============================================================================
#  Sub-model Definition (PDI)
# ==============================================================================

#' Build model including PDI (Peters et al. Delusions Inventory)
#' @param data_orig Preprocessed data
#' @param iquest Question index
#' @param questionToColumn Question-to-column mapping
#' @param posterior_draws_path Path to posterior draws CSV for prior construction
#' @param current_output_dir Output directory
#' @return List (model, selectedData)
build_pdi_model <- function(data_orig, iquest, questionToColumn, posterior_draws_path, current_output_dir) {
    
    response_col <- colnames(data_orig)[questionToColumn[iquest]]
    
    # Predictor list
    predictors <- c("Condition", "Position", "Location", "Previous_Exposure", "Cognitive_Load", "Duration_sec", "Age", "Gender_IsMale", "PDI")
    
    # Exclude NA
    selectedData <- data_orig %>% 
        dplyr::filter(if_all(all_of(c(response_col, predictors)), ~ !is.na(.)))

    # Check levels of response variable
    if (length(unique(na.omit(selectedData[[response_col]]))) < 2) {
        cat("\nSkipping sub-model PDI: Response variable has only one level.\n")
        return(list(model = NULL, selectedData = selectedData))
    }

    selectedData$PDI <- (selectedData$PDI - mean(selectedData$PDI)) / sd(selectedData$PDI)
    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)
    
    # Base RHS formula
    base_formula_rhs <- "Condition + Position + Condition:Position + Location + Condition:Location + 
                         Previous_Exposure + Condition:Previous_Exposure + Cognitive_Load + Condition:Cognitive_Load + 
                         Duration_sec + Duration_sec:Condition + Age + Age:Condition + Gender_IsMale + 
                         PDI + PDI:Condition + (1+Condition|Experiment_ID)"
    
    # Adjust formula to avoid errors from insufficient levels
    formula_pdi <- adjust_formula_by_levels(selectedData, base_formula_rhs, response_col)
    
    family_type <- cumulative("probit", threshold = "flexible")
    
    cat("\n[module_sub] Building priors from main model for PDI...\n")
    priors_pdi <- make_priors_from_posterior_draws(
        posterior_draws_path = posterior_draws_path,
        formula = formula_pdi,
        data = selectedData,
        family = family_type,
        output_dir = current_output_dir
    )
    
    model_pdi <- brm(
        formula_pdi, 
        data = selectedData,
        family = family_type,
        prior = priors_pdi,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13), 
        init = 0
    )
    
    return(list(model = model_pdi, selectedData = selectedData))
}

# ==============================================================================
#  Sub-model Definition (Force)
# ==============================================================================

#' Build model including Force_field
build_force_model <- function(data_orig, iquest, questionToColumn, posterior_draws_path, current_output_dir) {

    response_col <- colnames(data_orig)[questionToColumn[iquest]]

    # Predictor list
    predictors <- c("Condition", "Position", "Location", "Previous_Exposure", "Cognitive_Load", "Duration_sec", "Age", "Gender_IsMale", "Force_field")

    # Exclude NA
    selectedData <- data_orig %>% 
        dplyr::filter(if_all(all_of(c(response_col, predictors)), ~ !is.na(.)))

    # Check levels of response variable
    if (length(unique(na.omit(selectedData[[response_col]]))) < 2) {
        cat("\nSkipping sub-model Force: Response variable has only one level.\n")
        return(list(model = NULL, selectedData = selectedData))
    }

    selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)

    # Base RHS formula
    base_formula_rhs <- "Condition + Position + Condition:Position + Location + Condition:Location + 
                         Previous_Exposure + Condition:Previous_Exposure + Cognitive_Load + Condition:Cognitive_Load + 
                         Duration_sec + Duration_sec:Condition + Age + Age:Condition + Gender_IsMale + 
                         Force_field + Force_field:Condition + (1+Condition|Experiment_ID)"

    # Adjust formula to avoid errors from insufficient levels
    formula_force <- adjust_formula_by_levels(selectedData, base_formula_rhs, response_col)

    family_force <- brms::cumulative("probit", threshold = "flexible")

    cat("\n[module_sub] Building priors from main model for Force...\n")
    priors_force <- make_priors_from_posterior_draws(
        posterior_draws_path = posterior_draws_path,
        formula = formula_force,
        data = selectedData,
        family = family_force,
        output_dir = current_output_dir
    )

    model_force <- brms::brm(
        formula_force,
        data = selectedData,
        family = family_force,
        prior = priors_force,
        warmup = 2000,
        iter = 8000,
        cores = 4,
        chains = 4,
        control = list(max_treedepth = 13),
        init = 0
    )

    return(list(model = model_force, selectedData = selectedData))
}

# ==============================================================================
#  Orchestration (Batch execution logic)
# ==============================================================================

if (use_sub_model) {
    
    # Definition of sub-models to analyze
    sub_models <- list(
        list(name = "Order", build_fn = build_order_model, data_check_col = "Order"),
        list(name = "EHI", build_fn = build_ehi_model, data_check_col = "EHI"),
        list(name = "PDI", build_fn = build_pdi_model, data_check_col = "PDI"),
        list(name = "Force", build_fn = build_force_model, data_check_col = "Force_field")
    )
    
    for (q_idx in seq_along(analysis_questions)) {
        current_iquest <- analysis_questions[q_idx]
        current_question_label_orig <- question_labels[q_idx]
        
        # Path to main model posterior (required as a prior)
        main_post_path <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label_orig), "posterior_draws_main_model.csv")
        
        if (!file.exists(main_post_path)) {
            cat("Warning: Main model posterior CSV not found for Q", current_iquest, ". Skipping sub-models.\n")
            next
        }
        
        for (sub in sub_models) {
            current_question_label <- paste0(current_question_label_orig, "_", sub$name)
            
            # Output directory
            question_output_dir <- file.path(output_dir, paste0("Q", current_iquest, "_", current_question_label))
            if (!dir.exists(question_output_dir)) dir.create(question_output_dir, recursive = TRUE)
            
            model_cache_path <- file.path(question_output_dir, paste0("model_", tolower(sub$name), ".rds"))
            
            # Check data existence (whether sub-model specific columns exist)
            # Find remaining rows after excluding NA
            sub_col <- sub$data_check_col
            valid_rows <- data_orig %>% 
                dplyr::filter(!is.na(.data[[sub_col]]), !is.na(Age))
            
            if (nrow(valid_rows) == 0) {
                cat("\nSkipping sub-model", sub$name, "for Q", current_iquest, "(No valid data for", sub_col, ")\n")
                next
            }
            
            # Skip estimation if cached
            if (use_model_cache && file.exists(model_cache_path)) {
                cat("\nLoading cached sub-model:", sub$name, "for Q", current_iquest, "\n")
                model_res <- list(
                    model = readRDS(model_cache_path),
                    selectedData = valid_rows
                )
                # Standardization for Age (replicate same process as during estimation)
                model_res$selectedData$Age <- (model_res$selectedData$Age - mean(model_res$selectedData$Age)) / sd(model_res$selectedData$Age)
            } else {
                cat("\nEstimating sub-model:", sub$name, "for Q", current_iquest, "...\n")
                model_res <- sub$build_fn(
                    data_orig = data_orig,
                    iquest = current_iquest,
                    questionToColumn = questionToColumn,
                    posterior_draws_path = main_post_path,
                    current_output_dir = question_output_dir
                )
                if (!is.null(model_res$model)) {
                    saveRDS(model_res$model, model_cache_path)
                    cat("Sub-model saved to:", model_cache_path, "\n")
                }
            }
            
            # Context settings for analysis scripts
            if (!is.null(model_res$model)) {
                current_model <- model_res$model
                selectedData_current <- model_res$selectedData
                
                # Execute various analyses
                cat("Running analysis for", current_question_label, "...\n")
                source("05_forest_plot.R")
                source("06_funnel_plot.R")
                source("07_posterior_check.R")
                source("08_convergence_diagnostics.R")
                source("09_coefficient_plots.R")
                source("10_rope_analysis.R")
            } else {
                cat("Skipping further analysis for", sub$name, "(Model estimation was skipped or failed)\n")
            }
        }
    }
} else {
    cat("====== Additional models module loaded (build functions only) ======\n")
    cat("Available: build_order_model(), build_ehi_model(), build_pdi_model(), build_force_model()\n")
}
