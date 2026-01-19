# ==============================================================================
#  utils_priors.R - Utility for converting posterior draw CSV -> brms priors
# ==============================================================================
#  Purpose:
#    Read posterior draws (CSV from posterior_samples) from the main model
#    and construct informative priors to be used in sub-models.
# ==============================================================================

library(readr)
library(dplyr)
library(brms)

#' Read posterior draw data
read_posterior_draws <- function(path) {
    if (!file.exists(path)) {
        stop(paste0("Posterior draws CSV not found: ", path))
    }
    readr::read_csv(path, show_col_types = FALSE)
}

.find_first_existing <- function(candidates, columns) {
    for (nm in candidates) {
        if (nm %in% columns) return(nm)
    }
    NULL
}

.param_column_for_prior_row <- function(draw_cols, class, coef = NA_character_, group = NA_character_) {
    if (is.na(class) || class == "") return(NULL)

    if (class == "b") {
        if (is.na(coef) || coef == "") return(NULL)
        return(paste0("b_", coef))
    }

    if (class == "Intercept") {
        if (is.na(coef) || coef == "") return(NULL)
        k <- coef
        candidates <- c(
            paste0("b_Intercept[", k, "]"),
            paste0("Intercept[", k, "]"),
            paste0("b_Intercept_", k),
            paste0("Intercept_", k)
        )
        return(.find_first_existing(candidates, draw_cols))
    }

    if (class == "sd") {
        if (is.na(group) || group == "" || is.na(coef) || coef == "") return(NULL)
        return(paste0("sd_", group, "__", coef))
    }

    NULL
}

#' Create priors from posterior distributions (filtering version matching model formula)
make_priors_from_posterior_draws <- function(
    posterior_draws_path,
    formula,
    data,
    family,
    output_dir = "outputs",
    fallback_b = "normal(0,5)",
    fallback_intercept = "normal(0,5)",
    fallback_sd = "student_t(3, 0, 2)"
) {
    # Read posterior draws from the main model
    draws <- read_posterior_draws(posterior_draws_path)
    draw_cols <- colnames(draws)

    # Get the list of parameters currently required by the sub-model
    # (Uses the formula adjusted by adjust_formula_by_levels)
    current_priors <- brms::get_prior(formula, data = data, family = family)
    
    priors_list <- list()
    audit_log <- list()

    for (i in 1:nrow(current_priors)) {
        p_row <- current_priors[i, ]
        p_class <- p_row$class
        p_coef  <- p_row$coef
        p_group <- p_row$group
        
        # Skip priors for entire models or specific groups (those that are not individual coefficients)
        if (p_coef == "" && p_group == "" && p_class != "Intercept") next
        if (p_class == "cor") next # Correlation parameters are omitted this time
        
        # Identify corresponding column name in the CSV
        col_name <- .param_column_for_prior_row(draw_cols, p_class, p_coef, p_group)
        
        if (!is.null(col_name) && col_name %in% draw_cols) {
            # If posterior distribution exists: construct prior using its statistics
            val_vec <- draws[[col_name]]
            
            if (p_class == "sd") {
                # sd is approximated with lognormal
                est_meanlog <- mean(log(val_vec), na.rm = TRUE)
                est_sdlog <- sd(log(val_vec), na.rm = TRUE)
                dist <- paste0("lognormal(", round(est_meanlog, 4), ",", round(est_sdlog, 4), ")")
            } else {
                # b and Intercept are approximated with normal
                est_mean <- mean(val_vec, na.rm = TRUE)
                est_sd <- sd(val_vec, na.rm = TRUE)
                dist <- paste0("normal(", round(est_mean, 4), ",", round(est_sd, 4), ")")
            }
            
            priors_list[[length(priors_list) + 1]] <- brms::set_prior(dist, class = p_class, coef = p_coef, group = p_group)
            
            audit_log[[length(audit_log) + 1]] <- data.frame(
                class = p_class, coef = p_coef, group = p_group, 
                prior = dist, used_posterior = TRUE
            )
        } else {
            # If not present in posterior (e.g., sub-model specific variables): use weakly informative priors
            dist <- switch(p_class,
                           "b" = fallback_b,
                           "Intercept" = fallback_intercept,
                           "sd" = fallback_sd,
                           "normal(0,10)") # default
            
            priors_list[[length(priors_list) + 1]] <- brms::set_prior(dist, class = p_class, coef = p_coef, group = p_group)
            
            audit_log[[length(audit_log) + 1]] <- data.frame(
                class = p_class, coef = p_coef, group = p_group, 
                prior = dist, used_posterior = FALSE
            )
        }
    }

    # Save audit log
    audit_df <- do.call(rbind, audit_log)
    write.csv(audit_df, file.path(output_dir, "priors_audit.csv"), row.names = FALSE)

    return(do.call(c, priors_list))
}
