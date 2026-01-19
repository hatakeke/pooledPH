# ==============================================================================
#  test_modules.R - Module Test Script
# ==============================================================================
#  Summary: Syntax check for each module and verification of package dependencies.
# ==============================================================================

cat("
================================================================================
Starting Module Test
================================================================================
")

# ==============================================================================
# 1. Verify R Version
# ==============================================================================
cat("\n[1] Verify R Version\n")
cat("R version:", R.version.string, "\n")

# ==============================================================================
# 1.5 Set Library Path (Prioritize D:/R/library)
# ==============================================================================
lib_path <- "D:/R/library"
if (dir.exists(lib_path)) {
    .libPaths(c(lib_path, .libPaths()))
    cat("\n[1.5] Library Path Configuration\n")
    cat("Using library path:", lib_path, "\n")
} else {
    cat("\n[1.5] Library Path Configuration\n")
    cat("Note:", lib_path, "does not exist. Using default .libPaths().\n")
}

# ==============================================================================
# 2. Verify Package Installation Status
# ==============================================================================
cat("\n[2] Verify Package Installation Status\n")

required_packages <- c(
    "tidyverse",
    "readxl",
    "ggplot2",
    "brms",
    "tidybayes",
    "RColorBrewer",
    "ggmcmc",
    "svglite",
    "bayestestR",
    "extrafont",
    "crayon",
    "ordinal",
    "RVAideMemoire"
)

check_packages <- function(packages) {
    installed_names <- rownames(installed.packages())
    data.frame(
        Package = packages,
        Installed = packages %in% installed_names,
        stringsAsFactors = FALSE
    )
}

pkg_status <- check_packages(required_packages)
print(pkg_status)

missing_packages <- pkg_status$Package[!pkg_status$Installed]
if (length(missing_packages) > 0) {
    cat("\n⚠ Missing Packages:\n")
    cat(paste(" -", missing_packages, collapse = "\n"), "\n")
    cat("\nYou can install them with the following command:\n")
    cat(paste0('install.packages(c("', paste(missing_packages, collapse = '", "'), '"))\n'))
} else {
    cat("\n✓ All packages are installed\n")
}

# ==============================================================================
# 3. Syntax Check for Each Module
# ==============================================================================
cat("\n[3] Module Syntax Check\n")

module_files <- c(
    "00_setup.R",
    "01_main_model.R",
    "02_forest_plot.R",
    "03_funnel_plot.R",
    "04_posterior_check.R",
    "05_additional_models.R",
    "06_convergence_diagnostics.R",
    "07_coefficient_plots.R",
    "08_rope_analysis.R",
    "09_mediation_analysis.R",
    "10_simulation.R",
    "main.R"
)

# Estimate "the directory where this script resides" based on execution status
get_script_dir <- function() {
    if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
        ctx <- tryCatch(rstudioapi::getActiveDocumentContext(), error = function(e) NULL)
        if (!is.null(ctx) && nzchar(ctx$path)) {
            return(normalizePath(dirname(ctx$path), winslash = "/", mustWork = FALSE))
        }
    }

    cmd <- commandArgs(trailingOnly = FALSE)
    file_arg <- sub("^--file=", "", cmd[grep("^--file=", cmd)])
    if (length(file_arg) > 0 && nzchar(file_arg[1])) {
        return(normalizePath(dirname(file_arg[1]), winslash = "/", mustWork = FALSE))
    }

    ofile <- tryCatch(sys.frames()[[1]]$ofile, error = function(e) NULL)
    if (!is.null(ofile) && nzchar(ofile)) {
        return(normalizePath(dirname(ofile), winslash = "/", mustWork = FALSE))
    }

    normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

script_dir <- get_script_dir()

check_syntax <- function(file_path) {
    tryCatch({
        parse(file = file_path)
        return(list(success = TRUE, error = NULL))
    }, error = function(e) {
        return(list(success = FALSE, error = e$message))
    })
}

cat("\nModule Directory:", script_dir, "\n\n")

syntax_results <- data.frame(
    Module = character(),
    Status = character(),
    Error = character(),
    stringsAsFactors = FALSE
)

for (module in module_files) {
    file_path <- file.path(script_dir, module)
    if (file.exists(file_path)) {
        result <- check_syntax(file_path)
        if (result$success) {
            syntax_results <- rbind(syntax_results, data.frame(
                Module = module,
                Status = "✓ OK",
                Error = "",
                stringsAsFactors = FALSE
            ))
        } else {
            syntax_results <- rbind(syntax_results, data.frame(
                Module = module,
                Status = "✗ ERROR",
                Error = result$error,
                stringsAsFactors = FALSE
            ))
        }
    } else {
        syntax_results <- rbind(syntax_results, data.frame(
            Module = module,
            Status = "? NOT FOUND",
            Error = paste("File not found:", file_path),
            stringsAsFactors = FALSE
        ))
    }
}

print(syntax_results[, c("Module", "Status")])

# Detailed display if there are errors
errors <- syntax_results[syntax_results$Status == "✗ ERROR", ]
if (nrow(errors) > 0) {
    cat("\n⚠ Syntax Error Details:\n")
    for (i in 1:nrow(errors)) {
        cat(paste0("\n", errors$Module[i], ":\n"))
        cat(paste0("  ", errors$Error[i], "\n"))
    }
}

# ==============================================================================
# 4. Summary
# ==============================================================================
cat("\n")
cat("================================================================================\n")
cat("Test Results Summary\n")
cat("================================================================================\n")

ok_count <- sum(syntax_results$Status == "✓ OK")
error_count <- sum(syntax_results$Status == "✗ ERROR")
notfound_count <- sum(syntax_results$Status == "? NOT FOUND")

cat(paste0("Syntax Check OK: ", ok_count, "/", length(module_files), "\n"))
cat(paste0("Syntax Errors: ", error_count, "\n"))
cat(paste0("Files Not Found: ", notfound_count, "\n"))
cat(paste0("Missing Packages: ", length(missing_packages), "\n"))

if (error_count == 0 && notfound_count == 0 && length(missing_packages) == 0) {
    cat("\n✓ All tests passed!\n")
} else {
    cat("\n⚠ There are some issues. Please check above.\n")
}

cat("\n================================================================================\n")
