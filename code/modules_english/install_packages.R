# ==============================================================================
#  install_packages.R - Package Installation Script
# ==============================================================================

cat("================================================================================\n")
cat("Starting installation of required packages\n")
cat("================================================================================\n\n")

# Set library path (Using D:/R/library)
lib_path <- "D:/R/library"
if (!dir.exists(lib_path)) {
    dir.create(lib_path, recursive = TRUE)
}

# Add library path to .libPaths
.libPaths(c(lib_path, .libPaths()))

cat("Library Path:", lib_path, "\n\n")

# Set CRAN mirror
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# List of packages to install
packages_to_install <- c(
    "tidyverse",     # Data manipulation (includes dplyr, ggplot2, tidyr, etc.)
    "readxl",        # Excel reading
    "brms",          # Bayesian modeling (*Takes the most time)
    "tidybayes",     # Bayesian data visualization
    "RColorBrewer",  # Color palettes
    "ggmcmc",        # MCMC plots
    "svglite",       # SVG output
    "bayestestR",    # ROPE analysis, etc.
    "extrafont",     # Font management
    "crayon",        # Console coloring
    "ordinal",       # Ordinal regression
    "RVAideMemoire"  # Statistical helpers
)

# Check installed packages
installed_pkgs <- rownames(installed.packages(lib.loc = lib_path))

# Filter packages that need installation
to_install <- packages_to_install[!packages_to_install %in% installed_pkgs]

if (length(to_install) > 0) {
    cat("Packages to install:\n")
    cat(paste(" -", to_install, collapse = "\n"), "\n\n")
    
    cat("Installing... (brms takes a particularly long time)\n\n")
    
    for (pkg in to_install) {
        cat(paste0("Installing: ", pkg, " ... "))
        tryCatch({
            install.packages(pkg, lib = lib_path, dependencies = TRUE, quiet = TRUE)
            cat("✓ Completed\n")
        }, error = function(e) {
            cat(paste0("✗ Error: ", e$message, "\n"))
        })
    }
} else {
    cat("All packages are already installed.\n")
}

cat("\n================================================================================\n")
cat("Installation Completed\n")
cat("================================================================================\n")

# Final verification
cat("\nVerification of installation status:\n")
installed_final <- rownames(installed.packages(lib.loc = lib_path))
for (pkg in packages_to_install) {
    if (pkg %in% installed_final) {
        cat(paste0("  ✓ ", pkg, "\n"))
    } else {
        cat(paste0("  ✗ ", pkg, " (Not installed)\n"))
    }
}
