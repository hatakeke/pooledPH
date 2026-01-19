# ==============================================================================
#  00_setup.R - Setup and Configuration
# ==============================================================================
#  Summary: Load libraries, read data, and perform preprocessing
#  Dependencies: None
#  Output: data_orig (Preprocessed dataframe)
# ==============================================================================

# Match the number of data points for comparison (guarantee fairness of NA handling)
use_complete_pairs_for_comparison <- FALSE
# use_complete_pairs_for_comparison <- TRUE

# Clear memory
gc()

# Data directory configuration
dataDir <- "./R/pooledPH/data"

# Output folder configuration
if (!exists("output_dir")) {
    output_dir <- "outputs"
}
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
}

# Load libraries
library(tidyverse)    # data manipulation
library(readxl)       # read excel
library(ggplot2)      # plot
library(brms)         # analysis
library(tidybayes)    # data manipulation
library(RColorBrewer) # needed for some extra colours in one of the graphs

# ==============================================================================
#  Study Label Definitions
# ==============================================================================
study_labels <- c(
    "Pilot 1",
    "Pilot 2", 
    "Pilot 3", 
    "Bernasconi (2021) Exp-1",
    "Bernasconi (2021) Exp-2", 
    "Pilot 4", 
    "Orepic 2021 Exp-1", 
    "Orepic_2021 Exp-2", 
    "Pilot 5", 
    "in-prep", 
    "Serino 2021 Exp-1", 
    "Serino 2021 Exp-2", 
    "Blanke 2014", 
    "Pilot 6", 
    "Salomon (2020)", 
    "Serino (2021) Exp-3", 
    "Orepic (2024) Exp-1", 
    "Faivre (2020) Exp-1",
    "Faivre (2020) Exp-2",
    "Faivre (2020) Exp-3",
    "Orepic (2024) Exp-2", 
    "Pilot 7",
    "Dhanis (2024) Ses-1",
    "Pilot 8",
    "Albert 2024 Exp-1",
    "Albert 2024 Exp-2"
)

# ==============================================================================
#  Question Item Definitions
# ==============================================================================
questionToColumn <- c(17:(17 + 20))
questions <- c(1:21)
noQuestions <- max(questions)

questionsDescription <- c(
    "I felt as if I had no body",                                             # 1 control
    "I felt as if I was touching my body",                                    # 2
    "I felt as if I was touching someone else' body",                         # 3
    "I felt I was behind my body",                                            # 4
    "I felt I had more than one body",                                        # 5 control
    "I felt as if someone else was touching my body",                         # 6 PE
    "I felt as if someone was standing behind my body",                       # 7 PH
    "I felt as if someone was standing in front of my body",                  # 8
    "I felt as if I had two right hands",                                     # 9
    "I felt as if the was a kind/gentle presence behind me",                  # 10
    "I felt as if there was a weird/unpleasant presence next to me",          # 11
    "I felt as if I could hear someone else's voice in my mind",              # 12
    "I felt as if I could share my thoughts with someone else",               # 13
    "I had the impression that I could not control my own thoughts",          # 14
    "I felt as if someone could read my mind or hear my thoughts",            # 15
    "I felt as if I was not controlling my movements or actions",             # 16 LoA
    "I felt anxious or stressed",                                             # 17
    "I felt as if I was in front of my body",                                 # 18
    "Presence position (0-no answer, 1-left, 2-middle, 3-right, 4-other)",    # 19
    "I felt as if I was touched by a robot",                                  # 20
    "I felt as if someone else was controlling my movements or actions"       # 21
)

# ==============================================================================
#  Visualization Settings
# ==============================================================================
colors_paper <- c("#d4bbff", "#4589ff")
colors_ppt <- c("#007380", "#00a79f")

fontsize_paper <- 7
fontsize_ppt <- 24

# EPFL Colors
EPFL_canard <- "#007480"
EPFL_leman <- "#00A79F"

# ==============================================================================
#  Helper Functions
# ==============================================================================

#' Data summary function
#' @param x Numeric vector
#' @return Named vector including y, ymin, and ymax
data_summary <- function(x) {
    m <- mean(x)
    ymin <- m - sd(x)
    ymax <- m + sd(x)
    if (ymax > 6) ymax <- 6
    if (ymin < 0) ymin <- 0
    return(c(y = m, ymin = ymin, ymax = ymax))
}

#' Mode calculation function
#' @param x Vector
#' @return Mode
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# ==============================================================================
#  Data Loading
# ==============================================================================
filename <- "../../data/PooledData.xlsx"

load_and_preprocess_data <- function(filename) {
    # Read data
    data_orig <- read_excel(filename, sheet = "Data")
    
    # Temporary Sync/Async data (for later use)
    temp_async <- filter(data_orig, Synchrony == 0)
    temp_sync <- filter(data_orig, Synchrony == 1)
    
    # Number of subjects
    noSubjects <- data_orig$Subject_No
    noSubjects <- noSubjects[length(noSubjects)]
    
    # Factor conversion
    data_orig$Experiment_ID <- factor(data_orig$Experiment_ID, labels = study_labels)
    data_orig$Subject_No <- factor(data_orig$Subject_No)
    data_orig$Gender_IsMale <- factor(data_orig$Gender_IsMale, labels = c("Female", "Male"))
    data_orig$Location <- factor(data_orig$Location, labels = c("Back", "Hand"))
    data_orig$Position <- factor(data_orig$Position, labels = c("Standing", "Laying"))
    data_orig$Order <- factor(data_orig$Order)
    data_orig$Previous_Exposure <- factor(data_orig$Previous_Exposure, labels = c("None", "Robot manipulation"))
    data_orig$Cognitive_Load <- factor(data_orig$Cognitive_Load, labels = c("No", "Yes"))
    data_orig$Force_field <- factor(data_orig$Force_field, labels = c("No", "Yes"))
    data_orig$Condition <- factor(data_orig$Condition, labels = c("Sync", "Async"))
    data_orig$Synchrony <- factor(data_orig$Synchrony, labels = c("Async", "Sync"))
    data_orig$Duration_sec <- as.numeric(scale(data_orig$Duration_sec))
    
    # Convert question items to ordinal factors
    for (i in 1:18) {
        col_name <- paste0("Question_ID_", i)
        if (col_name %in% colnames(data_orig)) {
            data_orig[[col_name]] <- factor(round(data_orig[[col_name]], digits = 0), ordered = TRUE)
        }
    }
    for (i in 20:21) {
        col_name <- paste0("Question_ID_", i)
        if (col_name %in% colnames(data_orig)) {
            data_orig[[col_name]] <- factor(round(data_orig[[col_name]], digits = 0), ordered = TRUE)
        }
    }

    return(list(
        data_orig = data_orig,
        temp_async = temp_async,
        temp_sync = temp_sync,
        noSubjects = noSubjects
    ))
}

# Execute data loading
data_list <- load_and_preprocess_data(filename)
data_orig <- data_list$data_orig
temp_async <- data_list$temp_async
temp_sync <- data_list$temp_sync
noSubjects <- data_list$noSubjects

# Question index definitions
# 1: Control (no body) | 7: PH (standing behind) 
# 1: no body (322) | 2: self-touch (554) | 5: more than one body (437) 
# 6: PE (580) | 7: PH (580) | 16: agency (189)

# Multiple question index list (for main analysis: PH & Control)
analysis_questions <- c(1, 7)  # Control(1), PH(7)
question_labels <- c("Control", "PH")

# Default single question index (for backward compatibility)
iquest <- 7

# ==============================================================================
#  NA Handling Configuration
# ==============================================================================
# How to handle NA during comparative analysis:
#  TRUE  : Use only data that is non-NA in both compared questions (strict comparison)
#  FALSE : Use all available data for each question (current default)

sl <- RColorBrewer::brewer.pal(9, 'Set1')

cat("\n====== Setup completed successfully ======\n")
cat("Data loaded:", nrow(data_orig), "rows,", ncol(data_orig), "columns\n")
cat("Analysis questions:", paste(paste0(analysis_questions, " (", question_labels, ")"), collapse=", "), "\n")

# ==============================================================================
#  Data Quality Check (Verify decimal data)
# ==============================================================================

cat("\n====== DATA QUALITY CHECK ======\n")

# Column indices for question items
question_cols <- questionToColumn[analysis_questions]

for (q_idx in seq_along(analysis_questions)) {
    current_q <- analysis_questions[q_idx]
    current_label <- question_labels[q_idx]
    col_idx <- question_cols[q_idx]
    col_name <- colnames(data_orig)[col_idx]
    
    # Get data for that column
    question_data <- data_orig[[col_name]]
    
    # Process only if not NULL or empty
    if (!is.null(question_data) && length(question_data) > 0) {
        # Convert to numeric (suppress warnings)
        numeric_data <- suppressWarnings(as.numeric(question_data))
        
        # Extract non-NA data
        valid_data <- numeric_data[!is.na(numeric_data)]
        
        if (length(valid_data) > 0) {
            # Check if there are decimals
            has_decimals <- any(valid_data != round(valid_data), na.rm = TRUE)
            
            # Display differently for integer-only vs decimal-containing
            if (has_decimals) {
                decimal_values <- valid_data[valid_data != round(valid_data)]
                cat("\nQuestion", current_q, "-", current_label, ":\n")
                cat("  ✓ Column:", col_name, "\n")
                cat("  ⚠ Contains DECIMAL values (not just integers 0-6):\n")
                cat("    Unique decimal values:", 
                    paste(sort(unique(decimal_values)), collapse=", "), "\n")
                cat("    Total decimal entries:", sum(valid_data != round(valid_data)), "\n")
                cat("    Example:", head(decimal_values, 5), "\n")
                cat("  → These decimal values ARE INCLUDED in the current analysis\n")
            } else {
                cat("\nQuestion", current_q, "-", current_label, ":\n")
                cat("  ✓ Column:", col_name, "\n")
                cat("  ✓ Contains only INTEGER values (0-6)\n")
            }
            
            # NA information
            n_na <- sum(is.na(numeric_data))
            cat("  Missing (NA):", n_na, "out of", length(question_data), 
                "rows (", round(100*n_na/length(question_data), 1), "%)\n")
        }
    }
}

cat("\n" , rep("-", 80), "\n", sep="")
cat("NA HANDLING MODE: ", 
    ifelse(use_complete_pairs_for_comparison, 
           "STRICT (both Q1 & Q7 must have valid data)",
           "LENIENT (each question uses available data)"),
    "\n", sep="")
cat(rep("-", 80), "\n", sep="")
