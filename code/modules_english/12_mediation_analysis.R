# ==============================================================================
#  12_mediation_analysis.R - Mediation Analysis
# ==============================================================================
#  Summary: Mediation analysis between PH, PE, and LOA.
#  Dependencies: 00_setup.R
#  Output: Mediation analysis models and plots.
# ==============================================================================

library(tidyverse)
library(readxl)
library(brms)
library(tidybayes)

# ==============================================================================
#  Data Preparation for Mediation Analysis
# ==============================================================================

#' Prepare data for mediation analysis
#' @param filename Data filename
#' @return Prepared dataframe
prepare_mediation_data <- function(filename = "PooledData.xlsx") {
    
    data_orig2 <- read_excel(filename, sheet = "Data")
    
    # Factor conversion
    data_orig2$Subject_No <- factor(data_orig2$Subject_No)
    data_orig2$Gender_IsMale <- factor(data_orig2$Gender_IsMale, labels = c("Female", "Male"))
    data_orig2$Position <- factor(data_orig2$Position, labels = c("Standing", "Laying"))
    data_orig2$Location <- factor(data_orig2$Location, labels = c("Back", "Hand"))
    data_orig2$Order <- factor(data_orig2$Order)
    data_orig2$Previous_Exposure <- factor(data_orig2$Previous_Exposure, labels = c("None", "Robot manipulation"))
    data_orig2$Cognitive_Load <- factor(data_orig2$Cognitive_Load, labels = c("No", "Yes"))
    data_orig2$Condition <- factor(data_orig2$Condition, labels = c("Sync", "Async"))
    data_orig2$Synchrony <- factor(data_orig2$Synchrony, labels = c("Async", "Sync"))
    data_orig2$Duration_sec <- as.numeric(scale(data_orig2$Duration_sec))
    
    return(data_orig2)
}

#' Calculation of relative increase (sPH, sPE, sLoA)
#' @param selectedData Selected data
#' @return Dataframe including mediation variables
compute_relative_increase <- function(selectedData) {
    
    # Helper function to safely convert factors to numeric
    to_num <- function(x) as.numeric(as.character(x))
    
    # PH relative increase
    PH_async <- filter(selectedData, !is.na(`Question_ID_7`), Synchrony == "Async")
    PH_sync <- filter(selectedData, !is.na(`Question_ID_7`), Synchrony == "Sync")
    PH_diff <- to_num(PH_async$`Question_ID_7`) - to_num(PH_sync$`Question_ID_7`)
    PH_diff <- PH_diff > 0
    PH_diff <- rep(PH_diff, each = 2)
    
    # PE relative increase
    PE_async <- filter(selectedData, !is.na(`Question_ID_6`), Synchrony == "Async")
    PE_sync <- filter(selectedData, !is.na(`Question_ID_6`), Synchrony == "Sync")
    PE_diff <- to_num(PE_async$`Question_ID_6`) - to_num(PE_sync$`Question_ID_6`)
    PE_diff <- PE_diff > 0
    PE_diff <- rep(PE_diff, each = 2)
    
    # LoA relative increase
    LoA_async <- filter(selectedData, Synchrony == "Async")
    LoA_sync <- filter(selectedData, Synchrony == "Sync")
    LoA_diff <- to_num(LoA_async$`Question_ID_16`) - to_num(LoA_sync$`Question_ID_16`)
    LoA_diff <- LoA_diff > 0
    LoA_diff <- rep(LoA_diff, each = 2)
    
    # Create mediation frame
    mediationFrame <- selectedData
    mediationFrame$sPH <- factor(PH_diff)
    mediationFrame$sPE <- factor(PE_diff)
    mediationFrame$sLoA <- factor(LoA_diff)
    
    # Convert questions to ordinal factor
    mediationFrame$Question_ID_6 <- factor(round(to_num(mediationFrame$`Question_ID_6`), digits = 0), ordered = TRUE)
    mediationFrame$Question_ID_7 <- factor(round(to_num(mediationFrame$`Question_ID_7`), digits = 0), ordered = TRUE)
    mediationFrame$Question_ID_16 <- factor(round(to_num(mediationFrame$`Question_ID_16`), digits = 0), ordered = TRUE)
    
    return(mediationFrame)
}

# ==============================================================================
#  Mediation Model Definition
# ==============================================================================

# Informative priors (Common)
mediation_priors <- c(
    set_prior("normal(-0.4220, 0.1367)", class = "Intercept", coef = "1"),
    set_prior("normal(-0.0520, 0.1365)", class = "Intercept", coef = "2"),
    set_prior("normal(0.2973, 0.1366)", class = "Intercept", coef = "3"),
    set_prior("normal(0.5935, 0.1371)", class = "Intercept", coef = "4"),
    set_prior("normal(1.0518, 0.1388)", class = "Intercept", coef = "5"),
    set_prior("normal(1.5836, 0.1429)", class = "Intercept", coef = "6"),
    set_prior("normal(0.3406, 0.0908)", coef = "ConditionAsync"),
    set_prior("normal(-0.6545, 0.2808)", coef = "PositionLaying"),
    set_prior("normal(-0.6079, 0.1740)", coef = "LocationHand"),
    set_prior("normal(-0.0983, 0.1524)", coef = "Previous_ExposureRobotmanipulation"),
    set_prior("normal(0.1719, 0.4473)", coef = "Cognitive_LoadYes"),
    set_prior("normal(-0.0299, 0.1619)", coef = "Duration_sec"),
    set_prior("normal(-0.0846, 0.0470)", coef = "Age"),
    set_prior("normal(-0.0732, 0.0640)", coef = "Gender_IsMaleMale"),
    set_prior("normal(0.0651, 0.2104)", coef = "ConditionAsync:PositionLaying"),
    set_prior("normal(0.0023, 0.2130)", coef = "ConditionAsync:LocationHand"),
    set_prior("normal(-0.1607, 0.1542)", coef = "ConditionAsync:Previous_ExposureRobotmanipulation"),
    set_prior("normal(-0.2214, 0.2986)", coef = "ConditionAsync:Cognitive_LoadYes"),
    set_prior("normal(0.0018, 0.1146)", coef = "ConditionAsync:Duration_sec"),
    set_prior("normal(-0.0840, 0.0641)", coef = "ConditionAsync:Age")
)

#' Build PH mediated by PE model
#' @param mediationFrame Mediation analysis dataframe
#' @return brms model object
build_ph_mediated_by_pe <- function(mediationFrame) {
    
    formula_PHmedPE <- as.formula(paste(
        "Question_ID_7 ~ Condition + 
        Position + Condition:Position + 
        Location + Condition:Location +
        Previous_Exposure + Condition:Previous_Exposure +
        Cognitive_Load + Condition:Cognitive_Load +
        Duration_sec + Duration_sec:Condition +
        Age + Age:Condition +
        Gender_IsMale + 
        sPE + sPE:Condition +
        (1|Experiment_ID)"
    ))
    
    model <- brm(
        formula_PHmedPE, 
        data = mediationFrame,
        family = cumulative("probit", threshold = "flexible"),
        prior = mediation_priors,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13), 
        init = 0
    )
    
    return(model)
}

#' Build PE mediated by PH model
#' @param mediationFrame Mediation analysis dataframe
#' @return brms model object
build_pe_mediated_by_ph <- function(mediationFrame) {
    
    # Priors for PE
    pe_priors <- c(
        set_prior("normal(-0.4619, 0.1458)", class = "Intercept", coef = "1"),
        set_prior("normal(-0.0875, 0.1457)", class = "Intercept", coef = "2"),
        set_prior("normal(0.2718, 0.1458)", class = "Intercept", coef = "3"),
        set_prior("normal(0.5751, 0.1463)", class = "Intercept", coef = "4"),
        set_prior("normal(1.0508, 0.1482)", class = "Intercept", coef = "5"),
        set_prior("normal(1.5837, 0.1522)", class = "Intercept", coef = "6"),
        set_prior("normal(0.3431, 0.0925)", coef = "ConditionAsync"),
        set_prior("normal(-0.6436, 0.3023)", coef = "PositionLaying"),
        set_prior("normal(-0.1328, 0.1649)", coef = "Previous_ExposureRobotmanipulation"),
        set_prior("normal(-0.0142, 0.1751)", coef = "Duration_sec"),
        set_prior("normal(-0.0627, 0.0501)", coef = "Age"),
        set_prior("normal(-0.0971, 0.0673)", coef = "Gender_IsMaleMale"),
        set_prior("normal(-0.0901, 0.0885)", coef = "sPHTRUE"),
        set_prior("normal(0.6589, 0.1220)", coef = "ConditionAsync:sPHTRUE"),
        set_prior("normal(0.0540, 0.2145)", coef = "ConditionAsync:PositionLaying"),
        set_prior("normal(-0.1462, 0.1639)", coef = "ConditionAsync:Previous_ExposureRobotmanipulation"),
        set_prior("normal(0.0006, 0.1155)", coef = "ConditionAsync:Duration_sec"),
        set_prior("normal(-0.0853, 0.0678)", coef = "ConditionAsync:Age")
    )
    
    formula_PEmedPH <- as.formula(paste(
        "Question_ID_6 ~ Condition + 
        Position + Condition:Position + 
        Previous_Exposure + Condition:Previous_Exposure +
        Duration_sec + Duration_sec:Condition +
        Age + Age:Condition +
        sPH + sPH:Condition +
        sLoA + sLoA:Condition +
        Gender_IsMale + 
        (1|Experiment_ID)"
    ))
    
    model <- brm(
        formula_PEmedPH, 
        data = mediationFrame,
        family = cumulative("probit", threshold = "flexible"),
        prior = pe_priors,
        warmup = 2000, 
        iter = 8000,
        cores = 4, 
        chains = 4, 
        control = list(max_treedepth = 13),
        init = 0
    )
    
    return(model)
}

# ==============================================================================
#  Mediation Effect Plots
# ==============================================================================

#' Create mediation effect plot
#' @param post Posterior sample dataframe
#' @param interaction_var Interaction variable name
#' @param x_title X-axis title
#' @param colors Color settings
#' @param fontsize Font size
#' @param output_file Output filename
#' @return ggplot object
create_mediation_plot <- function(
    post,
    interaction_var,
    x_title,
    colors = c("#d4bbff", "#4589ff"),
    fontsize = 7,
    output_file = NULL
    ) {
    
    # Calculate statistics manually
    vals <- post[[interaction_var]]
    summary_df <- data.frame(
        median = median(vals),
        lower_89 = quantile(vals, 0.055),
        upper_89 = quantile(vals, 0.945),
        lower_66 = quantile(vals, 0.17),
        upper_66 = quantile(vals, 0.83),
        y = 0
    )
    
    p <- ggplot(summary_df, aes(x = median, y = y)) +
        geom_linerange(aes(xmin = lower_89, xmax = upper_89), color = colors[1], linewidth = 0.8) +
        geom_linerange(aes(xmin = lower_66, xmax = upper_66), color = colors[1], linewidth = 1.5) +
        geom_point(color = colors[2], size = 1.5) +
        geom_vline(xintercept = c(0.1), linetype = "dashed") +
        theme_minimal() + 
        theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            text = element_text(size = fontsize, )
        ) +
        xlab(x_title)
    
    if (!is.null(output_file)) {
        png_file <- paste0(output_file, ".png")
        ggsave(png_file, plot = p, units = "mm", width = 42, height = 48, dpi = 300)
        cat("Mediation plot saved to:", png_file, "\n")
        
        # Save numeric data as CSV
        csv_file <- paste0(output_file, "_data.csv")
        write.csv(summary_df, csv_file, row.names = FALSE)
        cat("Mediation data saved to:", csv_file, "\n")
    }
    
    return(list(plot = p, data = summary_df))
}

# ==============================================================================
#  Execution (Commented out - Run as needed)
# ==============================================================================

# These processes may take time. Please run them individually if needed.

# # Data Preparation
# data_orig2 <- prepare_mediation_data("PooledData.xlsx")
# 
# # Select subjects with both PH and PE data
# selectedData <- filter(data_orig2, !is.na(`Question_ID_7`), !is.na(`Question_ID_6`))
# # To include LoA as well:
# # selectedData <- filter(data_orig2, !is.na(`Question_ID_7`), !is.na(`Question_ID_6`), !is.na(`Question_ID_16`))
# 
# # Calculate mediation variables
# mediationFrame <- compute_relative_increase(selectedData)
# 
# # Model for PH mediated by PE
# model_PHmedPE <- build_ph_mediated_by_pe(mediationFrame)
# 
# # Get posterior samples
# post_PHmedByPE <- posterior_samples(model_PHmedPE) %>%
#     mutate(iter = 1:n())
# 
# # Check results
# mode_hdi(post_PHmedByPE$`b_ConditionAsync:sPETRUE`, .width = 0.89)
# 
# # Plotting
# create_mediation_plot(
#     post_PHmedByPE,
#     "b_ConditionAsync:sPETRUE",
#     "Synchrony : riPE\n(Async, +)",
#     output_file = file.path(output_dir, "PH_Interaction_Condition_PE")
# )

cat("====== Mediation analysis module loaded ======\n")
cat("Available functions:\n")
cat("  - prepare_mediation_data()\n")
cat("  - compute_relative_increase()\n")
cat("  - build_ph_mediated_by_pe()\n")
cat("  - build_pe_mediated_by_ph()\n")
cat("  - create_mediation_plot()\n")
