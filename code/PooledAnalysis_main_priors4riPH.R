#deletes everything in the global environment
# rm(list=ls()) 
#eliminates from memory
gc()

dataDir = "./R/pooledPH/data"
# setwd(dataDir)

library(tidyverse)    # data manipulation
library(readxl)       # read excel
library(ggplot2)      # plot
library(brms)         # analysis
library(tidybayes)    # data manipulation
library(RColorBrewer) # needed for some extra colours in one of the graphs

study_labels = c("Pilot 1",
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
                 "Albert 2024 Exp-2")

filename = "PooledData.xlsx"

data_orig <- read_excel(filename, sheet = "Data")
temp_async = filter(data_orig, Synchrony == 0) # used later
temp_sync  = filter(data_orig, Synchrony == 1) # used later

noSubjects = data_orig$Subject_No
noSubjects = noSubjects[length(noSubjects)]

data_orig$Experiment_ID     = factor(data_orig$Experiment_ID, labels = study_labels)

data_orig$Subject_No        = factor(data_orig$Subject_No)
data_orig$Gender_IsMale     = factor(data_orig$Gender_IsMale, labels = c("Female", "Male"))
data_orig$Location          = factor(data_orig$Location, labels = c("Back", "Hand"))
data_orig$Position          = factor(data_orig$Position, labels = c("Standing", "Laying"))
data_orig$Order             = factor(data_orig$Order)
data_orig$Previous_Exposure = factor(data_orig$Previous_Exposure, labels = c("None", "Robot manipulation"))
data_orig$Cognitive_Load    = factor(data_orig$Cognitive_Load, labels = c("No", "Yes"))
data_orig$Force_field       = factor(data_orig$Force_field, labels = c("No", "Yes"))
data_orig$Condition         = factor(data_orig$Condition, labels = c("Sync", "Async"))
data_orig$Synchrony         = factor(data_orig$Synchrony, labels = c("Async", "Sync"))
data_orig$Duration_sec      = scale(data_orig$Duration_sec)
data_orig$`Question_ID_1`   = factor(round(data_orig$`Question_ID_1`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_2`   = factor(round(data_orig$`Question_ID_2`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_3`   = factor(round(data_orig$`Question_ID_3`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_4`   = factor(round(data_orig$`Question_ID_4`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_5`   = factor(round(data_orig$`Question_ID_5`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_6`   = factor(round(data_orig$`Question_ID_6`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_7`   = factor(round(data_orig$`Question_ID_7`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_8`   = factor(round(data_orig$`Question_ID_8`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_9`   = factor(round(data_orig$`Question_ID_9`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_10`  = factor(round(data_orig$`Question_ID_10`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_11`  = factor(round(data_orig$`Question_ID_11`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_12`  = factor(round(data_orig$`Question_ID_12`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_13`  = factor(round(data_orig$`Question_ID_13`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_14`  = factor(round(data_orig$`Question_ID_14`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_15`  = factor(round(data_orig$`Question_ID_15`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_16`  = factor(round(data_orig$`Question_ID_16`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_17`  = factor(round(data_orig$`Question_ID_17`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_18`  = factor(round(data_orig$`Question_ID_18`, digits = 0), ordered = TRUE)
#data_orig$`Question_ID_19`  = factor(round(data_orig$`Question_ID_19`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_20`  = factor(round(data_orig$`Question_ID_20`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_21`  = factor(round(data_orig$`Question_ID_21`, digits = 0), ordered = TRUE)

questionToColumn = c(17:(17+20));

questions   = c(1:21)
noQuestions = max(questions)
questionsDescription = c("I felt as if I had no body", # control
                         "I felt as if I was touching my body", 
                         "I felt as if I was touching someone else' boby", 
                         "I felt I was behind my body", 
                         "I felt I had more than one body", # control
                         "I felt as if someone else was touching my body", 
                         "I felt as if someone was standing behing my body",
                         "I felt as if someone was standing in front of my body", 
                         "I felt as if I had two right hands",
                         "I felt as if the was a kind/gentle presence behind me",
                         "I felt as if there was a weird/unpleasend presence next to me",
                         "I felt as if I could hear someone else's voice in my mind",
                         "I felt as if I could share my thoughts with someone else",
                         "I had the impression that I could not control my own thoughts",
                         "I felt as if someone could read my mind or hear my thoughts",
                         "I felt as if I was not controlling my movements or actions",
                         "I felt anxious or stressed",
                         "I felt as if I was in front of my body",
                         "Presence position (0-no answer, 1-left, 2-middle, 3-right, 4-other)",
                         "I felt as if I was touched by a robot",
                         "I felt as if someone else was controlling my movements or actions")


colors_paper = c("#d4bbff", "#4589ff")
colors_ppt = c("#007380", "#00a79f")

fontsize_paper = 7
fontsize_ppt = 24


data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  if(ymax > 6){
    ymax = 6
  }
  if(ymin < 0){
    ymin = 0
  }
  return(c(y=m,ymin=ymin,ymax=ymax))
}
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 1  no body (322) | 2 self-touch (554) | 5 more than one body (437) | 6 PE (580) | 7 PH (580) | 16 agency (189)
iquest = 7
sl <- RColorBrewer::brewer.pal(9,'Set1')
print(paste0(":::::::::: Starting analysis for question ", iquest, ": ", questionsDescription[iquest], " ::::::::::"))

# colours
EPFL_canard = "#007480"
EPFL_leman  = "#00A79F"

# Main full model -----------------------------------------------------------------

# there's like 4 participants without age 
selectedData = filter(data_orig, !is.na(Age))
selectedData$Age = (selectedData$Age-mean(selectedData$Age))/sd(selectedData$Age)
formula_full_dem =
  as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
                   "~ Condition +
                   Position + Condition:Position +
                   Location + Condition:Location +
                   Previous_Exposure + Condition:Previous_Exposure +
                   Cognitive_Load + Condition:Cognitive_Load +
                   Duration_sec + Condition:Duration_sec +
                   Age + Age:Condition +
                   Gender_IsMale + (1+Condition|Experiment_ID)"))

# # use this for SoA
# formula_full_dem = 
#   as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
#                    "~ Condition + 
#                    Position + Condition:Position + 
#                    Previous_Exposure + Condition:Previous_Exposure +
#                    Duration_sec + Condition:Duration_sec +
#                    Age + Age:Condition +
#                    Gender_IsMale + (1+Condition|Experiment_ID)"))


print(formula_full_dem)
print(paste0("Selected question ", iquest, ": ", questionsDescription[iquest]))

model_fullExperimentalParameters_dem = 
  brm(formula_full_dem, 
      data = selectedData,
      family = cumulative("probit", threshold = "flexible"),
      prior = c(set_prior("normal(0,5)", class = "b"),
                set_prior("student_t(3, 0, 2)", class = "sd")),
      warmup = 2000, iter = 8000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13), init = 0)

# keep a short alias `model` because later plotting code expects `model`
model <- model_fullExperimentalParameters_dem

# For the forest plot:
library(tidyverse)
library(tidybayes)

colors_paper = c("#d4bbff", "#4589ff")
colors_ppt = c("#007380", "#00a79f")

A = spread_draws(model_fullExperimentalParameters_dem, 
                 b_ConditionAsync, r_Experiment_ID[Experiment_ID,])

# add the grand mean to the group-specific deviations
B = mutate(A, mu = b_ConditionAsync + r_Experiment_ID)

C = ungroup(B)

D = mutate(C, outcome = str_replace_all(Experiment_ID, "[.]", " "))

ggplot(D, aes(x = mu, y = reorder(outcome, mu))) +
  geom_vline(xintercept = 0, linetype = "dotdash", color = "black") +
  geom_vline(xintercept = -0.5, linetype = "dotdash", color = "grey") +
  geom_vline(xintercept = 0.5, linetype = "dotdash", color = "grey") +
  geom_vline(xintercept = -1.0, linetype = "dotdash", color = "grey") +
  geom_vline(xintercept = 1.0, linetype = "dotdash", color = "grey") +
  stat_halfeye(.width = .89, size = 2/3, fill = "#d4bbff", color = "#4589ff") +
  scale_x_continuous(limits = c(-1.25,1.3)) +
  labs(x = expression(italic("as Cohen's d")),
       y = NULL) +
  theme_minimal() +
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0.95),
        text = element_text(size = 7, family = "Arial"))


library(svglite)

figureName = "ForestPlot.svg"
ggsave(figureName, units="mm", width = 90, height = 130) # cut axis


# Funnel plot -----------------------------------------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggmcmc)

# Your extracted ranef_df
ranef_df <- as.data.frame(ranef(model)$Experiment_ID[, , "Intercept"]) %>%
  mutate(Experiment_ID = rownames(ranef(model)$Experiment_ID))

# Calculate precision
ranef_df <- ranef_df %>%
  mutate(Precision = 1 / Est.Error)

# Create funnel shading data (expected 95% CI region around zero)
se_range <- seq(min(ranef_df$Est.Error), max(ranef_df$Est.Error), length.out = 100)
funnel_df <- data.frame(
  SE = se_range,
  Precision = 1 / se_range,
  Lower = -1.96 * se_range,
  Upper = 1.96 * se_range
)

# Plot with shaded funnel
ggplot(ranef_df, aes(x = Estimate, y = Precision)) +
  geom_ribbon(
    data = funnel_df,
    aes(xmin = Lower, xmax = Upper, y = Precision),
    inherit.aes = FALSE,
    fill = "grey80",
    alpha = 0.5
  ) +
  geom_point(alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Funnel Plot Analogue (Random Intercepts)",
    x = "Deviation from Overall Intercept (Latent Scale)",
    y = "Precision (1 / SE)"
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())



# REPEAT FOR EXPERIMENT-DEPENDENT CONDITION
# Get random effects for Experiment_ID
library(dplyr)
library(ggplot2)

# Base data
slope_df <- as.data.frame(ranef(model)$Experiment_ID[, , "ConditionAsync"]) %>%
  mutate(Experiment_ID = rownames(.)) %>%
  rename(Estimate = Estimate, SE = Est.Error) %>%
  mutate(Precision = 1 / SE)

# Create symmetric funnel shape based on expected SE range
se_range <- seq(min(slope_df$SE), max(slope_df$SE), length.out = 100)
funnel <- data.frame(
  SE = se_range,
  Precision = 1 / se_range,
  Lower = -1.96 * se_range,
  Upper =  1.96 * se_range
)

# Plot with working shaded funnel
ggplot(slope_df, aes(x = Estimate, y = Precision)) +
  geom_ribbon(
    data = funnel,
    aes(xmin = Lower, xmax = Upper, y = Precision),
    inherit.aes = FALSE,
    fill = "grey80", alpha = 0.5
  ) +
  geom_point(alpha = 0.8) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "blue") +
  labs(
    title = "Funnel Plot Analogue: Random Slopes for Condition (Async vs. Sync)",
    x = "Random Slope Estimate for ConditionAsync",
    y = "Precision (1 / SE)"
  ) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())




# Posterior predictive checks -------------------------------------------------------------------------------------

library(ggplot2)

# Create ECDF overlay plot
p <- pp_check(model, type = "ecdf_overlay")

# Customize it with ggplot2
p + 
  labs(
    title = "Posterior Predictive Check: ECDF Overlay",
    x = "Response Category (Quest)",
    y = "Cumulative Proportion"
  ) +
  scale_color_manual(
    name = "Data Type",
    values = c("black", "red"),
    labels = c("Observed Responses", "Model Predictions")
  ) +
  theme_minimal()



# Model: order ------------------------------------------------------------------------------------------------
formula_full_dem_order = 
  as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
                   "~ Condition +
                   Position + Condition:Position + 
                   Location + Condition:Location + 
                   Previous_Exposure + Condition:Previous_Exposure +
                   Cognitive_Load + Condition:Cognitive_Load +
                   Duration_sec + Duration_sec:Condition +
                   Age + Age:Condition +
                   Gender_IsMale + 
                   Order + Order:Condition + 
                   (1+Condition|Experiment_ID)"))

model_fullExperimentalParameters_dem_order = 
  brm(formula_full_dem_order, 
      data = selectedData,
      family = cumulative("probit", threshold = "flexible"),
      prior = c(set_prior("normal(-0.4220, 0.1367)", class = "Intercept", coef = "1"),
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
                set_prior("normal(-0.0840, 0.0641)", coef = "ConditionAsync:Age")),
      warmup = 2000, iter = 8000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13))


# Model: EHI ------------------------------------------------------------------------------------------------------
formula_full_dem_EHI = 
  as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
                   "~ Condition + 
                   Position + Condition:Position + 
                   Location + Condition:Location +
                   Previous_Exposure + Condition:Previous_Exposure +
                   Cognitive_Load + Condition:Cognitive_Load +
                   Duration_sec + Duration_sec:Condition +
                   Age + Age:Condition +
                   Gender_IsMale +
                   EHI + EHI:Condition + 
                   (1+Condition|Experiment_ID)"))

selectedData = filter(data_orig, !is.na(Age), !is.na(EHI))
selectedData$EHI = (selectedData$EHI-mean(selectedData$EHI))/sd(selectedData$EHI)
selectedData$Age = (selectedData$Age-mean(selectedData$Age))/sd(selectedData$Age)
model_fullExperimentalParameters_dem_EHI = 
  brm(formula_full_dem_EHI, 
      data = selectedData,
      family = cumulative("probit", threshold = "flexible"),
      prior = c(set_prior("normal(-0.4220, 0.1367)", class = "Intercept", coef = "1"),
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
                set_prior("normal(-0.0840, 0.0641)", coef = "ConditionAsync:Age")),
      warmup = 2000, iter = 8000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13))


# Model: PDI ------------------------------------------------------------------------------------------------------
formula_full_dem_PDI = 
  as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
                   "~ Condition + 
                   Position + Condition:Position + 
                   Location + Condition:Location + 
                   Previous_Exposure + Condition:Previous_Exposure +
                   Cognitive_Load + Condition:Cognitive_Load +
                   Duration_sec + Duration_sec:Condition +
                   Age + Age:Condition +
                   Gender_IsMale +
                   PDI + PDI:Condition + 
                   (1+Condition|Experiment_ID)"))

selectedData = filter(data_orig, !is.na(Age), !is.na(PDI))
selectedData$PDI = (selectedData$PDI-mean(selectedData$PDI))/sd(selectedData$PDI)
selectedData$Age = (selectedData$Age-mean(selectedData$Age))/sd(selectedData$Age)
model_fullExperimentalParameters_dem_PDI = 
  brm(formula_full_dem_PDI, 
      data = selectedData,
      family = cumulative("probit", threshold = "flexible"),
      prior = c(set_prior("normal(-0.4220, 0.1367)", class = "Intercept", coef = "1"),
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
                set_prior("normal(-0.0840, 0.0641)", coef = "ConditionAsync:Age")),
      warmup = 2000, iter = 8000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13), init = 0)

# Assess convergency (catterpillar plots) ---------------------------------
modelADPT_full = ggs(model_fullExperimentalParameters_dem)

# Ensure 'betas' exists (some code expects betas to be defined later in the script).
# If it's missing, derive a sensible default from the ggs() output.
if(!exists("betas")){
  betas <- unique(modelADPT_full$Parameter)
}

ggplot(filter(modelADPT_full, Parameter %in% 
                betas[c(4,5,6)]),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  scale_y_continuous(limits = c(-3, 3)) + 
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")


# Intercept only predictions -----------------------

# check rating histogram
ggplot(data = data_orig, aes(x = Question_ID_7)) +
  geom_bar() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "PH rating - (nal data)", y = "Counts") + 
  theme_minimal()

pp_check(model_fullExperimentalParameters_dem)

# Real PH rating histogram
histo = ggplot(data_orig, aes(x = Question_ID_7)) +
  geom_bar() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  labs(x = "PH rating (observed data)", y = "Counts") + 
  theme_minimal()
histo


# Mostly testing, the appropriate thresholds are computed in fuller models
# gives posterior samples, selects only the intercepts, and adds the iteration column
post_predictions = posterior_samples(model_fullExperimentalParameters_dem) %>%
  select(`b_Intercept[1]`:`b_Intercept[6]`) %>%
  mutate(iter = 1:n())

# check if counts have similar histogram
set.seed(23)
post_predictions %>% 
  mutate(z = rnorm(n(), mean = 0, sd = 1)) %>% 
  mutate(Question_ID_7 = case_when(
    z < `b_Intercept[1]` ~ 0,
    z < `b_Intercept[2]` ~ 1,
    z < `b_Intercept[3]` ~ 2,
    z < `b_Intercept[4]` ~ 3,
    z < `b_Intercept[5]` ~ 4,
    z < `b_Intercept[6]` ~ 5,
    z >= `b_Intercept[6]` ~ 6
  ) %>% as.factor(.)) %>% 
  
  ggplot(aes(x = Question_ID_7)) +
  geom_bar() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  labs(x = "PH rating (simulated data: using intercepts only)", y = "Counts") + 
  theme_minimal()

# magrittr: https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
# meaning of transmute and mutate
# https://www.datanovia.com/en/lessons/compute-and-add-new-variables-to-a-data-frame-in-r/
# basically:
# post = select(post, -iter)
# post = mutate_all(post, .funs) 
# post = transmute(post, .funs)
# 

colors_paper = c("#d4bbff", "#4589ff")
fontsize_paper = 7

post_predictions %>% 
  select(-iter) %>% 
  mutate_all(.funs = ~pnorm(. , 0, 1)) %>% 
  transmute(`p[Q7==0]` = `b_Intercept[1]`,
            `p[Q7==1]` = `b_Intercept[2]` - `b_Intercept[1]`,
            `p[Q7==2]` = `b_Intercept[3]` - `b_Intercept[2]`,
            `p[Q7==3]` = `b_Intercept[4]` - `b_Intercept[3]`,
            `p[Q7==4]` = `b_Intercept[5]` - `b_Intercept[4]`,
            `p[Q7==5]` = `b_Intercept[6]` - `b_Intercept[5]`,
            `p[Q7==6]` = 1 - `b_Intercept[6]`) %>% 
  set_names(0:6) %>% 
  pivot_longer(everything(), names_to = "Q7") %>% 
  
  ggplot(aes(x = value, y = Q7)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89,
               fill = colors_paper[1], color = colors_paper[2], size = 1/2) +
  scale_x_continuous(expression(italic(p)*"("*italic(rating)*")"), 
                     expand = c(0, 0), limits = c(0, 0.65)) + 
  labs(y = "Rating") +
  theme_minimal() + 
  theme(legend.position = "none",
        text = element_text(size = fontsize_paper, family = "Arial"))
ggsave("PH_Intercepts.svg", units="mm", width = 70, height = 70) # for all


tibble(x = seq(from = -3.5, to = 3.5, by = .01)) %>%
  mutate(d = dnorm(x)) %>% 
  
  ggplot(aes(x = x, ymin = 0, ymax = d)) +
  geom_ribbon(fill = "black") +
  geom_vline(xintercept = fixef(model_fullExperimentalParameters_dem)[1:6, 1], 
                                color = colors_paper[1], linetype = 2, size = 0.75) +
  # make breaks/labels robust to models with fewer than 6 thresholds
  {
    tmp_breaks <- tryCatch(as.numeric(fixef(model_fullExperimentalParameters_dem)[,1]), error = function(e) NULL)
    if (is.null(tmp_breaks)) tmp_breaks <- numeric(0)
    n_breaks <- min(length(tmp_breaks), 6)
    if (n_breaks > 0) {
      tmp_breaks2 <- tmp_breaks[1:n_breaks]
      tmp_labels2 <- parse(text = str_c("theta[", seq_len(n_breaks), "]"))
    } else {
      tmp_breaks2 <- NULL
      tmp_labels2 <- NULL
    }
    scale_x_continuous("Posterior modes for the rating scale intercepts",
                       breaks = tmp_breaks2,
                       labels = tmp_labels2)
  } +
  scale_y_continuous(NULL, breaks = NULL, expand = expansion(mult = c(0, 0.05))) +
  # ggtitle("Standard normal distribution underlying the ordinal Y data:",
  #         subtitle = "The dashed vertical lines mark the posterior means for the thresholds.") +
  coord_cartesian(xlim = c(-3, 5)) + 
  theme_minimal() +
  theme(
        # title = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial"))
ggsave("PH_Intercepts_norm.svg", units="mm", width = 70, height = 70) # for all




# Coefficients for different effects --------------------------------------

# model_fullExperimentalParameters_dem_EHI
post = posterior_samples(model_fullExperimentalParameters_dem) %>%
  mutate(iter = 1:n())

figure_name = c("PH_Async.svg",
                "PH_BodyPosition.svg",
                "PH_BodyLocation.svg",
                "PH_Previousexposure.svg",
                "PH_CognitiveLoad.svg",
                "PH_Duration.svg",
                "PH_Age.svg",
                "PH_Async_BodyPosition.svg",
                "PH_Async_BodyLocation.svg",
                "PH_Async_PreviousExposure.svg",
                "PH_Async_CognitiveLoad.svg",
                "PH_Async_Duration.svg",
                "PH_Async_Age.svg",
                "PH_SexAtBirth.svg")

betas = c("b_ConditionAsync", 
          "b_PositionLaying", 
          "b_LocationHand",
          "b_Previous_ExposureRobotmanipulation", 
          "b_Cognitive_LoadYes", 
          "b_Duration_sec", 
          "b_Age",
          "b_ConditionAsync:PositionSupine",
          "b_ConditionAsync:LocationHand",
          "b_ConditionAsync:Previous_ExposureRobotmanipulation", 
          "b_ConditionAsync:Cognitive_Load_Yes",
          "b_ConditionAsync:Duration_sec",
          "b_ConditionAsync:Age",
          "b_Gender_IsMaleMale")

xtitles = c("Asynchrony\n(async)", 
            "Body Position\n(supine)",
            "Body Location\n(hand)",
            "Previous Exposure\n(+)",
            "Cognitive Load\n(+)", 
            "Duration\n(increasing)",
            "Age\n(increasing in age)",
            "Asynchrony : Body Position\n(Async, supine)",
            "Asynchrony : Body Location\n(Async, hand)",
            "Asynchrony : Prev. Exp.\n(Async, +)", 
            "Asynchrony : Cog. Load\n(Async, +)",
            "Asynchrony : Duration\n(Async, Increasing)",
            "Asynchrony : Age\n(Async, Increasing)",
            "Sex at birth\n(Male)"
            )

effects_condE = c("Condition", "Position", "Previous_Exposure", "Cognitive_Load",  "Duration_sec")

# figures
library(extrafont)
# font_import()
# loadfonts(device = "win")

colors_paper = c("#d4bbff", "#4589ff")
colors_ppt = c("#007380", "#00a79f")

fontsize_paper = 7
fontsize_ppt = 24


# estimate of beta 
for(i in c(2:7)){ 
    p = ggplot(post, aes_string(x = betas[i], y = 0, fill = paste0("abs(stat(x)) < 0.1"))) +
    stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
    # scale_x_continuous(limits = c(-1.0,1.3)) + # for async only
    scale_fill_manual(values = c(colors_paper[1], "gray80")) +
    geom_vline(xintercept = 0.1, linetype = "dashed") +
    geom_vline(xintercept = -0.1, linetype = "dashed") +
    # geom_vline(xintercept = 0, linetype = "dotdash", color = "black") +  # for async only
    # geom_vline(xintercept = 0.5, linetype = "dotdash", color = "grey") +  # for async only
    # geom_vline(xintercept = -0.5, linetype = "dotdash", color = "grey") + # for async only
    # geom_vline(xintercept = 1.0, linetype = "dotdash", color = "grey") +  # for async only
    # geom_vline(xintercept = -1.0, linetype = "dotdash", color = "grey") + # for async only
    theme_minimal() + 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = fontsize_paper, family = "Arial")) +
    xlab(xtitles[i])

    print(p)
    
    if(i==1){
      ggsave(figure_name[i], units="mm", width = 61.35, height = 40) # for all
    } else if (i==7){ # age
      ggsave(figure_name[i], units="mm", width = 42, height = 48) # for all
    }else if(i>1){
      ggsave(figure_name[i], units="mm", width = 31, height = 40) # for all
    }
}

# estimate of the effect over the different ordered answers
ce_sync = conditional_effects(model_fullExperimentalParameters_dem, categorical = T, effects = "Age")

ce_sync_plot <- plot(ce_sync, plot = TRUE)[[1]] 
ce_sync_plot + xlab("Age") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) 

ggsave("PH_ConditionalEffects_Age.svg", units="mm", width = 70, height = 60) 
  

# include below for duration
ce_sync_plot + 
  scale_y_continuous(limits = c(0, 0.80)) +
  # scale_x_discrete(labels=c("Standing" = "Original Robot", "Laying" = "MR-compatible robot"))+
  # scale_x_discrete(labels=c("None" = "None", "Robot manipulation" = "Yes"))+
  scale_colour_discrete(name = "PH rating", labels = c("0", "1", "2", "3", "4", "5", "6")) +
  scale_fill_discrete(name = "PH rating", labels = c("0", "1", "2", "3", "4", "5", "6")) + 
  facet_wrap("cats__")

figureName = "PH_ConditionalEffects_PreviousExposure.tiff"
ggsave(figureName, units="mm", width = 135, height = 135, dpi=500) # then divide by 3

# below for age, EHI and PDI
figureName = "PH_ConditionalEffects_Age.tiff"
ggsave(figureName, units="mm", width = 65, height = 60, dpi=500) 

i = 8
post %>% 
  ggplot(aes(x = `b_ConditionAsync:PositionLaying`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Condition_Position.svg"
ggsave(figureName, units="mm", width = 35, height = 40)

i = 9
post %>% 
  ggplot(aes(x = `b_ConditionAsync:LocationHand`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Location_Hand.svg"
ggsave(figureName, units="mm", width = 35, height = 40)

i = 10
post %>% 
  ggplot(aes(x = `b_ConditionAsync:Previous_ExposureRobotmanipulation`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.10, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Condition_PreviousExp.svg"
ggsave(figureName, units="mm", width = 35, height = 40)


i = 11
post %>% 
  ggplot(aes(x = `b_ConditionAsync:Cognitive_LoadYes`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Condition_CogLoad.svg"
ggsave(figureName, units="mm", width = 35, height = 40)


i = 12
post %>% 
  ggplot(aes(x = `b_ConditionAsync:Duration_sec`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Condition_Duration.svg"
ggsave(figureName, units="mm", width = 35, height = 40)


i = 13
post %>% 
  ggplot(aes(x = `b_ConditionAsync:Age`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Condition_Age.svg"
ggsave(figureName, units="mm", width = 42, height = 48)


# Plot: order -----------------------------------------------------------------------------------------------------

post_order = posterior_samples(model_fullExperimentalParameters_dem_order) %>%
  mutate(iter = 1:n())

post_order %>% 
  ggplot(aes(x = b_Order2, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 7, family = "Times New Roman")) +
  xlab("Order\n(second)")
figureName = "PH_Order.svg"
ggsave(figureName, units="mm", width = 35, height = 40)

post_order %>% 
  ggplot(aes(x = `b_ConditionAsync:Order2`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 7, family = "Times New Roman")) +
  xlab("Synchrony : Order\n(Async, Second)")
figureName = "PH_Interaction_Condition_Order.svg"
ggsave(figureName, units="mm", width = 35, height = 40)


# Plot EHI --------------------------------------------------------------------------------------------------------

post_EHI = posterior_samples(model_fullExperimentalParameters_dem_EHI) %>%
  mutate(iter = 1:n())

post_EHI %>% 
  ggplot(aes(x = `b_EHI`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 7, family = "Times New Roman")) +
  xlab("EHI\n(right-handedness)")

figureName = "PH_EHI.svg"
ggsave(figureName, units="mm", width = 42, height = 48, dpi=500)

post_EHI %>% 
  ggplot(aes(x = `b_ConditionAsync:EHI`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 7, family = "Times New Roman")) +
  xlab("Synchrony : EHI\n(Async, right-handedness)")

figureName = "PH_Interaction_EHI.svg"
ggsave(figureName, units="mm", width = 42, height = 48, dpi=500)



# Plot: PDI -------------------------------------------------------------------------------------------------------

post_PDI = posterior_samples(model_fullExperimentalParameters_dem_PDI) %>%
  mutate(iter = 1:n())

post_PDI %>% 
  ggplot(aes(x = `b_PDI`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  # geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Times New Roman")) +
  xlab("PDI\n(higher scores)")

figureName = "PH_PDI.svg"
ggsave(figureName, units="mm", width = 42, height = 48, dpi=500)

post_PDI %>% 
  ggplot(aes(x = `b_ConditionAsync:PDI`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Times New Roman")) +
  xlab("Synchrony : PDI\n(Async, higher scores)")

figureName = "PH_Interaction_PDI.svg"
ggsave(figureName, units="mm", width = 42, height = 48, dpi=500)


ce_sync = conditional_effects(model_fullExperimentalParameters_dem_PDI, categorical = T, effects = "PDI")

ce_sync_plot <- plot(ce_sync, plot = TRUE)[[1]]
ce_sync_plot + theme_minimal() +
  ylab("Rating probability") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_discrete(name = "PH rating", labels = c("0", "1", "2", "3", "4", "5", "6")) +
  scale_fill_discrete(name = "PH rating", labels = c("0", "1", "2", "3", "4", "5", "6")) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(size = fontsize_paper, family = "Times New Roman")) 

figureName = "PH_ConditionalEffects_PDI.svg"
ggsave(figureName, units="mm", width = 90, height = 75, dpi=500)



# Get overlaps HDI ROPE ---------------------------------------------------

library("bayestestR")
rope(model_fullExperimentalParameters_dem, range = c(-0.1, 0.1), ci = 1)
rope(model_fullExperimentalParameters_dem_order, range = c(-0.1, 0.1), ci = 1)
rope(model_fullExperimentalParameters_dem_EHI, range = c(-0.1, 0.1), ci = 1)
rope(model_fullExperimentalParameters_dem_PDI, range = c(-0.1, 0.1), ci = 1)



# Tests for mediation between PH PE LOA (Question ID 7, 6, 16) ------------------------
# /!\ HOW TO RUN THIS /!\
# PH and PE have more subjects than LOA, so first run PH and PE. Once the effects for PH/PE are obtained,
# use them as priors for LOA and re run to obtain LOA estimates.
# Comment and uncomment to run again

data_orig2 <- read_excel(filename, sheet="Data")
data_orig2$Subject_No        = factor(data_orig$Subject_No)
data_orig2$Gender_IsMale     = factor(data_orig$Gender_IsMale, labels = c("Female", "Male"))
data_orig2$Position          = factor(data_orig$Position, labels = c("Standing", "Laying"))
data_orig2$Location          = factor(data_orig$Location, labels = c("Back", "Hand"))
data_orig2$Order             = factor(data_orig$Order)
data_orig2$Previous_Exposure = factor(data_orig$Previous_Exposure, labels = c("None", "Robot manipulation"))
data_orig2$Cognitive_Load    = factor(data_orig$Cognitive_Load, labels = c("No", "Yes"))
data_orig2$Condition         = factor(data_orig$Condition, labels = c("Sync", "Async"))
data_orig2$Synchrony         = factor(data_orig$Synchrony, labels = c("Async", "Sync"))
data_orig2$Duration_sec      = scale(data_orig$Duration_sec)

# first PH PE (LoA has less participants)
selectedData <- filter(data_orig2, !is.na(`Question_ID_7`), !is.na(`Question_ID_6`))
selectedData <- filter(data_orig2, !is.na(`Question_ID_7`), !is.na(`Question_ID_6`), !is.na(`Question_ID_16`)) 

PH_async = filter(selectedData, !is.na(`Question_ID_7`), Synchrony == "Async")
PH_sync = filter(selectedData, !is.na(`Question_ID_7`), Synchrony == "Sync")
PH_diff = PH_async$`Question_ID_7`-PH_sync$`Question_ID_7`
PH_diff = PH_diff > 0
PH_diff = rep(PH_diff, each = 2)

PE_async = filter(selectedData, !is.na(`Question_ID_6`), Synchrony == "Async")
PE_sync = filter(selectedData, !is.na(`Question_ID_6`), Synchrony == "Sync")
PE_diff = PE_async$`Question_ID_6`-PE_sync$`Question_ID_6`
PE_diff = PE_diff > 0
PE_diff = rep(PE_diff, each = 2)

LoA_async = filter(selectedData, Synchrony == "Async")
LoA_sync = filter(selectedData, Synchrony == "Sync")
LoA_diff = LoA_async$`Question_ID_16`- LoA_sync$`Question_ID_16`
LoA_diff = LoA_diff > 0
LoA_diff = rep(LoA_diff, each = 2)


mediationFrame = selectedData
mediationFrame$sPH = factor(PH_diff)
mediationFrame$sPE = factor(PE_diff)
mediationFrame$sLoA = factor(LoA_diff)

mediationFrame$Question_ID_6  = factor(round(mediationFrame$`Question_ID_6`, digits = 0), ordered = TRUE)
mediationFrame$Question_ID_7  = factor(round(mediationFrame$`Question_ID_7`, digits = 0), ordered = TRUE)
mediationFrame$Question_ID_16 = factor(round(mediationFrame$`Question_ID_16`, digits = 0), ordered = TRUE)

# PH mediated by PE
formula_full_dem_PHmedPE = 
  as.formula(paste("Question_ID_7 ~ Condition + 
                   Position + Condition:Position + 
                   Location + Condition:Location +
                   Previous_Exposure + Condition:Previous_Exposure +
                   Cognitive_Load + Condition:Cognitive_Load +
                   Duration_sec + Duration_sec:Condition +
                   Age + Age:Condition +
                   Gender_IsMale + 
                   sPE + sPE:Condition +
                   (1|Experiment_ID)"))
## for LOA add: sLoA + sLoA:Condition +
## for LoA:PE interaction add: sLoA:sPE:Condition +
## for LoA remove:                    Cognitive_Load + Condition:Cognitive_Load +


model_fullExperimentalParameters_dem_PHmedPE = 
  brm(formula_full_dem_PHmedPE, 
      data = mediationFrame,
      family = cumulative("probit", threshold = "flexible"),
      prior = c(set_prior("normal(-0.4220, 0.1367)", class = "Intercept", coef = "1"),
                set_prior("normal(-0.0520, 0.1365)", class = "Intercept", coef = "2"),
                set_prior("normal(0.2973, 0.1366)", class = "Intercept", coef = "3"),
                set_prior("normal(0.5935, 0.1371)", class = "Intercept", coef = "4"),
                set_prior("normal(1.0518, 0.1388)", class = "Intercept", coef = "5"),
                set_prior("normal(1.5836, 0.1429)", class = "Intercept", coef = "6"),
                set_prior("normal(0.3406, 0.0908)", coef = "ConditionAsync"),
                set_prior("normal(-0.6545, 0.2808)", coef = "PositionLaying"),
                set_prior("normal(-0.6079, 0.1740)", coef = "LocationHand"),
                set_prior("normal(-0.0983, 0.1524)", coef = "Previous_ExposureRobotmanipulation"),
                set_prior("normal(0.1719, 0.4473)", coef = "Cognitive_LoadYes"), # remove for LoA
                set_prior("normal(-0.0299, 0.1619)", coef = "Duration_sec"),
                set_prior("normal(-0.0846, 0.0470)", coef = "Age"),
                set_prior("normal(-0.0732, 0.0640)", coef = "Gender_IsMaleMale"),
                # set_prior("normal(-0.1047, 0.0889)", coef = "sPETRUE"), # uncomment for LoA
                # set_prior("normal(0.5165, 0.1204)", coef = "ConditionAsync:sPETRUE"), # uncomment for LoA
                set_prior("normal(0.0651, 0.2104)", coef = "ConditionAsync:PositionLaying"),
                set_prior("normal(0.0023, 0.2130)", coef = "ConditionAsync:LocationHand"),
                set_prior("normal(-0.1607, 0.1542)", coef = "ConditionAsync:Previous_ExposureRobotmanipulation"),
                set_prior("normal(-0.2214, 0.2986)", coef = "ConditionAsync:Cognitive_LoadYes"), # remove for LoA
                set_prior("normal(0.0018, 0.1146)", coef = "ConditionAsync:Duration_sec"),
                set_prior("normal(-0.0840, 0.0641)", coef = "ConditionAsync:Age")),
      warmup = 2000, iter = 8000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13), init = 0)

post_PHmedByPE = posterior_samples(model_fullExperimentalParameters_dem_PHmedPE) %>%
  mutate(iter = 1:n())
mode_hdi(post_PHmedByPE$`b_ConditionAsync:sPETRUE`, .width = 0.89)
mode_hdi(post_PHmedByPE$`b_ConditionAsync:sLoATRUE`, .width = 0.89)
mode_hdi(post_PHmedByPE$`b_ConditionAsync:sPETRUE:sLoATRUE`, .width = 0.89)

post_PHmedByPE$`b_ConditionAsync:sPETRUE:sLoATRUE`
post_PHmedByPE %>% 
  ggplot(aes(x = `b_ConditionAsync:sPETRUE`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab("Synchrony : riPE\n(Async, +)")

figureName = "PH_Interaction_Condition_PE.svg"
ggsave(figureName, units="mm", width = 42, height = 48)

# PH med by LoA
post_PHmedByPE %>% 
  ggplot(aes(x = `b_ConditionAsync:sLoATRUE`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab("Synchrony : agency\n(Async, +)")

figureName = "PH_Interaction_Condition_LoA.svg"
ggsave(figureName, units="mm", width = 42, height = 48)

# plot PH med by riPE LoA
post_PHmedByPE %>% 
  ggplot(aes(x = `b_ConditionAsync:sPETRUE:sLoATRUE`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab("Synchrony : agency : riPE\n(Async, +, +)")

figureName = "PH_Interaction_Condition_LoA_riPE.svg"
ggsave(figureName, units="mm", width = 42, height = 48)


# PE mediated by PH
# NEED TO RE-RUN DATA2 and MEDIATION FRAME
formula_full_dem_PEmedPH = 
  as.formula(paste("Question_ID_6 ~ Condition + 
                   Position + Condition:Position + 
                   Previous_Exposure + Condition:Previous_Exposure +
                   Duration_sec + Duration_sec:Condition +
                   Age + Age:Condition +
                   sPH + sPH:Condition +
                   sLoA + sLoA:Condition +
                   Gender_IsMale + 
                   (1|Experiment_ID)"))
## for LOA add: sLoA + sLoA:Condition +
## for LoA:PE interaction add: sLoA:sPH:Condition
##                    Cognitive_Load + Condition:Cognitive_Load +

model_fullExperimentalParameters_dem_PEmedPH = 
  brm(formula_full_dem_PEmedPH, 
      data = mediationFrame,
      family = cumulative("probit", threshold = "flexible"),
      prior = c(set_prior("normal(-0.4619, 0.1458)", class = "Intercept", coef = "1"),
                set_prior("normal(-0.0875, 0.1457)", class = "Intercept", coef = "2"),
                set_prior("normal(0.2718, 0.1458)", class = "Intercept", coef = "3"),
                set_prior("normal(0.5751, 0.1463)", class = "Intercept", coef = "4"),
                set_prior("normal(1.0508, 0.1482)", class = "Intercept", coef = "5"),
                set_prior("normal(1.5837, 0.1522)", class = "Intercept", coef = "6"),
                set_prior("normal(0.3431, 0.0925)", coef = "ConditionAsync"),
                set_prior("normal(-0.6436, 0.3023)", coef = "PositionLaying"),
                set_prior("normal(-0.1328, 0.1649)", coef = "Previous_ExposureRobotmanipulation"),
                # set_prior("normal(0.1223, 0.4881)", coef = "Cognitive_LoadYes"),
                set_prior("normal(-0.0142, 0.1751)", coef = "Duration_sec"),
                set_prior("normal(-0.0627, 0.0501)", coef = "Age"),
                set_prior("normal(-0.0971, 0.0673)", coef = "Gender_IsMaleMale"),
                set_prior("normal(-0.0901, 0.0885)", coef = "sPHTRUE"), # uncomment for LoA
                set_prior("normal(0.6589, 0.1220)", coef = "ConditionAsync:sPHTRUE"), # uncomment for LoA
                set_prior("normal(0.0540, 0.2145)", coef = "ConditionAsync:PositionLaying"),
                set_prior("normal(-0.1462, 0.1639)", coef = "ConditionAsync:Previous_ExposureRobotmanipulation"),
                # set_prior("normal(-0.2256, 0.3094)", coef = "ConditionAsync:Cognitive_LoadYes"),
                set_prior("normal(0.0006, 0.1155)", coef = "ConditionAsync:Duration_sec"),
                set_prior("normal(-0.0853, 0.0678)", coef = "ConditionAsync:Age")),
      warmup = 2000, iter = 8000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13),
      inits = "0")

post_PEmedPH = posterior_samples(model_fullExperimentalParameters_dem_PEmedPH) %>%
  mutate(iter = 1:n())

mode_hdi(post_PEmedPH$`b_ConditionAsync:sPHTRUE`, .width = 0.89)
mean(post_PEmedPH$b_sPHTRUE)
sd(post_PEmedPH$b_sPHTRUE)
mean(post_PEmedPH$`b_ConditionAsync:sPHTRUE`)
sd(post_PEmedPH$`b_ConditionAsync:sPHTRUE`)



# Compare using priors on scale versus not using --------------------------

# simulate data based on priors

library(crayon)

N_simulations    = 15
results_priors   = c()
results_nopriors = c()
results_flag     = c()

for(isim in c(1:N_simulations)){
  
  cat(red("Running simulation number ", isim, "\n"))
  
  # how many participants: random 
  N = round(runif(1, min = 20, max=25))
  # set experimental parameters
  wall     = round(runif(1)) * rnorm(1, mean(post$b_Force_fieldYes), sd(post$b_Force_fieldYes))
  exposure = round(runif(1)) * rnorm(1, mean(post$b_Previous_ExposureRobotmanipulation), sd(post$b_Previous_ExposureRobotmanipulation))
  duration = round(runif(1)) * rnorm(1, mean(post$b_Duration_sec), sd(post$b_Duration_sec))
  robot    = round(runif(1)) * rnorm(1, mean(post$b_PositionLaying), sd(post$b_PositionLaying))
  
  simSet = data.frame(Subject_No = c(1:N),
                      Condition  = c(1:N),
                      Response   = c(1:N))
  Subject_No = c(0)
  Condition = c(0)
  Location = c(0)
  Load = c(0)
  Response = c(0)
  
  for(iN in c(1:N)){
    
    condition_set = c(0,1,0,1,0,1,0,1)
    load_set      = c(0,0,1,1,0,0,1,1)
    location_set  = c(0,0,0,0,1,1,1,1)
    
    subject_bias = rnorm(1,0,0.25)
    
    for(j in c(1:8)){
      Subject_No[iN*8+(-8+j)] = iN
      Condition[iN*8+(-8+j)] = condition_set[j]
      Location[iN*8+(-8+j)] = location_set[j]
      Load[iN*8+(-8+j)] = load_set[j]
      
      Response[iN*8+(-8+j)] = rnorm(1, 0, 1) + subject_bias +
        condition_set[j] * rnorm(1, mean(post$b_ConditionAsync), sd(post$b_ConditionAsync)) + # condition
        wall * rnorm(1, mean(post$b_Force_fieldYes), sd(post$b_Force_fieldYes)) + # virtual wall
        exposure * rnorm(1, mean(post$b_Previous_ExposureRobotmanipulation), sd(post$b_Previous_ExposureRobotmanipulation)) + # previous exposure
        location_set[j] * rnorm(1, mean(post$b_LocationHand), sd(post$b_LocationHand)) + # location (hand vs back)
        duration * rnorm(1, mean(post$b_Duration_sec), sd(post$b_Duration_sec)) + # duration
        load_set[j] * rnorm(1, mean(post$b_Cognitive_LoadYes), sd(post$b_Cognitive_LoadYes)) + # cogn load
        robot * rnorm(1, mean(post$b_PositionLaying), sd(post$b_PositionLaying)) # robot type
      
    }
    
  }
  Response = case_when(
    Response < mean(post$`b_Intercept[1]`) ~ 0,
    Response < mean(post$`b_Intercept[2]`) ~ 1,
    Response < mean(post$`b_Intercept[3]`) ~ 2,
    Response < mean(post$`b_Intercept[4]`) ~ 3,
    Response < mean(post$`b_Intercept[5]`) ~ 4,
    Response < mean(post$`b_Intercept[6]`) ~ 5,
    Response >= mean(post$`b_Intercept[1]`) ~ 6)
  
  
  simSet = data.frame(Subject_No = factor(Subject_No),
                      Condition = factor(Condition),
                      Location = factor(Location),
                      Load = factor(Load),
                      Rating = factor(round(Response), ordered = TRUE))
  
  formula_simData = 
    as.formula("Rating ~ Condition + Location + Load + Load:Condition + (1|Subject_No)")
  
  model_simSet = 
    brm(formula_simData, 
        data = simSet,
        family = cumulative("probit"),
        warmup = 100, iter = 1000,
        cores = 2, chains = 2, 
        control = list(max_treedepth = 13),
        init = "random", seed = 123)
  
  post_simData = posterior_samples(model_simSet) %>%
    mutate(iter = 1:n())
  

  priors = c(
    prior(normal(-0.42, 0.14), class = Intercept, coef = 1),
    prior(normal(-0.05, 0.14), class = Intercept, coef = 2),
    prior(normal(0.30, 0.14), class = Intercept, coef = 3),
    prior(normal(0.59, 0.14), class = Intercept, coef = 4),
    prior(normal(1.05, 0.14), class = Intercept, coef = 5),
    prior(normal(1.59, 0.15), class = Intercept, coef = 6))
  
  model_simSet_priors = 
    brm(formula_simData, 
        data = simSet,
        family = cumulative("probit"),
        prior = priors,
        warmup = 100, iter = 1000,
        cores = 2, chains = 2, 
        control = list(max_treedepth = 13),
        init = "random", seed = 123)
  
  post_simData_priors = posterior_samples(model_simSet_priors) %>%
    mutate(iter = 1:n())
  
  results_priors[isim]   = mode_hdi(post_simData_priors$b_Condition1)[1]
  results_nopriors[isim] = mode_hdi(post_simData$b_Condition1)[1]
  if(abs(mode_hdi(post_simData_priors$b_Condition1)[1] - mode_hdi(post$b_ConditionAsync)[1]) - 
     abs(mode_hdi(post_simData$b_Condition1)[1] - mode_hdi(post$b_ConditionAsync)[1]) < 0){
    results_flag[isim] = TRUE
  } else {
    results_flag[isim] = FALSE
  }
  
}


# Simulating based on brms functions --------------------------------------
# see: https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/

library(ordinal)
library(RVAideMemoire)

selectedData = filter(data_orig, !is.na(Age))
selectedData$Age = (selectedData$Age-mean(selectedData$Age))/sd(selectedData$Age)
formula_test = 
  as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
                   "~ Condition + Location + Condition:Location +
                   Position + Condition:Position + 
                   Previous_Exposure + Condition:Previous_Exposure +
                   Cognitive_Load + Condition:Cognitive_Load +
                   Duration_sec + Condition:Duration_sec +
                   Force_field + Force_field:Condition +
                   Age + Age:Condition +
                   Gender_IsMale + (1+Condition|Experiment_ID)"))
print(formula_test)

priors = c(
  prior(normal(-0.42, 0.14), class = Intercept, coef = 1),
  prior(normal(-0.05, 0.14), class = Intercept, coef = 2),
  prior(normal(0.30, 0.14), class = Intercept, coef = 3),
  prior(normal(0.59, 0.14), class = Intercept, coef = 4),
  prior(normal(1.05, 0.14), class = Intercept, coef = 5),
  prior(normal(1.59, 0.15), class = Intercept, coef = 6),
  prior(normal(0.33,0.11), coef = ConditionAsync),
  prior(normal(-0.61,0.18), coef = LocationHand),
  prior(normal(-0.66,0.28), coef = PositionLaying),
  prior(normal(-0.10,0.16), coef = Previous_ExposureRobotmanipulation),
  prior(normal(0.18,0.45), coef = Cognitive_LoadYes),
  prior(normal(-0.04,0.16), coef = Duration_sec),
  prior(normal(-0.03,0.36), coef = Force_fieldYes),
  prior(normal(-0.08,0.05), coef = Age),
  prior(normal(-0.07,0.06), coef = Gender_IsMaleMale))

model_test =  
  brm(formula_test, 
      data = selectedData,
      family = cumulative("probit", threshold = "flexible"),
      prior = priors,
      # sample_prior = "only",
      warmup = 1000, iter = 10000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13),
      init = "random", seed = 123)

post_model_test = posterior_samples(model_test) %>%
  mutate(iter = 1:n())
mean(post_model_test$b_ConditionAsync)


N_simulations    = 100
results_priors   = c()
results_nopriors = c()
results_flag     = c()
results_anova    = c()

# always the same
formula_meta_test = 
  as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
                   "~ Condition + Location + Location:Condition + (1|Subject_No)"))
print(formula_meta_test)

priors = c(
  prior(normal(-0.42, 0.14), class = Intercept, coef = 1),
  prior(normal(-0.05, 0.14), class = Intercept, coef = 2),
  prior(normal(0.30, 0.14), class = Intercept, coef = 3),
  prior(normal(0.59, 0.14), class = Intercept, coef = 4),
  prior(normal(1.05, 0.14), class = Intercept, coef = 5),
  prior(normal(1.59, 0.15), class = Intercept, coef = 6))

position_opts = c("Standing", "Laying")
load_opts     = c("No", "Yes")
wall_opts     = c("No", "Yes")
gender_opts   = c("Female", "Male")


while(length(results_flag) <= N_simulations){
  
  try({
  
    Ndraws = round(runif(1, 85, 120))
    
    testData = subset(selectedData, select = c(Condition, Location, Experiment_ID, Position, Previous_Exposure,
                                               Cognitive_Load, Duration_sec, Force_field, Age, Gender_IsMale))
    testData = testData[1:8,]
    
    testData$Location          = c("Back", "Back", "Hand", "Hand", "Back", "Back", "Hand", "Hand")
    testData$Experiment_ID     = rep("newExp", 8)
    testData$Previous_Exposure = c(rep("None", 4), rep("Robot manipulation", 4))
    
    # these vary randomly
    testData$Position       = rep(position_opts[round(runif(1,1,2))], 8)
    testData$Cognitive_Load = rep(load_opts[round(runif(1,1,2))], 8)
    testData$Force_field    = rep(wall_opts[round(runif(1,1,2))], 8)
    testData$Gender_IsMale  = rep(gender_opts[round(runif(1,1,2))], 8)
    
    # A = posterior_predict(model_test, newdata = testData)
    simData = predicted_draws(model_test, newdata = testData, ndraws = Ndraws, allow_new_levels = TRUE)
    colnames(simData)[ncol(simData)] <- "Question_ID_7"
    simData$Question_ID_7 = factor(simData$Question_ID_7, ordered = TRUE)
    simData$Subject_No = factor(rep(c(1:Ndraws),8))
    
    # actual testing of performance
    meta_test1 = brm(formula_meta_test, 
                     data = simData,
                     family = cumulative("probit"),#, threshold = "flexible"),
                     prior = priors,
                     warmup = 1000, iter = 10000,
                     cores = 4, chains = 4, 
                     control = list(max_treedepth = 13),
                     init = "random", seed = 123)
    
    meta_test2 = brm(formula_meta_test, 
                     data = simData,
                     family = cumulative("probit", threshold = "flexible"),
                     warmup = 1000, iter = 10000,
                     cores = 4, chains = 4, 
                     control = list(max_treedepth = 13),
                     init = "random", seed = 123)
    
    post_metatest1 = posterior_samples(meta_test1) %>%
      mutate(iter = 1:n())
    mean(post_metatest1$b_ConditionAsync)
    
    post_metatest2 = posterior_samples(meta_test2) %>%
      mutate(iter = 1:n())
    mean(post_metatest2$b_ConditionAsync)
    
    model_full = clmm(Question_ID_7 ~ Condition + (1|Subject_No), 
                      data = simData, threshold = "flexible", link = "probit")
    results_anova[length(results_anova)+1] = Anova.clmm(model_full)[3] < 0.05
    
    results_priors[length(results_priors)+1]   = mode_hdi(post_metatest1$b_ConditionAsync)[1]
    results_nopriors[length(results_nopriors)+1] = mode_hdi(post_metatest2$b_ConditionAsync)[1]
    if(abs(mode_hdi(post_metatest1$b_ConditionAsync)[1] - mode_hdi(post_model_test$b_ConditionAsync)[1]) - 
       abs(mode_hdi(post_metatest2$b_ConditionAsync)[1] - mode_hdi(post_model_test$b_ConditionAsync)[1]) < 0){
      results_flag[length(results_flag)+1] = TRUE
    } else {
      results_flag[length(results_flag)+1] = FALSE
    }
  }, silent = TRUE)
  
  
}


