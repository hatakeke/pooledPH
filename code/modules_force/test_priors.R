# ==============================================================================
#  test_priors.R - 事前分布生成だけを確認（フィット無し）
# ==============================================================================

source("00_setup.R")
source("01_priors_from_posterior.R")

selectedData <- dplyr::filter(data_orig, !is.na(Age))
selectedData$Age <- (selectedData$Age - mean(selectedData$Age)) / sd(selectedData$Age)

formula_force <- as.formula(paste(
    colnames(data_orig)[questionToColumn[iquest]],
    "~ Condition +
    Position + Condition:Position +
    Location + Condition:Location +
    Previous_Exposure + Condition:Previous_Exposure +
    Cognitive_Load + Condition:Cognitive_Load +
    Duration_sec + Condition:Duration_sec +
    Force_field + Condition:Force_field +
    Age + Age:Condition +
    Gender_IsMale +
    (1 + Condition | Experiment_ID) +
    (1 | Subject_No)"
))

family_force <- brms::cumulative("probit", threshold = "flexible")

priors_force <- make_priors_from_posterior_draws(
    posterior_draws_path = posterior_draws_path,
    formula = formula_force,
    data = selectedData,
    family = family_force,
    output_dir = output_dir
)

cat("\nGenerated priors:", length(priors_force), "entries\n")
print(head(priors_force, 10))
