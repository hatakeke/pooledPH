# ==============================================================================
#  13_simulation.R - シミュレーション分析
# ==============================================================================
#  概要: 事前分布の効果を検証するシミュレーション
#  依存: 00_setup.R, 01_main_model.R
#  出力: シミュレーション結果
# ==============================================================================

library(crayon)
library(tidyverse)
library(brms)
library(tidybayes)
library(ordinal)
library(RVAideMemoire)

# ==============================================================================
#  シミュレーション関数（手動データ生成）
# ==============================================================================

#' 事前分布効果のシミュレーション
#' @param post メインモデルの事後サンプル
#' @param N_simulations シミュレーション回数
#' @return 結果リスト
run_prior_effect_simulation <- function(post, N_simulations = 15) {
    
    results_priors <- c()
    results_nopriors <- c()
    results_flag <- c()
    
    for (isim in seq_len(N_simulations)) {
        
        cat(red("Running simulation number ", isim, "\n"))
        
        # 被験者数：ランダム
        N <- round(runif(1, min = 20, max = 25))
        
        # 実験パラメータ設定
        wall <- round(runif(1)) * rnorm(1, mean(post$b_Force_fieldYes), sd(post$b_Force_fieldYes))
        exposure <- round(runif(1)) * rnorm(1, mean(post$b_Previous_ExposureRobotmanipulation), sd(post$b_Previous_ExposureRobotmanipulation))
        duration <- round(runif(1)) * rnorm(1, mean(post$b_Duration_sec), sd(post$b_Duration_sec))
        robot <- round(runif(1)) * rnorm(1, mean(post$b_PositionLaying), sd(post$b_PositionLaying))
        
        Subject_No <- c(0)
        Condition <- c(0)
        Location <- c(0)
        Load <- c(0)
        Response <- c(0)
        
        for (iN in seq_len(N)) {
            
            condition_set <- c(0, 1, 0, 1, 0, 1, 0, 1)
            load_set <- c(0, 0, 1, 1, 0, 0, 1, 1)
            location_set <- c(0, 0, 0, 0, 1, 1, 1, 1)
            
            subject_bias <- rnorm(1, 0, 0.25)
            
            for (j in seq_len(8)) {
                idx <- iN * 8 + (-8 + j)
                Subject_No[idx] <- iN
                Condition[idx] <- condition_set[j]
                Location[idx] <- location_set[j]
                Load[idx] <- load_set[j]
                
                Response[idx] <- rnorm(1, 0, 1) + subject_bias +
                condition_set[j] * rnorm(1, mean(post$b_ConditionAsync), sd(post$b_ConditionAsync)) +
                wall * rnorm(1, mean(post$b_Force_fieldYes), sd(post$b_Force_fieldYes)) +
                exposure * rnorm(1, mean(post$b_Previous_ExposureRobotmanipulation), sd(post$b_Previous_ExposureRobotmanipulation)) +
                location_set[j] * rnorm(1, mean(post$b_LocationHand), sd(post$b_LocationHand)) +
                duration * rnorm(1, mean(post$b_Duration_sec), sd(post$b_Duration_sec)) +
                load_set[j] * rnorm(1, mean(post$b_Cognitive_LoadYes), sd(post$b_Cognitive_LoadYes)) +
                robot * rnorm(1, mean(post$b_PositionLaying), sd(post$b_PositionLaying))
            }
        }
    
        # 応答のカテゴリ化
        Response <- case_when(
            Response < mean(post$`b_Intercept[1]`) ~ 0,
            Response < mean(post$`b_Intercept[2]`) ~ 1,
            Response < mean(post$`b_Intercept[3]`) ~ 2,
            Response < mean(post$`b_Intercept[4]`) ~ 3,
            Response < mean(post$`b_Intercept[5]`) ~ 4,
            Response < mean(post$`b_Intercept[6]`) ~ 5,
            Response >= mean(post$`b_Intercept[1]`) ~ 6
        )
        
        simSet <- data.frame(
            Subject_No = factor(Subject_No),
            Condition = factor(Condition),
            Location = factor(Location),
            Load = factor(Load),
            Rating = factor(round(Response), ordered = TRUE)
        )
        
        formula_simData <- as.formula("Rating ~ Condition + Location + Load + Load:Condition + (1|Subject_No)")
        
        # 事前分布なしモデル
        model_simSet <- brm(
            formula_simData, 
            data = simSet,
            family = cumulative("probit"),
            warmup = 100, 
            iter = 1000,
            cores = 2, 
            chains = 2, 
            control = list(max_treedepth = 13),
            init = "random", 
            seed = 123
        )
        
        post_simData <- posterior_samples(model_simSet) %>%
            mutate(iter = 1:n())
        
        # 事前分布ありモデル
        priors <- c(
            prior(normal(-0.42, 0.14), class = Intercept, coef = 1),
            prior(normal(-0.05, 0.14), class = Intercept, coef = 2),
            prior(normal(0.30, 0.14), class = Intercept, coef = 3),
            prior(normal(0.59, 0.14), class = Intercept, coef = 4),
            prior(normal(1.05, 0.14), class = Intercept, coef = 5),
            prior(normal(1.59, 0.15), class = Intercept, coef = 6)
        )
        
        model_simSet_priors <- brm(
            formula_simData, 
            data = simSet,
            family = cumulative("probit"),
            prior = priors,
            warmup = 100, 
            iter = 1000,
            cores = 2, 
            chains = 2, 
            control = list(max_treedepth = 13),
            init = "random", 
            seed = 123
        )
        
        post_simData_priors <- posterior_samples(model_simSet_priors) %>%
            mutate(iter = 1:n())
        
        results_priors[isim] <- mode_hdi(post_simData_priors$b_Condition1)[1]
        results_nopriors[isim] <- mode_hdi(post_simData$b_Condition1)[1]
        
        if (abs(mode_hdi(post_simData_priors$b_Condition1)[1] - mode_hdi(post$b_ConditionAsync)[1]) - 
            abs(mode_hdi(post_simData$b_Condition1)[1] - mode_hdi(post$b_ConditionAsync)[1]) < 0) {
            results_flag[isim] <- TRUE
        } else {
            results_flag[isim] <- FALSE
        }
    }
    
    return(list(
        priors = results_priors,
        nopriors = results_nopriors,
        flag = results_flag
    ))
}

# ==============================================================================
#  brms関数ベースのシミュレーション
# ==============================================================================

#' brms予測ベースのシミュレーション
#' @param model_test 参照モデル
#' @param selectedData 選択済みデータ
#' @param N_simulations シミュレーション回数
#' @param questionToColumn 質問列マッピング
#' @param iquest 質問インデックス
#' @return 結果リスト
run_brms_based_simulation <- function(
    model_test, 
    selectedData,
    N_simulations = 100,
    questionToColumn,
    iquest
    ) {
    
    results_priors <- c()
    results_nopriors <- c()
    results_flag <- c()
    results_anova <- c()
    
    # 共通の公式
    formula_meta_test <- as.formula(paste(
        colnames(selectedData)[questionToColumn[iquest]],
        "~ Condition + Location + Location:Condition + (1|Subject_No)"
    ))
    print(formula_meta_test)
    
    priors <- c(
        prior(normal(-0.42, 0.14), class = Intercept, coef = 1),
        prior(normal(-0.05, 0.14), class = Intercept, coef = 2),
        prior(normal(0.30, 0.14), class = Intercept, coef = 3),
        prior(normal(0.59, 0.14), class = Intercept, coef = 4),
        prior(normal(1.05, 0.14), class = Intercept, coef = 5),
        prior(normal(1.59, 0.15), class = Intercept, coef = 6)
    )
    
    position_opts <- c("Standing", "Laying")
    load_opts <- c("No", "Yes")
    wall_opts <- c("No", "Yes")
    gender_opts <- c("Female", "Male")
    
    post_model_test <- posterior_samples(model_test) %>%
        mutate(iter = 1:n())
    
    while (length(results_flag) <= N_simulations) {
        
        try({
            
            Ndraws <- round(runif(1, 85, 120))
            
            testData <- subset(
                selectedData, 
                select = c(
                    Condition, 
                    Location, 
                    Experiment_ID,
                    Position, 
                    Previous_Exposure, 
                    Cognitive_Load, 
                    Duration_sec, 
                    Force_field, 
                    Age, 
                    Gender_IsMale
                )
            )
            testData <- testData[1:8, ]
            
            testData$Location <- c("Back", "Back", "Hand", "Hand", "Back", "Back", "Hand", "Hand")
            testData$Experiment_ID <- rep("newExp", 8)
            testData$Previous_Exposure <- c(rep("None", 4), rep("Robot manipulation", 4))
            
            # ランダム変動
            testData$Position <- rep(position_opts[round(runif(1, 1, 2))], 8)
            testData$Cognitive_Load <- rep(load_opts[round(runif(1, 1, 2))], 8)
            testData$Force_field <- rep(wall_opts[round(runif(1, 1, 2))], 8)
            testData$Gender_IsMale <- rep(gender_opts[round(runif(1, 1, 2))], 8)
            
            # 予測データ生成
            simData <- predicted_draws(model_test, newdata = testData, ndraws = Ndraws, allow_new_levels = TRUE)
            colnames(simData)[ncol(simData)] <- "Question_ID_7"
            simData$Question_ID_7 <- factor(simData$Question_ID_7, ordered = TRUE)
            simData$Subject_No <- factor(rep(seq_len(Ndraws), 8))
            
            # 事前分布ありモデル
            meta_test1 <- brm(
                formula_meta_test, 
                data = simData,
                family = cumulative("probit"),
                prior = priors,
                warmup = 1000, 
                iter = 10000,
                cores = 4, 
                chains = 4, 
                control = list(max_treedepth = 13),
                init = "random", 
                seed = 123
            )
            
            # 事前分布なしモデル
            meta_test2 <- brm(
                formula_meta_test, 
                data = simData,
                family = cumulative("probit", threshold = "flexible"),
                warmup = 1000, 
                iter = 10000,
                cores = 4, 
                chains = 4, 
                control = list(max_treedepth = 13),
                init = "random", 
                seed = 123
            )
            
            post_metatest1 <- posterior_samples(meta_test1) %>%
                mutate(iter = 1:n())
            
            post_metatest2 <- posterior_samples(meta_test2) %>%
                mutate(iter = 1:n())
            
            # 頻度論的検定
            model_full <- clmm(
                Question_ID_7 ~ Condition + (1|Subject_No), 
                data = simData, 
                threshold = "flexible", 
                link = "probit"
            )
            results_anova[length(results_anova) + 1] <- Anova.clmm(model_full)[3] < 0.05
            
            results_priors[length(results_priors) + 1] <- mode_hdi(post_metatest1$b_ConditionAsync)[1]
            results_nopriors[length(results_nopriors) + 1] <- mode_hdi(post_metatest2$b_ConditionAsync)[1]
            
            if (abs(mode_hdi(post_metatest1$b_ConditionAsync)[1] - mode_hdi(post_model_test$b_ConditionAsync)[1]) - 
                    abs(mode_hdi(post_metatest2$b_ConditionAsync)[1] - mode_hdi(post_model_test$b_ConditionAsync)[1]) < 0) {
                results_flag[length(results_flag) + 1] <- TRUE
            } else {
                results_flag[length(results_flag) + 1] <- FALSE
            }
            
        }, silent = TRUE)
    }
    
    return(list(
        priors = results_priors,
        nopriors = results_nopriors,
        flag = results_flag,
        anova = results_anova
    ))
}

#' シミュレーション結果のサマリー
#' @param results シミュレーション結果リスト
#' @return サマリーデータフレーム
summarize_simulation_results <- function(results) {
    
    summary_df <- data.frame(
        metric = c(
            "Mean (with priors)", 
            "Mean (without priors)", 
            "SD (with priors)", 
            "SD (without priors)",
            "% better with priors"
        ),
        value = c(
            mean(results$priors, na.rm = TRUE),
            mean(results$nopriors, na.rm = TRUE),
            sd(results$priors, na.rm = TRUE),
            sd(results$nopriors, na.rm = TRUE),
            mean(results$flag, na.rm = TRUE) * 100
        )
    )
    
    return(summary_df)
}

# ==============================================================================
#  実行（コメントアウト - 必要に応じて実行）
# ==============================================================================

# 以下のコードは非常に時間がかかるため、必要に応じて個別に実行してください

# if (exists("post") && exists("model_fullExperimentalParameters_dem")) {
#     cat("\n====== Running Prior Effect Simulation ======\n")
#     
#     # 手動シミュレーション
#     sim_results <- run_prior_effect_simulation(post, N_simulations = 15)
#     
#     # 結果サマリー
#     sim_summary <- summarize_simulation_results(sim_results)
#     print(sim_summary)
#     
#     cat("====== Simulation completed ======\n")
# }

cat("====== Simulation module loaded ======\n")
cat("Available functions:\n")
cat("  - run_prior_effect_simulation()\n")
cat("  - run_brms_based_simulation()\n")
cat("  - summarize_simulation_results()\n")