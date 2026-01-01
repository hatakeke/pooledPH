# ==============================================================================
#  00_setup.R - セットアップと設定
# ==============================================================================
#  概要: ライブラリ読み込み、データ読み込み、前処理を行う
#  依存: なし
#  出力: data_orig (前処理済みデータフレーム)
# ==============================================================================

# メモリクリア
gc()

# データディレクトリ設定
dataDir <- "./R/pooledPH/data"

# 出力フォルダ設定
output_dir <- "outputs"
if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
    cat("Created output directory:", output_dir, "\n")
}

# ライブラリ読み込み
library(tidyverse)    # data manipulation
library(readxl)       # read excel
library(ggplot2)      # plot
library(brms)         # analysis
library(tidybayes)    # data manipulation
library(RColorBrewer) # needed for some extra colours in one of the graphs

# ==============================================================================
#  研究ラベル定義
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
#  質問項目の定義
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
#  可視化用設定
# ==============================================================================
colors_paper <- c("#d4bbff", "#4589ff")
colors_ppt <- c("#007380", "#00a79f")

fontsize_paper <- 7
fontsize_ppt <- 24

# EPFL カラー
EPFL_canard <- "#007480"
EPFL_leman <- "#00A79F"

# ==============================================================================
#  ヘルパー関数
# ==============================================================================

#' データサマリー関数
#' @param x 数値ベクトル
#' @return y, ymin, ymaxを含む名前付きベクトル
data_summary <- function(x) {
    m <- mean(x)
    ymin <- m - sd(x)
    ymax <- m + sd(x)
    if (ymax > 6) ymax <- 6
    if (ymin < 0) ymin <- 0
    return(c(y = m, ymin = ymin, ymax = ymax))
}

#' 最頻値計算関数
#' @param x ベクトル
#' @return 最頻値
Mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

# ==============================================================================
#  データ読み込み
# ==============================================================================
filename <- "../../data/PooledData.xlsx"

load_and_preprocess_data <- function(filename) {
    # データ読み込み
    data_orig <- read_excel(filename, sheet = "Data")
    
    # Sync/Async一時データ（後で使用）
    temp_async <- filter(data_orig, Synchrony == 0)
    temp_sync <- filter(data_orig, Synchrony == 1)
    
    # 被験者数
    noSubjects <- data_orig$Subject_No
    noSubjects <- noSubjects[length(noSubjects)]
    
    # Factor変換
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
    data_orig$Duration_sec <- scale(data_orig$Duration_sec)
    
    # 質問項目をordinal factorに変換
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

# データ読み込み実行
data_list <- load_and_preprocess_data(filename)
data_orig <- data_list$data_orig
temp_async <- data_list$temp_async
temp_sync <- data_list$temp_sync
noSubjects <- data_list$noSubjects

# デフォルトの質問インデックス（PH = 7）
# 1: no body (322) | 2: self-touch (554) | 5: more than one body (437) 
# 6: PE (580) | 7: PH (580) | 16: agency (189)
iquest <- 7

sl <- RColorBrewer::brewer.pal(9, 'Set1')
print(paste0(":::::::::: Starting analysis for question ", iquest, ": ", questionsDescription[iquest], " ::::::::::"))

cat("\n====== Setup completed successfully ======\n")
cat("Data loaded:", nrow(data_orig), "rows,", ncol(data_orig), "columns\n")
cat("Selected question:", iquest, "-", questionsDescription[iquest], "\n")
