# ==============================================================================
#  00_setup.R - セットアップと設定
# ==============================================================================
#  概要: ライブラリ読み込み、データ読み込み、前処理を行う
#  依存: なし
#  出力: data_orig (前処理済みデータフレーム)
# ==============================================================================

# 比較対象のデータ数を揃える（NAの公平性の担保）
use_complete_pairs_for_comparison <- FALSE
# use_complete_pairs_for_comparison <- TRUE

# メモリクリア
gc()

# データディレクトリ設定
dataDir <- "./R/pooledPH/data"

# 出力フォルダ設定
if (!exists("output_dir")) {
    output_dir <- "outputs"
}
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
    data_orig$Duration_sec <- as.numeric(scale(data_orig$Duration_sec))
    
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

# 質問インデックス定義
# 1: Control (no body) | 7: PH (standing behind) 
# 1: no body (322) | 2: self-touch (554) | 5: more than one body (437) 
# 6: PE (580) | 7: PH (580) | 16: agency (189)

# 複数質問インデックスリスト（主分析用：PH & Control）
analysis_questions <- c(1, 7)  # Control(1), PH(7)
question_labels <- c("Control", "PH")

# デフォルトの単一質問インデックス（後方互換性）
iquest <- 7

# ==============================================================================
#  NA処理設定
# ==============================================================================
# 比較分析時のNAの扱い：
#  TRUE  : 比較対象の両質問で非NAのデータのみを使用（厳密な比較）
#  FALSE : 各質問で利用可能なすべてのデータを使用（現在のデフォルト）

sl <- RColorBrewer::brewer.pal(9, 'Set1')

cat("\n====== Setup completed successfully ======\n")
cat("Data loaded:", nrow(data_orig), "rows,", ncol(data_orig), "columns\n")
cat("Analysis questions:", paste(paste0(analysis_questions, " (", question_labels, ")"), collapse=", "), "\n")

# ==============================================================================
#  データ品質チェック（小数データの確認）
# ==============================================================================

cat("\n====== DATA QUALITY CHECK ======\n")

# 質問項目の列インデックス
question_cols <- questionToColumn[analysis_questions]

for (q_idx in seq_along(analysis_questions)) {
    current_q <- analysis_questions[q_idx]
    current_label <- question_labels[q_idx]
    col_idx <- question_cols[q_idx]
    col_name <- colnames(data_orig)[col_idx]
    
    # その列のデータを取得
    question_data <- data_orig[[col_name]]
    
    # NULLまたは空でない場合のみ処理
    if (!is.null(question_data) && length(question_data) > 0) {
        # 数値に変換（警告は抑制）
        numeric_data <- suppressWarnings(as.numeric(question_data))
        
        # NA以外のデータを抽出
        valid_data <- numeric_data[!is.na(numeric_data)]
        
        if (length(valid_data) > 0) {
            # 小数かどうかの判定
            has_decimals <- any(valid_data != round(valid_data), na.rm = TRUE)
            
            # 整数のみの場合と小数混在の場合で表示を分ける
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
            
            # NA情報
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
