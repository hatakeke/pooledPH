# ==============================================================================
#  test_modules.R - モジュールテストスクリプト
# ==============================================================================
#  概要: 各モジュールの構文チェックとパッケージ依存関係の確認
# ==============================================================================

cat("
================================================================================
モジュールテスト開始
================================================================================
")

# ==============================================================================
# 1. Rバージョン確認
# ==============================================================================
cat("\n[1] Rバージョン確認\n")
cat("R version:", R.version.string, "\n")

# ==============================================================================
# 2. 必要パッケージのインストール状況確認
# ==============================================================================
cat("\n[2] パッケージインストール状況確認\n")

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
    results <- data.frame(
        Package = packages,
        Installed = sapply(packages, function(p) p %in% rownames(installed.packages())),
        stringsAsFactors = FALSE
    )
    return(results)
}

pkg_status <- check_packages(required_packages)
print(pkg_status)

missing_packages <- pkg_status$Package[!pkg_status$Installed]
if (length(missing_packages) > 0) {
    cat("\n⚠ 未インストールパッケージ:\n")
    cat(paste(" -", missing_packages, collapse = "\n"), "\n")
    cat("\n以下のコマンドでインストールできます:\n")
    cat(paste0('install.packages(c("', paste(missing_packages, collapse = '", "'), '"))\n'))
} else {
    cat("\n✓ 全てのパッケージがインストールされています\n")
}

# ==============================================================================
# 3. 各モジュールの構文チェック
# ==============================================================================
cat("\n[3] モジュール構文チェック\n")

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

# 現在の作業ディレクトリを使用
script_dir <- getwd()

check_syntax <- function(file_path) {
    tryCatch({
        parse(file = file_path)
        return(list(success = TRUE, error = NULL))
    }, error = function(e) {
        return(list(success = FALSE, error = e$message))
    })
}

cat("\nモジュールディレクトリ:", script_dir, "\n\n")

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

# エラーがあれば詳細表示
errors <- syntax_results[syntax_results$Status == "✗ ERROR", ]
if (nrow(errors) > 0) {
    cat("\n⚠ 構文エラー詳細:\n")
    for (i in 1:nrow(errors)) {
        cat(paste0("\n", errors$Module[i], ":\n"))
        cat(paste0("  ", errors$Error[i], "\n"))
    }
}

# ==============================================================================
# 4. サマリー
# ==============================================================================
cat("\n")
cat("================================================================================\n")
cat("テスト結果サマリー\n")
cat("================================================================================\n")

ok_count <- sum(syntax_results$Status == "✓ OK")
error_count <- sum(syntax_results$Status == "✗ ERROR")
notfound_count <- sum(syntax_results$Status == "? NOT FOUND")

cat(paste0("構文チェック OK: ", ok_count, "/", length(module_files), "\n"))
cat(paste0("構文エラー: ", error_count, "\n"))
cat(paste0("ファイル未検出: ", notfound_count, "\n"))
cat(paste0("パッケージ不足: ", length(missing_packages), "\n"))

if (error_count == 0 && notfound_count == 0 && length(missing_packages) == 0) {
    cat("\n✓ 全てのテストに合格しました！\n")
} else {
    cat("\n⚠ いくつかの問題があります。上記を確認してください。\n")
}

cat("\n================================================================================\n")
