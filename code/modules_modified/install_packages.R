# ==============================================================================
#  install_packages.R - パッケージインストールスクリプト
# ==============================================================================

cat("================================================================================\n")
cat("必要パッケージのインストール開始\n")
cat("================================================================================\n\n")

# ライブラリパスを設定（E:\R\library を使用）
lib_path <- "E:/R/library"
if (!dir.exists(lib_path)) {
    dir.create(lib_path, recursive = TRUE)
}

# .libPaths にライブラリパスを追加
.libPaths(c(lib_path, .libPaths()))

cat("ライブラリパス:", lib_path, "\n\n")

# CRANミラーを設定
options(repos = c(CRAN = "https://cloud.r-project.org/"))

# インストールするパッケージ一覧
packages_to_install <- c(
    "tidyverse",     # データ操作（dplyr, ggplot2, tidyr等を含む）
    "readxl",        # Excel読み込み
    "brms",          # ベイズモデリング（※これが一番時間がかかります）
    "tidybayes",     # ベイズデータ可視化
    "RColorBrewer",  # カラーパレット
    "ggmcmc",        # MCMCプロット
    "svglite",       # SVG出力
    "bayestestR",    # ROPE分析等
    "extrafont",     # フォント管理
    "crayon",        # コンソール色付け
    "ordinal",       # 順序回帰
    "RVAideMemoire"  # 統計ヘルパー
)

# インストール済みパッケージを確認
installed_pkgs <- rownames(installed.packages(lib.loc = lib_path))

# インストールが必要なパッケージをフィルタ
to_install <- packages_to_install[!packages_to_install %in% installed_pkgs]

if (length(to_install) > 0) {
    cat("インストールするパッケージ:\n")
    cat(paste(" -", to_install, collapse = "\n"), "\n\n")
    
    cat("インストール中... (brmsは特に時間がかかります)\n\n")
    
    for (pkg in to_install) {
        cat(paste0("Installing: ", pkg, " ... "))
        tryCatch({
            install.packages(pkg, lib = lib_path, dependencies = TRUE, quiet = TRUE)
            cat("✓ 完了\n")
        }, error = function(e) {
            cat(paste0("✗ エラー: ", e$message, "\n"))
        })
    }
} else {
    cat("全てのパッケージは既にインストールされています。\n")
}

cat("\n================================================================================\n")
cat("インストール完了\n")
cat("================================================================================\n")

# 最終確認
cat("\nインストール状況確認:\n")
for (pkg in packages_to_install) {
    if (pkg %in% rownames(installed.packages())) {
        cat(paste0("  ✓ ", pkg, "\n"))
    } else {
        cat(paste0("  ✗ ", pkg, " (未インストール)\n"))
    }
}
