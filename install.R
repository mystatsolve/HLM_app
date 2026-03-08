# install.R - 다층모형분석 웹앱 패키지 설치

cat("필요 패키지 설치 중...\n")

pkgs <- c("plumber", "lme4", "lmerTest")

for (pkg in pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  설치: %s\n", pkg))
    install.packages(pkg, repos = "https://cran.rstudio.com/")
  } else {
    cat(sprintf("  이미 설치됨: %s (%s)\n", pkg, packageVersion(pkg)))
  }
}

cat("\n설치 완료! run.bat을 실행하세요.\n")
