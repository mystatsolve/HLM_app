@echo off
chcp 65001 > nul
title 다층모형분석 웹앱 (R Plumber)

cd /d "%~dp0"

echo ========================================
echo   다층모형분석 웹앱 (MLM Web App)
echo ========================================
echo.
echo [1단계] 패키지 확인 중...
Rscript -e "pkgs <- c('plumber','lme4','lmerTest'); missing <- pkgs[!sapply(pkgs, requireNamespace, quietly=TRUE)]; if(length(missing)>0){cat('누락 패키지:', paste(missing,collapse=','),'\n설치하려면 install.R을 실행하세요.\n'); quit(status=1)}"
if %ERRORLEVEL% NEQ 0 (
  echo.
  echo 패키지 설치가 필요합니다. install.R을 실행하세요:
  echo   Rscript install.R
  pause
  exit /b 1
)

echo [2단계] R API 서버 시작 (포트: 8000)
echo.
echo  브라우저에서 http://localhost:8000 을 열어주세요.
echo  종료: Ctrl+C
echo.

Rscript -e "library(plumber); pr <- plumb('api.R'); pr_run(pr, host='127.0.0.1', port=8000, docs=FALSE)"

pause
