# start_server.R - R Plumber API 서버 시작
# 사용법: Rscript start_server.R

# 스크립트 위치로 작업 디렉토리 설정
setwd(dirname(sys.frame(1)$ofile))

library(plumber)
pr <- plumb("api.R")
pr_run(pr, host = "127.0.0.1", port = 8001, docs = FALSE)
