# ═══════════════════════════════════════════════════════════════
# Dockerfile - HLM_app 다층모형분석 웹앱 배포용
# ═══════════════════════════════════════════════════════════════

FROM r-base:4.4.0

# 시스템 라이브러리 설치
# cmake: nloptr 패키지 컴파일에 필요
# libsodium-dev: plumber의 sodium 패키지에 필요
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    cmake \
    && rm -rf /var/lib/apt/lists/*

# R 패키지 설치
RUN R -e "install.packages(c('plumber', 'lme4', 'lmerTest'), repos='https://cran.rstudio.com/', lib='/usr/local/lib/R/site-library', Ncpus=2)"

# 설치 확인
RUN R -e "library(plumber); library(lme4); library(lmerTest); cat('All packages OK\n')"

# 앱 복사
WORKDIR /app
COPY . /app

ENV PORT=8001

CMD ["sh", "-c", "Rscript -e \"library(plumber); pr <- plumb('api.R'); pr_run(pr, host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '8001')), docs=FALSE)\""]
