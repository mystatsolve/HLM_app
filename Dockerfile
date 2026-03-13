# ═══════════════════════════════════════════════════════════════
# Dockerfile - HLM_app 다층모형분석 웹앱 배포용
# ═══════════════════════════════════════════════════════════════
# 빌드: docker build -t hlm-app .
# 실행: docker run -p 8001:8001 hlm-app
# ═══════════════════════════════════════════════════════════════

FROM r-base:4.4.0

# 시스템 라이브러리 설치 (lme4 컴파일에 필요)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# R 패키지 설치 (캐시 레이어 활용을 위해 먼저 실행)
RUN R -e "install.packages(c('plumber', 'lme4', 'lmerTest'), repos='https://cran.rstudio.com/', Ncpus=2)"

# 앱 복사
WORKDIR /app
COPY . /app

# Render.com은 PORT 환경변수를 주입함 (기본값 8001)
ENV PORT=8001

# 서버 시작 (PORT 환경변수 사용)
CMD ["sh", "-c", "Rscript -e \"library(plumber); pr <- plumb('api.R'); pr_run(pr, host='0.0.0.0', port=as.integer(Sys.getenv('PORT', '8001')), docs=FALSE)\""]
