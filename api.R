# api.R - 다층모형분석 Plumber API
# 패키지: plumber, lme4, lmerTest

library(plumber)
library(lme4)

# lmerTest: Satterthwaite 자유도로 p값 계산
has_lmerTest <- requireNamespace("lmerTest", quietly = TRUE)
if (has_lmerTest) {
  library(lmerTest)
  message("[OK] lmerTest 로드 - p값 계산 가능")
} else {
  message("[경고] lmerTest 없음 - t값만 제공. install.R을 먼저 실행하세요.")
}

# 정적 파일 제공 (www/ 폴더)
#* @assets ./www /
list()

# ─────────────────────────────────────────
#  유틸리티 함수
# ─────────────────────────────────────────

# ICC 계산 (집단내 상관계수)
calc_icc <- function(model) {
  tryCatch({
    vc <- as.data.frame(VarCorr(model))
    int_var <- vc$vcov[!is.na(vc$var1) & vc$var1 == "(Intercept)" & is.na(vc$var2)]
    res_var <- vc$vcov[vc$grp == "Residual"]
    if (length(int_var) == 0 || length(res_var) == 0) return(NA)
    round(int_var[1] / (int_var[1] + res_var[1]), 4)
  }, error = function(e) NA)
}

# R² 계산 (Nakagawa & Schielzeth, 2013)
calc_r2 <- function(model) {
  tryCatch({
    vc    <- as.data.frame(VarCorr(model))
    sig2  <- sigma(model)^2
    X     <- model.matrix(model)
    beta  <- fixef(model)
    var_f <- var(as.vector(X %*% beta))
    var_r <- sum(vc$vcov[vc$grp != "Residual"])
    tot   <- var_f + var_r + sig2
    list(
      marginal    = round(var_f / tot, 4),
      conditional = round((var_f + var_r) / tot, 4)
    )
  }, error = function(e) list(marginal = NA, conditional = NA))
}

# 모형 적합도 지수
extract_fit <- function(model) {
  list(
    AIC      = round(AIC(model), 2),
    BIC      = round(BIC(model), 2),
    logLik   = round(as.numeric(logLik(model)), 2),
    deviance = round(-2 * as.numeric(logLik(model)), 2),
    npar     = attr(logLik(model), "df")
  )
}

# 고정효과 추출 (추정값, SE, t, p, 95% CI)
extract_fixed <- function(model) {
  # coef(summary()) → S3/S4 모두 안전 (summary()$coefficients 는 S4에서 $ 오류 발생)
  cm  <- as.data.frame(coef(summary(model)))
  cm$term <- rownames(cm)
  rownames(cm) <- NULL

  ci <- tryCatch(
    as.data.frame(confint(model, method = "Wald", level = 0.95)),
    error = function(e) NULL
  )

  lapply(seq_len(nrow(cm)), function(i) {
    row  <- cm[i, ]
    pcol <- grep("Pr\\(>\\|t\\|\\)", names(cm), value = TRUE)
    pval <- if (length(pcol) > 0) round(row[[pcol[1]]], 4) else NA

    ci_l <- NA; ci_u <- NA
    if (!is.null(ci) && row$term %in% rownames(ci)) {
      ci_l <- round(ci[row$term, 1], 4)
      ci_u <- round(ci[row$term, 2], 4)
    }
    list(
      term     = row$term,
      estimate = round(row$Estimate, 4),
      se       = round(row[["Std. Error"]], 4),
      t        = round(row[["t value"]], 4),
      p        = pval,
      ci_lower = ci_l,
      ci_upper = ci_u
    )
  })
}

# 무선효과 추출
extract_random <- function(model) {
  vc <- as.data.frame(VarCorr(model))
  lapply(seq_len(nrow(vc)), function(i) {
    list(
      group = vc$grp[i],
      var1  = ifelse(is.na(vc$var1[i]), "", as.character(vc$var1[i])),
      var2  = ifelse(is.na(vc$var2[i]), "", as.character(vc$var2[i])),
      vcov  = round(vc$vcov[i], 4),
      sdcor = round(vc$sdcor[i], 4)
    )
  })
}

# 모형 진단 데이터 추출
extract_diagnostics <- function(model, df, outcome, group_var, l1_preds, l2_covs, model_name) {
  tryCatch({
    predicted <- as.numeric(predict(model))
    obs       <- as.numeric(df[[outcome]])
    residuals <- obs - predicted
    res_mean  <- mean(residuals, na.rm = TRUE)
    res_sd    <- sd(residuals, na.rm = TRUE)
    zresid    <- if (res_sd > 0) (residuals - res_mean) / res_sd else rep(0, length(residuals))

    # 무선효과 BLUP
    re_list   <- ranef(model)
    re_data   <- list()
    re_names  <- character(0)
    if (group_var %in% names(re_list)) {
      re_raw   <- re_list[[group_var]]
      re_names <- colnames(re_raw)
      re_data  <- lapply(re_names, function(nm) {
        list(name = nm, values = round(as.numeric(re_raw[[nm]]), 6))
      })
    }

    # 독립변수 값 (잔차 vs 독립변수 플롯용)
    all_preds <- c(l1_preds, l2_covs)
    predictor_values <- list()
    if (length(all_preds) > 0) {
      for (v in all_preds) {
        if (v %in% names(df)) {
          predictor_values[[v]] <- as.numeric(df[[v]])
        }
      }
    }

    # re_names를 항상 배열로 보내기 위해 as.list()
    list(
      model_name       = model_name,
      predicted        = round(predicted, 6),
      residuals        = round(residuals, 6),
      zresid           = round(zresid, 6),
      re_data          = re_data,
      re_names         = as.list(re_names),
      predictor_names  = as.list(all_preds),
      predictor_values = predictor_values,
      outcome_values   = round(obs, 6),
      outcome_name     = outcome,
      group_values     = as.character(df[[group_var]]),
      group_name       = group_var
    )
  }, error = function(e) {
    list(model_name = model_name, error = e$message)
  })
}

# ─────────────────────────────────────────
#  Plumber 엔드포인트
# ─────────────────────────────────────────

#* CORS 필터
#* @filter cors
function(req, res) {
  res$setHeader("Access-Control-Allow-Origin",  "*")
  res$setHeader("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Accept")
  if (req$REQUEST_METHOD == "OPTIONS") { res$status <- 200; return(list()) }
  plumber::forward()
}

#* API 상태 확인
#* @get /api/health
#* @serializer unboxedJSON
function() {
  list(
    status   = "ok",
    R        = as.character(getRversion()),
    lme4     = as.character(packageVersion("lme4")),
    lmerTest = if (has_lmerTest) as.character(packageVersion("lmerTest")) else "미설치"
  )
}

#* 다층모형 분석 실행
#* @post /api/analyze
#* @parser json
#* @serializer unboxedJSON
function(req) {
  tryCatch({
    body <- req$body

    # ── 1. 데이터 변환 (컬럼 형식 JSON → data.frame) ──────────────
    df <- as.data.frame(body$data, stringsAsFactors = FALSE)

    # ── 2. 파라미터 ────────────────────────────────────────────────
    outcome     <- as.character(body$outcome)
    group_var   <- as.character(body$group_var)
    l1_preds    <- if (!is.null(body$l1_preds))    as.character(unlist(body$l1_preds))    else character(0)
    l2_covs     <- if (!is.null(body$l2_covs))     as.character(unlist(body$l2_covs))     else character(0)
    rand_slopes <- if (!is.null(body$rand_slopes)) as.character(unlist(body$rand_slopes)) else character(0)
    l1_centering <- if (!is.null(body$l1_centering)) as.character(body$l1_centering) else "none"
    l2_centering <- if (!is.null(body$l2_centering)) as.character(body$l2_centering) else "none"
    cross_interactions <- if (!is.null(body$cross_interactions)) body$cross_interactions else list()
    # jsonlite가 [{l1:"a",l2:"b"},...] 를 data.frame으로 변환하므로 list-of-list로 정규화
    if (is.data.frame(cross_interactions)) {
      cross_interactions <- lapply(seq_len(nrow(cross_interactions)), function(i) {
        list(l1 = as.character(cross_interactions$l1[i]),
             l2 = as.character(cross_interactions$l2[i]))
      })
    } else if (length(cross_interactions) > 0 && !is.null(names(cross_interactions))) {
      # 단일 상호작용이 flat named list로 온 경우
      cross_interactions <- list(cross_interactions)
    }

    # ── 3. 변수 존재 확인 ─────────────────────────────────────────
    needed  <- unique(c(outcome, group_var, l1_preds, l2_covs))
    missing <- setdiff(needed, names(df))
    if (length(missing) > 0) stop(paste0("데이터에 없는 변수: ", paste(missing, collapse = ", ")))

    # ── 4. 수치형 변환 ────────────────────────────────────────────
    df[[outcome]] <- suppressWarnings(as.numeric(df[[outcome]]))
    for (v in c(l1_preds, l2_covs)) {
      df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
    }

    # ── 5. 결측치 제거 ────────────────────────────────────────────
    df       <- df[complete.cases(df[needed]), ]
    n_obs    <- nrow(df)
    n_groups <- length(unique(df[[group_var]]))

    if (n_obs    <  10) stop("관측치가 너무 적습니다 (최소 10개 이상 필요).")
    if (n_groups <   3) stop("집단 수가 너무 적습니다 (최소 3개 이상 필요).")

    # ── 6. 센터링 (Centering) ──────────────────────────────────────
    cwc_gm_vars <- character(0)   # CWC 시 자동 추가되는 집단평균 변수명

    if (l1_centering == "gmc" && length(l1_preds) > 0) {
      for (v in l1_preds) {
        df[[v]] <- df[[v]] - mean(df[[v]], na.rm = TRUE)
      }
    } else if (l1_centering == "cwc" && length(l1_preds) > 0) {
      for (v in l1_preds) {
        gm <- ave(df[[v]], df[[group_var]], FUN = function(x) mean(x, na.rm = TRUE))
        df[[v]] <- df[[v]] - gm                          # 집단평균 중심화
        gm_name <- paste0(v, "_gm")
        df[[gm_name]] <- gm - mean(df[[v]] + gm, na.rm = TRUE)  # 집단평균의 전체평균 중심화
        cwc_gm_vars <- c(cwc_gm_vars, gm_name)
      }
      # CWC 집단평균 변수를 L2 공변량에 자동 추가
      l2_covs <- unique(c(l2_covs, cwc_gm_vars))
    }

    if (l2_centering == "gmc" && length(l2_covs) > 0) {
      for (v in l2_covs) {
        if (v %in% names(df)) {
          df[[v]] <- df[[v]] - mean(df[[v]], na.rm = TRUE)
        }
      }
    }

    result <- list(
      success   = TRUE,
      data_info = list(
        n_obs    = n_obs,
        n_groups = n_groups,
        outcome  = outcome,
        group_var = group_var,
        l1_centering = l1_centering,
        l2_centering = l2_centering,
        cwc_gm_vars  = as.list(cwc_gm_vars)
      )
    )

    # ── R 코드 로그 구축 ────────────────────────────────────────
    rcode <- character(0)
    rcode <- c(rcode,
      "# ═══════════════════════════════════════════════════",
      "# 다층모형(MLM/HLM) 분석 R 코드",
      "# 패키지: lme4, lmerTest",
      "# ═══════════════════════════════════════════════════",
      "",
      "# ── 1. 패키지 로드 ──",
      "library(lme4)",
      "library(lmerTest)  # Satterthwaite 자유도 기반 p값 계산",
      "",
      "# ── 2. 데이터 불러오기 및 전처리 ──",
      paste0('df <- read.csv("데이터파일.csv")'),
      paste0('# 종속변수: ', outcome),
      paste0('# 집단변수(Level-2 ID): ', group_var),
      if (length(l1_preds) > 0) paste0('# Level-1 독립변수: ', paste(l1_preds, collapse = ', ')) else '# Level-1 독립변수: (없음)',
      if (length(l2_covs) > 0) paste0('# Level-2 공변량: ', paste(setdiff(l2_covs, cwc_gm_vars), collapse = ', ')) else '# Level-2 공변량: (없음)',
      paste0('# 관측치: ', n_obs, '개, 집단 수: ', n_groups, '개'),
      ""
    )

    # 센터링 코드 로그
    if (l1_centering == "gmc" && length(l1_preds) > 0) {
      rcode <- c(rcode,
        "# ── 3. 센터링: Grand Mean Centering (L1) ──",
        "# 각 L1 독립변수에서 전체 평균을 빼서 중심화",
        paste0("# X_gmc = X - mean(X)")
      )
      for (v in l1_preds) {
        rcode <- c(rcode, paste0('df$', v, ' <- df$', v, ' - mean(df$', v, ', na.rm = TRUE)'))
      }
      rcode <- c(rcode, "")
    } else if (l1_centering == "cwc" && length(l1_preds) > 0) {
      rcode <- c(rcode,
        "# ── 3. 센터링: Group Mean Centering (CWC, L1) ──",
        "# 각 L1 독립변수에서 해당 집단의 평균을 빼서 중심화",
        "# 집단 평균은 Grand Mean Centering하여 L2 공변량으로 추가",
        paste0("# X_cwc = X - mean_j(X),  X_gm = mean_j(X) - mean(X)")
      )
      for (v in l1_preds) {
        rcode <- c(rcode,
          paste0('gm_', v, ' <- ave(df$', v, ', df$', group_var, ', FUN = function(x) mean(x, na.rm = TRUE))'),
          paste0('df$', v, ' <- df$', v, ' - gm_', v, '  # 집단평균 중심화 (CWC)'),
          paste0('df$', v, '_gm <- gm_', v, ' - mean(df$', v, ' + gm_', v, ', na.rm = TRUE)  # 집단평균의 GMC')
        )
      }
      rcode <- c(rcode, "")
    } else {
      rcode <- c(rcode, "# ── 3. 센터링: 없음 (원점수 사용) ──", "")
    }

    if (l2_centering == "gmc" && length(l2_covs) > 0) {
      rcode <- c(rcode, "# L2 공변량 Grand Mean Centering")
      for (v in l2_covs) {
        rcode <- c(rcode, paste0('df$', v, ' <- df$', v, ' - mean(df$', v, ', na.rm = TRUE)'))
      }
      rcode <- c(rcode, "")
    }

    # ── Model 0: 기저모형 (Null) ──────────────────────────────────
    f0 <- as.formula(paste0(outcome, " ~ 1 + (1|", group_var, ")"))
    m0 <- suppressWarnings(lmer(f0, data = df, REML = FALSE))

    rcode <- c(rcode,
      "# ── 4. Model 0: 기저모형 (Null Model) ──",
      "# 고정효과 없이 무선절편만 포함하여 ICC 계산",
      paste0('m0 <- lmer(', deparse(f0), ', data = df, REML = FALSE)'),
      "summary(m0)",
      paste0("# ICC = ", round(calc_icc(m0), 4), " → 종속변수 분산의 ", round(calc_icc(m0) * 100, 1), "%가 집단 간 차이"),
      ""
    )

    result$null_model <- list(
      formula        = deparse(f0),
      icc            = calc_icc(m0),
      random_effects = extract_random(m0),
      fit            = extract_fit(m0)
    )

    # ── Model 1: 무선절편 모형 (Random Intercept) ─────────────────
    all_fixed <- c(l1_preds, l2_covs)
    if (length(all_fixed) > 0) {
      f1_str <- paste0(outcome, " ~ ",
                       paste(all_fixed, collapse = " + "),
                       " + (1|", group_var, ")")
      m1 <- suppressWarnings(lmer(as.formula(f1_str), data = df, REML = FALSE))
      lrt01 <- suppressWarnings(anova(m0, m1))

      result$ri_model <- list(
        formula        = f1_str,
        fixed_effects  = extract_fixed(m1),
        random_effects = extract_random(m1),
        icc            = calc_icc(m1),
        r2             = calc_r2(m1),
        fit            = extract_fit(m1),
        lrt_vs_null    = list(
          chi2 = round(lrt01[["Chisq"]][2], 4),
          df   = lrt01[["Df"]][2],
          p    = round(lrt01[["Pr(>Chisq)"]][2], 4)
        )
      )

      rcode <- c(rcode,
        "# ── 5. Model 1: 무선절편 모형 (Random Intercept) ──",
        "# Level-1, Level-2 독립변수를 고정효과로 투입하고 무선절편만 포함",
        "# 집단 간 절편(평균) 차이는 허용하되, 기울기는 모든 집단에서 동일하다고 가정",
        paste0('m1 <- lmer(', f1_str, ', data = df, REML = FALSE)'),
        "summary(m1)  # 고정효과 계수, 무선효과 분산 확인",
        "",
        "# Model 0 vs Model 1: 우도비 검정 (Likelihood Ratio Test)",
        "anova(m0, m1)  # 독립변수 투입 효과가 유의한지 검정",
        ""
      )

      # ── Model 2: 무선기울기 모형 (Random Slope) ─────────────────
      valid_rs <- intersect(rand_slopes, l1_preds)
      if (length(valid_rs) > 0) {
        slope_part <- paste(c("1", valid_rs), collapse = " + ")
        f2_str <- paste0(outcome, " ~ ",
                         paste(all_fixed, collapse = " + "),
                         " + (", slope_part, "|", group_var, ")")

        # 에러 시 문자열 반환 → is.character()로 판별 (S4 모형에 $ 사용 금지)
        m2 <- tryCatch(
          suppressWarnings(
            lmer(as.formula(f2_str), data = df, REML = FALSE,
                 control = lmerControl(optimizer = "bobyqa",
                                       optCtrl   = list(maxfun = 2e5)))
          ),
          error = function(e) e$message
        )

        if (is.character(m2)) {
          result$rs_model_error <- paste0("수렴 실패: ", m2)
        } else {
          lrt12 <- tryCatch(suppressWarnings(anova(m1, m2)), error = function(e) NULL)

          result$rs_model <- list(
            formula        = f2_str,
            fixed_effects  = extract_fixed(m2),
            random_effects = extract_random(m2),
            icc            = calc_icc(m2),
            r2             = calc_r2(m2),
            fit            = extract_fit(m2),
            lrt_vs_ri      = if (!is.null(lrt12)) list(
              chi2 = round(lrt12[["Chisq"]][2], 4),
              df   = lrt12[["Df"]][2],
              p    = round(lrt12[["Pr(>Chisq)"]][2], 4)
            ) else NULL
          )

          rcode <- c(rcode,
            "# ── 6. Model 2: 무선기울기 모형 (Random Slope) ──",
            "# 무선절편에 무선기울기를 추가하여 집단별 기울기 차이를 허용",
            "# 즉, 독립변수의 효과가 집단마다 다를 수 있는지 검정",
            paste0('m2 <- lmer(', f2_str, ','),
            "             data = df, REML = FALSE,",
            "             control = lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))",
            "summary(m2)  # 고정효과 + 무선기울기 분산 확인",
            "",
            "# Model 1 vs Model 2: 우도비 검정",
            "anova(m1, m2)  # 무선기울기 추가가 유의한지 검정",
            ""
          )
        }
      }

      # ── Model 3: 교차수준 상호작용 모형 (Cross-Level Interaction) ──
      if (length(cross_interactions) > 0) {
        int_terms   <- character(0)
        int_l1_vars <- character(0)
        for (ci in cross_interactions) {
          l1v <- as.character(ci$l1)
          l2v <- as.character(ci$l2)
          if (l1v %in% l1_preds && l2v %in% l2_covs) {
            int_terms   <- c(int_terms, paste0(l1v, ":", l2v))
            int_l1_vars <- c(int_l1_vars, l1v)
          }
        }

        if (length(int_terms) > 0) {
          int_l1_vars <- unique(int_l1_vars)
          all_rs_m3   <- unique(c(valid_rs, int_l1_vars))
          fixed_part  <- paste(c(all_fixed, int_terms), collapse = " + ")
          slope_part3 <- paste(c("1", all_rs_m3), collapse = " + ")
          f3_str <- paste0(outcome, " ~ ", fixed_part,
                           " + (", slope_part3, "|", group_var, ")")

          m3 <- tryCatch(
            suppressWarnings(
              lmer(as.formula(f3_str), data = df, REML = FALSE,
                   control = lmerControl(optimizer = "bobyqa",
                                         optCtrl   = list(maxfun = 2e5)))
            ),
            error = function(e) e$message
          )

          if (is.character(m3)) {
            result$cl_model_error <- paste0("수렴 실패: ", m3)
          } else {
            prev_m <- m1
            if (length(valid_rs) > 0 && exists("m2") && !is.character(m2)) prev_m <- m2
            lrt_m3 <- tryCatch(suppressWarnings(anova(prev_m, m3)), error = function(e) NULL)

            result$cl_model <- list(
              formula           = f3_str,
              fixed_effects     = extract_fixed(m3),
              random_effects    = extract_random(m3),
              icc               = calc_icc(m3),
              r2                = calc_r2(m3),
              fit               = extract_fit(m3),
              interaction_terms = as.list(int_terms),
              lrt_vs_prev       = if (!is.null(lrt_m3)) list(
                chi2 = round(lrt_m3[["Chisq"]][2], 4),
                df   = lrt_m3[["Df"]][2],
                p    = round(lrt_m3[["Pr(>Chisq)"]][2], 4)
              ) else NULL
            )

            rcode <- c(rcode,
              "# ── 7. Model 3: 교차수준 상호작용 모형 (Cross-Level Interaction) ──",
              "# Level-1 변수와 Level-2 변수의 교차수준 상호작용 효과 검정",
              "# 집단 수준 변수가 개인 수준 변수의 기울기에 미치는 영향 분석",
              paste0('m3 <- lmer(', f3_str, ','),
              "             data = df, REML = FALSE,",
              "             control = lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 2e5)))",
              "summary(m3)  # 교차수준 상호작용 계수 확인",
              "",
              "# 이전 모형 vs Model 3: 우도비 검정",
              paste0("anova(", if (length(valid_rs) > 0 && !is.null(result$rs_model)) "m2" else "m1", ", m3)  # 교차수준 상호작용 효과 검정"),
              ""
            )
          }
        }
      }
    }

    # ── 진단 데이터 추출 (최종 모형) ─────────────────────────────
    # result 에 이미 저장된 모형 키를 기준으로 최종 모형 결정 (exists 스코프 문제 회피)
    final_model <- m0
    final_name  <- "Model 0: 기저모형"
    if (!is.null(result$ri_model)) {
      final_model <- m1; final_name <- "Model 1: 무선절편"
    }
    if (!is.null(result$rs_model)) {
      final_model <- m2; final_name <- "Model 2: 무선기울기"
    }
    if (!is.null(result$cl_model)) {
      final_model <- m3; final_name <- "Model 3: 교차수준 상호작용"
    }
    result$diagnostics <- tryCatch(
      extract_diagnostics(final_model, df, outcome, group_var, l1_preds, l2_covs, final_name),
      error = function(e) list(model_name = final_name, error = e$message)
    )

    # ── 모형 비교표 ───────────────────────────────────────────────
    comp <- list(list(
      name     = "Model 0: 기저모형",
      npar     = result$null_model$fit$npar,
      AIC      = result$null_model$fit$AIC,
      BIC      = result$null_model$fit$BIC,
      logLik   = result$null_model$fit$logLik,
      deviance = result$null_model$fit$deviance,
      delta_deviance = NA, chi2 = NA, df_chi = NA, p_chi = NA
    ))

    if (!is.null(result$ri_model)) {
      lrt <- result$ri_model$lrt_vs_null
      comp[[2]] <- list(
        name     = "Model 1: 무선절편",
        npar     = result$ri_model$fit$npar,
        AIC      = result$ri_model$fit$AIC,
        BIC      = result$ri_model$fit$BIC,
        logLik   = result$ri_model$fit$logLik,
        deviance = result$ri_model$fit$deviance,
        delta_deviance = round(result$null_model$fit$deviance - result$ri_model$fit$deviance, 2),
        chi2 = lrt$chi2, df_chi = lrt$df, p_chi = lrt$p
      )
    }

    if (!is.null(result$rs_model)) {
      lrt <- result$rs_model$lrt_vs_ri
      comp[[3]] <- list(
        name     = "Model 2: 무선기울기",
        npar     = result$rs_model$fit$npar,
        AIC      = result$rs_model$fit$AIC,
        BIC      = result$rs_model$fit$BIC,
        logLik   = result$rs_model$fit$logLik,
        deviance = result$rs_model$fit$deviance,
        delta_deviance = if (!is.null(lrt)) round(result$ri_model$fit$deviance - result$rs_model$fit$deviance, 2) else NA,
        chi2     = if (!is.null(lrt)) lrt$chi2 else NA,
        df_chi   = if (!is.null(lrt)) lrt$df   else NA,
        p_chi    = if (!is.null(lrt)) lrt$p    else NA
      )
    }

    if (!is.null(result$cl_model)) {
      lrt <- result$cl_model$lrt_vs_prev
      prev_dev <- if (!is.null(result$rs_model)) result$rs_model$fit$deviance
                  else result$ri_model$fit$deviance
      comp[[length(comp) + 1]] <- list(
        name     = "Model 3: 교차수준 상호작용",
        npar     = result$cl_model$fit$npar,
        AIC      = result$cl_model$fit$AIC,
        BIC      = result$cl_model$fit$BIC,
        logLik   = result$cl_model$fit$logLik,
        deviance = result$cl_model$fit$deviance,
        delta_deviance = if (!is.null(lrt)) round(prev_dev - result$cl_model$fit$deviance, 2) else NA,
        chi2     = if (!is.null(lrt)) lrt$chi2 else NA,
        df_chi   = if (!is.null(lrt)) lrt$df   else NA,
        p_chi    = if (!is.null(lrt)) lrt$p    else NA
      )
    }

    result$model_comparison <- comp

    # ── R 코드 로그 마무리 ─────────────────────────────────────────
    rcode <- c(rcode,
      "# ── 모형 비교 (Model Comparison) ──",
      "# 중첩 모형 간 우도비 검정(LRT)으로 최적 모형 선택",
      "# AIC/BIC가 작을수록, LRT p값이 유의할수록 해당 모형이 우수"
    )
    if (!is.null(result$ri_model)) {
      rcode <- c(rcode, "anova(m0, m1)  # 기저모형 vs 무선절편")
    }
    if (!is.null(result$rs_model)) {
      rcode <- c(rcode, "anova(m1, m2)  # 무선절편 vs 무선기울기")
    }
    if (!is.null(result$cl_model)) {
      if (!is.null(result$rs_model)) {
        rcode <- c(rcode, "anova(m2, m3)  # 무선기울기 vs 교차수준")
      } else {
        rcode <- c(rcode, "anova(m1, m3)  # 무선절편 vs 교차수준")
      }
    }
    rcode <- c(rcode, "")
    result$r_code <- paste(rcode, collapse = "\n")

    result

  }, error = function(e) {
    list(success = FALSE, error = e$message)
  })
}
