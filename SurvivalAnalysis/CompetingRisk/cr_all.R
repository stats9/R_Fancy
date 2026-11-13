#!/usr/bin/env Rscript
## =============================================================================
##  Reproduction code for
##    "Doubly robust calibration of lower predictive bounds for a cause-specific
##     event time under competing risks"
##    H. Ezzatabadipour and R. Delino
##  Repository: https://github.com/stats9/R_Fancy
##
##  Target: a bound L(X), capped at a horizon tau, such that
##          P{T1* >= L(X)} >= 1 - alpha, where T1* = T if J = 1 and +Inf otherwise.
##
##  ONE COMMAND reproduces every number, table and figure in the paper:
##      source("cr_all.R")                 # R console / RStudio / VS Code
##      Rscript cr_all.R 4                 # terminal; 4 = number of cores
##
##  Requirements: R >= 4.2; packages survival, parallel (base R) and
##  randomForestSRC (stage "rf" only). Run time with 4 cores: about 50 minutes.
##
##  ---------------------------------------------------------------------------
##  OUTPUT (folder OUT_DIR, default "out_cr")           paper
##  ---------------------------------------------------------------------------
##  stage   files                                        result
##  main    results_main.rds, summary_main.csv           Table 1, Figure 1
##  nsweep  results_nsweep.rds, summary_nsweep.csv       Figure 2
##  rf      results_rf.rds, summary_rf.csv               Scenario S6, Figure 4 / Web Fig. 1
##  drcheck (printed in the log)                         Table A1 / Web Appendix C
##  app     app_mgus2_alpha05.rds, app_mgus2_alpha10.rds Section 6, Figure 3
##  figs    figs/Fig1-Fig4 (.pdf and .eps)               Figures 1-4
##  all     ALL_RESULTS.txt: every summary table + sessionInfo()
##  smoke   (optional, not run by default) 3 replications, a 10-second check
## =============================================================================

suppressPackageStartupMessages({
  library(survival)
  library(parallel)
})

## ============================================================================
## SETTINGS
## Each setting can be overridden by defining a variable of the same name
## before calling source(), e.g.
##     SIM_CORES <- 4; RUN_STAGES <- c("app", "figs"); source("cr_all.R")
## ----------------------------------------------------------------------------
##  SIM_CORES     number of cores (default 1; Rscript: first argument)
##  RUN_STAGES    stages to run, any of "smoke", "main", "nsweep", "rf",
##                "drcheck", "app", "figs" (default: all except "smoke")
##  OUT_DIR       output folder (default "out_cr")
##  FIG_DEVICE    "cairo" (default; fonts embedded; Helvetica drawn as Arial)
##                or "base" (pdf()/postscript(); standard fonts not embedded)
##  FIG_FAMILY    "Helvetica" (default), "Times", "Palatino", ...
##  FIG_POINTSIZE base font size (default 9)
##  FIG_FORMAT    "both" (default), "pdf" or "eps"
##  FIG_NAMES     "springer" (default; Fig1-Fig4) or "descriptive"
##                (fig_coverage, fig_nsweep, fig_app, fig_rf)
##  FIG_SCALE     size multiplier (default 1)
##  FIG_COLOR     TRUE (default) or FALSE for greyscale
##  FIG_EMBED     TRUE to embed fonts with Ghostscript (only for FIG_DEVICE = "base")
## ============================================================================
setting <- function(name, default) {
  if (exists(name, envir = globalenv(), inherits = FALSE)) get(name, envir = globalenv()) else default
}
ALL_STAGES <- c("smoke", "main", "nsweep", "rf", "drcheck", "app", "figs")
args   <- commandArgs(trailingOnly = TRUE)
cores  <- as.integer(setting("SIM_CORES", if (length(args) >= 1L) args[1] else 1L))
stages <- setting("RUN_STAGES", setdiff(ALL_STAGES, "smoke"))
OUT    <- setting("OUT_DIR", "out_cr")
if (length(bad <- setdiff(stages, ALL_STAGES)))
  stop("unknown stage(s): ", paste(bad, collapse = ", "),
       ". Valid stages: ", paste(ALL_STAGES, collapse = ", "))

## Simulation design (Section 5 of the paper). Changing these changes the results.
CFG <- list(
  tau = 3,            # horizon: bounds are capped at tau
  G = 301,            # time-grid points on [0, tau]
  beta_step = 0.002,  # grid for the pseudo-quantile level beta
  alpha = 0.1,        # target miscoverage
  g_min = 0.05,       # truncation of estimated censoring survival (positivity)
  s_min = 0.02,       # truncation of estimated all-cause survival in eta
  n_test = 5000,      # test covariates; coverage is computed exactly from the true CIF
  R = 500,            # replications per scenario (stage "main")
  k1 = 1.5, b1 = 3.0, # cause 1: Weibull cause-specific hazard
  l2 = 0.15,          # cause 2: exponential cause-specific hazard
  lC = 0.80,          # censoring: exponential hazard (about 58% censored)
  seed0 = 20261108L   # base seed; every replication has its own fixed seed
)

## ----------------------------------------------------------------------------
## Data-generating process
## ----------------------------------------------------------------------------
f1_fun <- function(X)     -0.2 + 0.8 * X[, 1] + 1.2 * X[, 1]^2 - 0.6 * X[, 2]
f2_fun <- function(X, a2)   a2 + 0.7 * X[, 2] + 1.0 * X[, 3]^2
g_fun  <- function(X)     -1.0 + 1.2 * X[, 1] + 2.5 * X[, 1]^2 + 0.4 * X[, 2]

gen_X <- function(n) {
  matrix(runif(4 * n, -1, 1), n, 4, dimnames = list(NULL, paste0("x", 1:4)))
}

gen_data <- function(n, a2, cfg) {
  X  <- gen_X(n)
  T1 <- cfg$b1 * (rexp(n) * exp(-f1_fun(X)))^(1 / cfg$k1)   # latent times give the
  T2 <- rexp(n) / (cfg$l2 * exp(f2_fun(X, a2)))             # stated cause-specific hazards
  C  <- rexp(n) / (cfg$lC * exp(g_fun(X)))
  T  <- pmin(T1, T2)
  J  <- ifelse(T1 <= T2, 1L, 2L)
  Y  <- pmin(T, C)
  D  <- as.integer(T <= C)
  list(X = X, Y = Y, D = D, J = J * D)          # J = 0 when censored
}

## True CIF of cause 1 on the grid (midpoint rule on the all-cause survival)
true_F1 <- function(X, a2, cfg, tg) {
  e1  <- exp(f1_fun(X)); e2 <- exp(f2_fun(X, a2))
  dL1 <- diff((tg / cfg$b1)^cfg$k1)
  mid <- (tg[-1] + tg[-length(tg)]) / 2
  Smid <- exp(-outer(e1, (mid / cfg$b1)^cfg$k1) - outer(e2, cfg$l2 * mid))
  cbind(0, rowCumsum(Smid * outer(e1, dL1)))
}

## ----------------------------------------------------------------------------
## Matrix helpers
## ----------------------------------------------------------------------------
rowCumsum <- function(M) {
  if (ncol(M) == 1L) return(M)
  out <- t(apply(M, 1, cumsum))
  if (nrow(M) == 1L) out <- matrix(out, nrow = 1)
  out
}
rowDiff <- function(M) cbind(M[, 1], M[, -1, drop = FALSE] - M[, -ncol(M), drop = FALSE])

## ----------------------------------------------------------------------------
## Nuisance estimation: Cox models + Breslow, evaluated on the grid
## ----------------------------------------------------------------------------
design <- function(X, spec) {
  switch(spec,
         rich   = cbind(X, x1sq = X[, 1]^2, x3sq = X[, 3]^2),   # contains the truth
         linear = X,                                            # misses the squares
         nox1   = cbind(X[, 2:4, drop = FALSE], x3sq = X[, 3]^2), # deployed score: no x1
         none   = NULL,                                         # ignores X (Nelson-Aalen)
         app_out  = X[, c("age", "male", "hgb", "creat", "mspike"), drop = FALSE],
         app_cens = X,
         stop("unknown spec"))
}

breslow_grid <- function(Y, ev, lp, tg) {
  o  <- order(Y); Ys <- Y[o]; evs <- ev[o]; r <- exp(lp[o])
  Rs <- rev(cumsum(rev(r)))
  Rs <- Rs[match(Ys, Ys)]                      # risk set {j: Y_j >= Y_i}, handles ties
  cum <- cumsum(evs / Rs)
  c(0, cum)[findInterval(tg, Ys) + 1L]
}

## Returns a list (one per element of newX) of n_new x G cumulative-hazard matrices
cox_grid <- function(Y, ev, X, spec, newX, tg) {
  Z <- design(X, spec)
  if (is.null(Z)) {
    b <- NULL; lp <- rep(0, length(Y))
  } else {
    fit <- coxph(Surv(Y, ev) ~ Z, ties = "breslow")
    b <- coef(fit); b[is.na(b)] <- 0
    lp <- drop(Z %*% b)
  }
  H0 <- breslow_grid(Y, ev, lp, tg)
  lapply(newX, function(Xn) {
    lpn <- if (is.null(b)) rep(0, nrow(Xn)) else drop(design(Xn, spec) %*% b)
    outer(exp(lpn), H0)
  })
}

## Random survival forests (scenario S6; needs package randomForestSRC)
rf_grid <- function(d1, newX, tg, cfg) {
  if (!requireNamespace("randomForestSRC", quietly = TRUE))
    stop("Scenario S6 needs: install.packages('randomForestSRC')")
  df_cr <- data.frame(Y = d1$Y, st = d1$J, d1$X)
  f_cr  <- randomForestSRC::rfsrc(Surv(Y, st) ~ ., data = df_cr, ntree = 300,
                                  nodesize = 15, splitrule = "logrankCR")
  df_c  <- data.frame(Y = d1$Y, st = 1L - d1$D, d1$X)
  f_c   <- randomForestSRC::rfsrc(Surv(Y, st) ~ ., data = df_c, ntree = 300, nodesize = 15)
  to_grid <- function(M, times) {
    M <- as.matrix(M)
    cbind(0, M)[, findInterval(tg, times) + 1L, drop = FALSE]
  }
  H1 <- H2 <- HC <- list()
  for (nm in names(newX)) {
    nd <- data.frame(newX[[nm]])
    p  <- predict(f_cr, newdata = nd)
    H1[[nm]] <- to_grid(p$chf[, , 1], p$time.interest)
    H2[[nm]] <- to_grid(p$chf[, , 2], p$time.interest)
    pc <- predict(f_c, newdata = nd)
    HC[[nm]] <- to_grid(pc$chf, pc$time.interest)
  }
  list(H1 = H1, H2 = H2, HC = HC)
}

cum_inc <- function(H1, H2) {
  S  <- exp(-(H1 + H2))
  Sm <- cbind(1, S[, -ncol(S), drop = FALSE])  # left limit S(u-)
  rowCumsum(Sm * rowDiff(H1))
}

## ----------------------------------------------------------------------------
## Calibration engine
##   eta(u | x) = 1 - {A(L) - A(u)} / B(u)   for u <= L
##   proposed : A = F1-hat, B = S-hat, censoring hazard = Lambda_C-hat
##   naive    : A = 1 - exp(-Lambda_1), B = exp(-Lambda_1), censoring = Lambda_C + Lambda_2
## ----------------------------------------------------------------------------
prep_engine <- function(A, B, Hcens, cfg) {
  Gm <- pmax(exp(-Hcens), cfg$g_min)
  B  <- pmax(B, cfg$s_min)
  ## Compensator weights w_k = 1/G(u_k) - 1/G(u_{k-1}) = dLambda*(u_k)/G(u_k), where
  ## dLambda* = -dG/G(u-) is the hazard of which G is the product integral. This keeps the
  ## identity Delta/G(V) = 1 - int dM/G exact for step-function (Breslow) estimators
  ## (Section 3.5 and Remark 3 of the paper).
  w  <- rowDiff(1 / Gm); w[, 1] <- 0
  list(A = A, B = B, Gm = Gm,
       CS1 = rowCumsum(w), CS2 = rowCumsum(w / B), CS3 = rowCumsum(w * A / B))
}

## Estimating functions at one beta.
## kL: grid index of L(X_i); kY: grid index of Y_i; ev: 1 if an event of a type that
## is NOT treated as censoring was observed; c1: 1 if that event is cause 1.
U_beta <- function(kL, kY, ev, c1, E, cc) {
  ii   <- seq_along(kL)
  kV   <- pmin(kY, kL)
  past <- kY >= kL                             # Y >= L: phi = 1 is known
  obs  <- (!past) & (ev == 1)                  # event observed before L
  cens <- (!past) & (ev == 0)                  # censored before L
  phi  <- ifelse(past, 1, 1 - c1)
  iV   <- cbind(ii, kV)
  Gv   <- E$Gm[iV]
  ipcw <- ifelse(past | obs, (phi - cc) / Gv, 0)
  AL   <- E$A[cbind(ii, kL)]
  iY   <- cbind(ii, kY)
  etaY <- 1 - (AL - E$A[iY]) / E$B[iY]
  jump <- ifelse(cens, (etaY - cc) / Gv, 0)
  comp <- (1 - cc) * E$CS1[iV] - AL * E$CS2[iV] + E$CS3[iV]
  c(ipcw = mean(ipcw), aipcw = mean(ipcw + jump - comp))
}

calibrate <- function(qidx, kY, ev, c1, E, cfg, betas) {
  cc <- 1 - cfg$alpha
  U <- t(vapply(seq_along(betas),
                function(b) U_beta(qidx[, b], kY, ev, c1, E, cc), numeric(2)))
  c(ipcw  = max(betas[U[, "ipcw"]  >= 0]),
    aipcw = max(betas[U[, "aipcw"] >= 0]))
}

cor_beta <- function(qidx, F1, cc, betas) {
  ii  <- seq_len(nrow(F1))
  cov <- vapply(seq_along(betas), function(b) mean(1 - F1[cbind(ii, qidx[, b])]), 0)
  max(betas[cov >= cc])
}

## grid index of q1(beta | x) = inf{t : F1(t | x) >= beta}, capped at tau
qidx_mat <- function(F1, betas, G) {
  t(apply(F1, 1, function(f) pmin(findInterval(betas, f, left.open = TRUE) + 1L, G)))
}
q_at <- function(F1, beta, G) pmin(rowSums(F1 < beta) + 1L, G)

## ----------------------------------------------------------------------------
## One replication
## ----------------------------------------------------------------------------
one_rep <- function(r, sc, cfg) {
  set.seed(cfg$seed0 + 100000L * (sc$id + 1L) + 1000L * (sc$n %/% 100L) + r)
  n     <- sc$n
  tg    <- seq(0, cfg$tau, length.out = cfg$G)
  betas <- seq(0, 1, by = cfg$beta_step)
  cc    <- 1 - cfg$alpha

  d1  <- gen_data(n, sc$a2, cfg)             # training
  d2  <- gen_data(n, sc$a2, cfg)             # calibration
  Xte <- gen_X(cfg$n_test)                   # test covariates
  newX <- list(cal = d2$X, test = Xte)

  ## (i) score model: a working model whose CIF defines L_beta(x) = q1(beta | x) ^ tau.
  ##     It may be poor; validity of IPCW/AIPCW does not require it to be correct.
  S1h <- cox_grid(d1$Y, as.integer(d1$J == 1L), d1$X, sc$score, newX, tg)
  S2h <- cox_grid(d1$Y, as.integer(d1$J == 2L), d1$X, sc$score, newX, tg)
  F1s_c <- cum_inc(S1h$cal, S2h$cal)
  F1s_t <- cum_inc(S1h$test, S2h$test)
  ## (ii) nuisance models used for calibration: (F1, S) and the censoring hazard
  if (sc$learner == "cox") {
    if (identical(sc$out, sc$score)) { H1 <- S1h; H2 <- S2h } else {
      H1 <- cox_grid(d1$Y, as.integer(d1$J == 1L), d1$X, sc$out, newX, tg)
      H2 <- cox_grid(d1$Y, as.integer(d1$J == 2L), d1$X, sc$out, newX, tg)
    }
    HC <- cox_grid(d1$Y, 1L - d1$D, d1$X, sc$cens, newX, tg)
  } else {
    rf <- rf_grid(d1, newX, tg, cfg); H1 <- rf$H1; H2 <- rf$H2; HC <- rf$HC
  }
  F1c <- cum_inc(H1$cal, H2$cal)
  F1t <- cum_inc(H1$test, H2$test)
  Sc  <- exp(-(H1$cal + H2$cal))
  kY  <- findInterval(d2$Y, tg)
  qidx <- qidx_mat(F1s_c, betas, cfg$G)

  ## proposed IPCW / AIPCW
  E  <- prep_engine(F1c, Sc, HC$cal, cfg)
  bp <- calibrate(qidx, kY, d2$D, as.integer(d2$J == 1L), E, cfg, betas)
  ## naive: competing event treated as censoring (base method applied to cause 1)
  En <- prep_engine(1 - exp(-H1$cal), exp(-H1$cal), HC$cal + H2$cal, cfg)
  bn <- calibrate(qidx, kY, as.integer(d2$J == 1L), rep(1L, n), En, cfg, betas)
  ## calibrated outcome regression
  bcor <- cor_beta(qidx, F1c, cc, betas)

  bet <- c(OR = cfg$alpha, COR = bcor, IPCW = bp[["ipcw"]], AIPCW = bp[["aipcw"]],
           "Naive-IPCW" = bn[["ipcw"]], "Naive-AIPCW" = bn[["aipcw"]])

  ## exact evaluation from the true CIF
  Ftrue <- true_F1(Xte, sc$a2, cfg, tg)
  hi <- Ftrue[, cfg$G] >= quantile(Ftrue[, cfg$G], 0.75)   # top-risk quartile
  ii <- seq_len(cfg$n_test)
  ev_one <- function(k) {
    cov_i <- 1 - Ftrue[cbind(ii, k)]
    c(cov = mean(cov_i), cov_hi = mean(cov_i[hi]), meanL = mean(tg[k]),
      fracTau = mean(k == cfg$G))
  }
  ## OR = q1(alpha | x) from the nuisance outcome model; all other methods calibrate the score
  res <- t(vapply(names(bet), function(m) {
    k <- if (m == "OR") q_at(F1t, cfg$alpha, cfg$G) else q_at(F1s_t, bet[[m]], cfg$G)
    ev_one(k)
  }, numeric(4)))
  orc <- ev_one(q_at(Ftrue, cfg$alpha, cfg$G))
  out <- rbind(Oracle = orc, res)
  pos_trunc <- mean(exp(-HC$cal[, cfg$G]) < cfg$g_min)   # share of calibration subjects truncated at tau
  data.frame(scenario = sc$name, n = n, rep = r, method = rownames(out),
             beta = c(NA, bet), out, pos_trunc = pos_trunc, row.names = NULL)
}

## ----------------------------------------------------------------------------
## Scenarios
## ----------------------------------------------------------------------------
SCEN <- list(
  ## score = model defining the bound; out / cens = nuisance models used for calibration
  S0 = list(id = 0, name = "S0 good score, nuisances correct", score = "rich", out = "rich",   cens = "rich", a2 = -0.4, learner = "cox"),
  S1 = list(id = 1, name = "S1 poor score, nuisances correct", score = "nox1", out = "rich",   cens = "rich", a2 = -0.4, learner = "cox"),
  S2 = list(id = 2, name = "S2 poor score, censoring wrong",   score = "nox1", out = "rich",   cens = "none", a2 = -0.4, learner = "cox"),
  S3 = list(id = 3, name = "S3 poor score, outcome wrong",     score = "nox1", out = "linear", cens = "rich", a2 = -0.4, learner = "cox"),
  S4 = list(id = 4, name = "S4 poor score, both wrong",        score = "nox1", out = "linear", cens = "none", a2 = -0.4, learner = "cox"),
  S5 = list(id = 5, name = "S5 heavy competing risk",          score = "nox1", out = "rich",   cens = "rich", a2 =  0.6, learner = "cox"),
  S6 = list(id = 6, name = "S6 random forest nuisances",       score = "nox1", out = NA,       cens = NA,     a2 = -0.4, learner = "rf")
)

run_scen <- function(sc, cfg, R, cores) {
  if (cores > 1L) {
    cl <- makeCluster(cores); on.exit(stopCluster(cl))
    clusterEvalQ(cl, {
      suppressPackageStartupMessages(library(survival)); options(rf.cores = 1L, mc.cores = 1L)
    })
    clusterExport(cl, ls(envir = globalenv()), envir = globalenv())
    res <- parLapply(cl, seq_len(R), one_rep, sc = sc, cfg = cfg)
  } else {
    res <- lapply(seq_len(R), one_rep, sc = sc, cfg = cfg)
  }
  do.call(rbind, res)
}

summarise_res <- function(df, alpha) {
  meth <- c("Oracle", "OR", "COR", "IPCW", "AIPCW", "Naive-IPCW", "Naive-AIPCW")
  key  <- interaction(df$scenario, df$n, df$method, drop = TRUE)
  out <- do.call(rbind, lapply(split(df, key), function(d) data.frame(
    scenario = d$scenario[1], n = d$n[1], method = d$method[1], reps = nrow(d),
    cov_mean = mean(d$cov), cov_sd = sd(d$cov), cov_mcse = sd(d$cov) / sqrt(nrow(d)),
    cov_q10 = unname(quantile(d$cov, 0.10)),
    pr_below = mean(d$cov < 1 - alpha - 0.01),          # fraction of runs > 1 pt below target
    cov_hi = mean(d$cov_hi), meanL = mean(d$meanL), fracTau = mean(d$fracTau),
    beta_mean = mean(d$beta), pos_trunc = mean(d$pos_trunc))))
  out$method <- factor(out$method, levels = meth)
  out <- out[order(out$scenario, out$n, out$method), ]
  rownames(out) <- NULL
  out
}

## ----------------------------------------------------------------------------
## Population-level check of double robustness (Web Appendix C / Table A1)
## Constant cause-specific hazards, covariate-dependent censoring, true nuisances
## known in closed form; the integral is computed by the midpoint rule.
## ----------------------------------------------------------------------------
dr_check <- function(n = 4e5, seed = 2026L, grid = 400L) {
  set.seed(seed)
  x   <- runif(n)
  l1  <- 0.5 * exp(x); l2 <- 0.8 * exp(-x); lam <- l1 + l2
  lc  <- 0.3 * exp(1.2 * x)
  T   <- rexp(n, lam); J <- ifelse(runif(n) < l1 / lam, 1L, 2L); C <- rexp(n, lc)
  Y   <- pmin(T, C); D <- as.integer(T <= C)
  L   <- pmin(1.5 + 1.5 * x, 3)
  psi <- mean(1 - ((T < L) & (J == 1L)))            # true coverage (full data)
  past <- Y >= L; obs <- (!past) & D == 1L; cens <- (!past) & D == 0L
  phi  <- ifelse(past, 1, ifelse(J == 1L, 0, 1))
  V    <- pmin(Y, L)
  eta  <- function(u, a1, a2) { s <- a1 + a2; ifelse(u < L, 1 - a1 / s * (1 - exp(-s * pmax(L - u, 0))), 1) }
  est  <- function(lch, a1, a2) {
    G    <- function(u) exp(-lch * u)
    ipcw <- ifelse(past | obs, phi / G(V), 0)
    jump <- ifelse(cens, eta(Y, a1, a2) / G(Y), 0)
    comp <- numeric(n)
    for (k in seq_len(grid)) { u <- (k - 0.5) / grid * V; comp <- comp + eta(u, a1, a2) / G(u) * lch }
    comp <- comp * V / grid
    c(IPCW = mean(ipcw), AIPCW = mean(ipcw + jump - comp))
  }
  lc_bad <- rep(mean(lc), n); l1_bad <- rep(mean(l1), n); l2_bad <- rep(2.5, n)
  tab <- rbind(
    "both correct"                         = est(lc, l1, l2),
    "censoring wrong, (F1,S) correct"      = est(lc_bad, l1, l2),
    "censoring correct, outcome wrong"     = est(lc, l1_bad, l2_bad),
    "censoring wrong, only cause-2 wrong"  = est(lc_bad, l1, l2_bad),
    "both wrong"                           = est(lc_bad, l1_bad, l2_bad))
  list(psi = psi, mc_se = sd(ifelse(past | obs, phi / exp(-lc * V), 0)) / sqrt(n), table = round(tab, 4))
}

## ----------------------------------------------------------------------------
## Application: mgus2 (survival package). Cause 1 = progression to plasma-cell
## malignancy, cause 2 = death without progression. Time in months.
## Coverage on the test third is estimated by IPCW, AIPCW and OR evaluators that
## use nuisance models fitted on the training third.
## ----------------------------------------------------------------------------
app_mgus2 <- function(cfg, n_split = 100L, alpha = 0.05, tau = 180, cores = 1L) {
  d <- survival::mgus2
  need <- c("age", "sex", "dxyr", "hgb", "creat", "mspike", "ptime", "pstat", "futime", "death")
  miss <- setdiff(need, names(d))
  if (length(miss)) stop("mgus2 lacks columns: ", paste(miss, collapse = ", "))
  d <- d[complete.cases(d[, need]), ]
  Y <- ifelse(d$pstat == 1, d$ptime, d$futime)
  J <- ifelse(d$pstat == 1, 1L, ifelse(d$death == 1, 2L, 0L))
  ok <- Y > 0; d <- d[ok, ]; Y <- Y[ok]; J <- J[ok]
  X <- cbind(age = (d$age - 70) / 10, male = as.numeric(d$sex == "M"),
             hgb = (d$hgb - 13) / 2, creat = log(d$creat), mspike = d$mspike,
             dxyr = (d$dxyr - 1980) / 10)
  desc <- list(n = nrow(X), events = table(factor(J, 0:2, c("censored", "progression", "death"))),
               max_follow_up = max(Y))
  acfg <- cfg; acfg$tau <- tau; acfg$alpha <- alpha
  one_split <- function(s) {
    set.seed(cfg$seed0 + 777000L + s)
    n  <- nrow(X); id <- sample(rep(1:3, length.out = n))
    tr <- id == 1; ca <- id == 2; te <- id == 3
    tg <- seq(0, acfg$tau, length.out = acfg$G); betas <- seq(0, 1, by = acfg$beta_step)
    cc <- 1 - acfg$alpha
    newX <- list(cal = X[ca, , drop = FALSE], test = X[te, , drop = FALSE])
    H1 <- cox_grid(Y[tr], as.integer(J[tr] == 1L), X[tr, ], "app_out",  newX, tg)
    H2 <- cox_grid(Y[tr], as.integer(J[tr] == 2L), X[tr, ], "app_out",  newX, tg)
    HC <- cox_grid(Y[tr], as.integer(J[tr] == 0L), X[tr, ], "app_cens", newX, tg)
    F1c <- cum_inc(H1$cal, H2$cal); F1t <- cum_inc(H1$test, H2$test)
    Sc  <- exp(-(H1$cal + H2$cal)); St <- exp(-(H1$test + H2$test))
    Dc  <- as.integer(J[ca] > 0L); Dt <- as.integer(J[te] > 0L)
    kYc <- findInterval(Y[ca], tg); kYt <- findInterval(Y[te], tg)
    qidx <- qidx_mat(F1c, betas, acfg$G)
    E  <- prep_engine(F1c, Sc, HC$cal, acfg)
    bp <- calibrate(qidx, kYc, Dc, as.integer(J[ca] == 1L), E, acfg, betas)
    En <- prep_engine(1 - exp(-H1$cal), exp(-H1$cal), HC$cal + H2$cal, acfg)
    bn <- calibrate(qidx, kYc, as.integer(J[ca] == 1L), rep(1L, sum(ca)), En, acfg, betas)
    bet <- c(OR = acfg$alpha, COR = cor_beta(qidx, F1c, cc, betas), IPCW = bp[["ipcw"]],
             AIPCW = bp[["aipcw"]], "Naive-IPCW" = bn[["ipcw"]], "Naive-AIPCW" = bn[["aipcw"]])
    Et <- prep_engine(F1t, St, HC$test, acfg)
    ii <- seq_len(sum(te))
    res <- t(vapply(names(bet), function(m) {
      k  <- q_at(F1t, bet[[m]], acfg$G)
      ev  <- U_beta(k, kYt, Dt, as.integer(J[te] == 1L), Et, 0)   # cc = 0: estimates of coverage
      ev1 <- U_beta(k, kYt, Dt, rep(0L, sum(te)), Et, 0)           # phi = 1: normaliser sum(Delta/G)/n
      c(cov_ipcw = ev[["ipcw"]] / ev1[["ipcw"]],                   # Hajek (normalised) IPCW
        cov_ipcw_ht = ev[["ipcw"]], cov_aipcw = ev[["aipcw"]], cov_or = mean(1 - F1t[cbind(ii, k)]),
        meanL = mean(tg[k]), medL = median(tg[k]), fracTau = mean(k == acfg$G), ipcw_norm = ev1[["ipcw"]])
    }, numeric(8)))
    data.frame(split = s, method = rownames(res), beta = bet, res, row.names = NULL)
  }
  if (cores > 1L) {
    cl <- makeCluster(cores); on.exit(stopCluster(cl))
    clusterEvalQ(cl, suppressPackageStartupMessages(library(survival)))
    clusterExport(cl, ls(envir = globalenv()), envir = globalenv())
    clusterExport(cl, c("X", "Y", "J", "acfg", "cfg"), envir = environment())
    out <- parLapply(cl, seq_len(n_split), one_split)
  } else out <- lapply(seq_len(n_split), one_split)
  res <- do.call(rbind, out)
  summ <- do.call(rbind, lapply(split(res, res$method), function(r) data.frame(
    method = r$method[1], beta = mean(r$beta), cov_ipcw = mean(r$cov_ipcw), cov_ipcw_sd = sd(r$cov_ipcw),
    cov_ipcw_ht = mean(r$cov_ipcw_ht), ipcw_norm = mean(r$ipcw_norm),
    cov_aipcw = mean(r$cov_aipcw), cov_aipcw_sd = sd(r$cov_aipcw), cov_or = mean(r$cov_or),
    meanL = mean(r$meanL), medL = mean(r$medL), fracTau = mean(r$fracTau))))
  lev <- c("OR", "COR", "IPCW", "AIPCW", "Naive-IPCW", "Naive-AIPCW")
  summ <- summ[order(match(summ$method, lev)), ]; rownames(summ) <- NULL
  list(desc = desc, alpha = alpha, tau = tau, results = res, summary = summ)
}

## ----------------------------------------------------------------------------
## Figures (no titles, vector output, panel labels (a), (b), ...).
## Built from the saved results in OUT_DIR, so stage "figs" can be run on its own.
## Options: see FIG_* in the settings block at the top of this file.
## ----------------------------------------------------------------------------
fig_opts <- function() {
  list(family = setting("FIG_FAMILY", "Helvetica"), pointsize = setting("FIG_POINTSIZE", 9),
       format = setting("FIG_FORMAT", "both"), names = setting("FIG_NAMES", "springer"),
       scale = setting("FIG_SCALE", 1), embed = setting("FIG_EMBED", FALSE),
       color = setting("FIG_COLOR", TRUE), device = setting("FIG_DEVICE", "cairo"))
}

make_figures <- function(out_dir = "out_cr", alpha = 0.1, opt = fig_opts()) {
  fig_dir <- file.path(out_dir, "figs"); dir.create(fig_dir, showWarnings = FALSE)
  stopifnot(opt$format %in% c("pdf", "eps", "both"), opt$names %in% c("descriptive", "springer"))
  fmts <- if (opt$format == "both") c("pdf", "eps") else opt$format
  stub <- function(desc, k) if (opt$names == "springer") paste0("Fig", k) else desc
  METH <- c("OR", "COR", "IPCW", "AIPCW", "Naive-IPCW", "Naive-AIPCW")
  LAB  <- c("OR", "COR", "IPCW", "AIPCW", "N-IPCW", "N-AIPCW")
  base_cols <- if (isTRUE(opt$color)) c("#4D4D4D", "#4D4D4D", "#4D4D4D", "#1F6FD1", "#E0622A", "#E0622A")
               else c("#4D4D4D", "#4D4D4D", "#4D4D4D", "#000000", "#8C8C8C", "#8C8C8C")
  COLS <- base_cols
  ## solid tints instead of transparency, so that EPS output is identical to PDF
  tint <- function(col, f = 0.85) { m <- grDevices::col2rgb(col) / 255
    grDevices::rgb(m[1, ] + (1 - m[1, ]) * f, m[2, ] + (1 - m[2, ]) * f, m[3, ] + (1 - m[3, ]) * f) }
  FILL <- tint(COLS)
  made <- character(0)
  save_fig <- function(name, w, h, draw) {
    for (fm in fmts) {
      f <- file.path(fig_dir, paste0(name, ".", fm))
      use_cairo <- identical(opt$device, "cairo") && isTRUE(capabilities("cairo"))
      if (identical(opt$device, "cairo") && !use_cairo) message("cairo not available; using base devices")
      if (use_cairo) {
        fam <- switch(opt$family, Helvetica = "Arial", Times = "Times New Roman", opt$family)
        if (fm == "pdf") {
          grDevices::cairo_pdf(f, width = w * opt$scale, height = h * opt$scale, family = fam,
                               pointsize = opt$pointsize)
        } else {
          grDevices::cairo_ps(f, width = w * opt$scale, height = h * opt$scale, family = fam,
                              pointsize = opt$pointsize, onefile = FALSE, fallback_resolution = 600)
        }
      } else if (fm == "pdf") {
        grDevices::pdf(f, width = w * opt$scale, height = h * opt$scale, family = opt$family,
                       pointsize = opt$pointsize, useDingbats = FALSE)
      } else {
        grDevices::postscript(f, width = w * opt$scale, height = h * opt$scale, family = opt$family,
                              pointsize = opt$pointsize, horizontal = FALSE, onefile = FALSE,
                              paper = "special")
      }
      ok <- tryCatch({ draw(); TRUE }, finally = grDevices::dev.off())
      if (isTRUE(opt$embed)) {
        gs <- tools::find_gs_cmd()
        if (nzchar(gs)) tryCatch(grDevices::embedFonts(f), error = function(e)
          message("embedFonts failed for ", f, ": ", conditionMessage(e)))
        else message("Ghostscript not found; fonts not embedded in ", f)
      }
      made <<- c(made, basename(f))
    }
  }
  tag <- function(i) mtext(sprintf("(%s)", letters[i]), side = 3, adj = 0, line = 0.25, font = 2, cex = 0.95)
  grid_h <- function(at) abline(h = at, col = "grey92", lwd = 0.6)
  rot_labels <- function(at, labs) {
    u <- par("usr")
    text(at, u[3] - 0.035 * diff(u[3:4]), labs, srt = 40, adj = c(1, 1), xpd = NA, cex = 0.9)
  }
  box_panel <- function(d, ylim, i, target = 1 - alpha) {
    vals <- lapply(METH, function(m) d$cov[d$method == m])
    plot(NA, xlim = c(0.5, 6.5), ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
    yt <- pretty(ylim, 5); grid_h(yt)
    abline(h = target, lty = 2, col = "grey40", lwd = 0.8)
    boxplot(vals, at = 1:6, add = TRUE, outline = FALSE, boxwex = 0.55, col = FILL, border = COLS,
            lwd = 0.8, medlwd = 1.4, whisklty = 1, staplewex = 0.35, xaxt = "n", yaxt = "n", frame.plot = FALSE)
    points(1:6, vapply(vals, mean, 0), pch = 21, bg = "white", col = COLS, cex = 0.85, lwd = 1.1)
    axis(2, at = yt, las = 1, lwd = 0.6, tcl = -0.25, cex.axis = 0.9)
    axis(1, at = 1:6, labels = FALSE, lwd = 0.6, tcl = -0.25)
    rot_labels(1:6, LAB); if (!is.null(i)) tag(i)
  }
  whisk_range <- function(v, g) range(unlist(lapply(split(v, g), function(x) boxplot.stats(x)$stats[c(1, 5)]))) + c(-0.004, 0.004)

  ## ---- Figure 1: coverage across replications, main scenarios
  f_main <- file.path(out_dir, "results_main.rds")
  if (file.exists(f_main)) {
    rm_ <- readRDS(f_main); rm_ <- rm_[rm_$method %in% METH, ]
    scs <- sort(unique(rm_$scenario))
    ylim <- whisk_range(rm_$cov, interaction(rm_$scenario, rm_$method, drop = TRUE))
    save_fig(stub("fig_coverage", 1), 7.0, 4.9, function() {
      par(mfrow = c(2, 3), mar = c(3.7, 2.8, 1.3, 0.5), oma = c(0, 1.4, 0, 0), mgp = c(2, 0.45, 0))
      for (k in seq_along(scs)) box_panel(rm_[rm_$scenario == scs[k], ], ylim, k)
      mtext("Coverage", side = 2, outer = TRUE, line = 0.2, cex = 0.95)
    })
  }
  ## ---- Figure 2: coverage against sample size (S1-S3; IPCW, AIPCW, COR)
  f_sw <- file.path(out_dir, "results_nsweep.rds")
  if (file.exists(f_main) && file.exists(f_sw)) {
    rs <- rbind(readRDS(f_main), readRDS(f_sw))
    rs <- rs[substr(rs$scenario, 1, 2) %in% c("S1", "S2", "S3") & rs$method %in% c("IPCW", "AIPCW", "COR"), ]
    agg <- do.call(rbind, lapply(split(rs, interaction(rs$scenario, rs$n, rs$method, drop = TRUE)), function(d)
      data.frame(scenario = d$scenario[1], n = d$n[1], method = d$method[1],
                 m = mean(d$cov), se = sd(d$cov) / sqrt(nrow(d)))))
    ns <- sort(unique(agg$n)); scs <- sort(unique(agg$scenario))
    ylim <- range(c(agg$m - 2 * agg$se, agg$m + 2 * agg$se, 1 - alpha)) + c(-0.002, 0.002)
    sty <- list(IPCW  = list(col = COLS[3], pch = 22, lty = 1, off = -0.07),
                AIPCW = list(col = COLS[4], pch = 21, lty = 1, off = 0),
                COR   = list(col = COLS[2], pch = 24, lty = 3, off = 0.07))
    save_fig(stub("fig_nsweep", 2), 7.0, 2.6, function() {
      par(mfrow = c(1, 3), mar = c(3.2, 2.8, 1.3, 0.5), oma = c(0, 1.4, 0, 0), mgp = c(1.9, 0.45, 0))
      for (k in seq_along(scs)) {
        plot(NA, xlim = c(0.6, length(ns) + 0.4), ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
        yt <- pretty(ylim, 5); grid_h(yt); abline(h = 1 - alpha, lty = 2, col = "grey40", lwd = 0.8)
        for (m in names(sty)) {
          a <- agg[agg$scenario == scs[k] & agg$method == m, ]; a <- a[order(a$n), ]
          x <- match(a$n, ns) + sty[[m]]$off
          arrows(x, a$m - 2 * a$se, x, a$m + 2 * a$se, angle = 90, code = 3, length = 0.02, col = sty[[m]]$col, lwd = 0.8)
          lines(x, a$m, col = sty[[m]]$col, lty = sty[[m]]$lty, lwd = 1.3)
          points(x, a$m, pch = sty[[m]]$pch, bg = "white", col = sty[[m]]$col, cex = 0.9, lwd = 1.1)
        }
        axis(1, at = seq_along(ns), labels = ns, lwd = 0.6, tcl = -0.25, cex.axis = 0.9)
        axis(2, at = yt, las = 1, lwd = 0.6, tcl = -0.25, cex.axis = 0.9)
        mtext("Sample size per split", side = 1, line = 1.9, cex = 0.85); tag(k)
        if (k == 1) legend("bottomright", legend = names(sty), col = vapply(sty, `[[`, "", "col"),
                           pch = vapply(sty, `[[`, 0, "pch"), lty = vapply(sty, `[[`, 0, "lty"),
                           pt.bg = "white", lwd = 1.2, bty = "n", cex = 0.85, seg.len = 2.2)
      }
      mtext("Mean coverage", side = 2, outer = TRUE, line = 0.2, cex = 0.95)
    })
  }
  ## ---- Figure 3: application, distribution over splits of the mean bound
  fa <- file.path(out_dir, sprintf("app_mgus2_alpha%02d.rds", c(5L, 10L)))
  if (all(file.exists(fa))) {
    aps <- lapply(fa, readRDS); tau <- aps[[1]]$tau
    save_fig(stub("fig_app", 3), 7.0, 2.8, function() {
      par(mfrow = c(1, 2), mar = c(3.7, 2.8, 1.3, 0.5), oma = c(0, 1.4, 0, 0), mgp = c(2, 0.45, 0))
      for (k in 1:2) {
        r <- aps[[k]]$results
        vals <- lapply(METH, function(m) r$meanL[r$method == m])
        ft <- vapply(METH, function(m) mean(r$fracTau[r$method == m]), 0)
        plot(NA, xlim = c(0.5, 6.5), ylim = c(0, tau * 1.12), xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
        yt <- pretty(c(0, tau), 5); grid_h(yt)
        abline(h = tau, lty = 2, col = "grey40", lwd = 0.8)
        boxplot(vals, at = 1:6, add = TRUE, outline = FALSE, boxwex = 0.55, col = FILL, border = COLS, lwd = 0.8,
                medlwd = 1.4, whisklty = 1, staplewex = 0.35, xaxt = "n", yaxt = "n", frame.plot = FALSE)
        text(1:6, tau * 1.075, sprintf("%.0f%%", 100 * ft), cex = 0.8, col = "grey20")
        axis(2, at = yt, las = 1, lwd = 0.6, tcl = -0.25, cex.axis = 0.9)
        axis(1, at = 1:6, labels = FALSE, lwd = 0.6, tcl = -0.25)
        rot_labels(1:6, LAB); tag(k)
        mtext(bquote(alpha == .(sprintf("%.2f", aps[[k]]$alpha))), side = 3, adj = 1, line = 0.25, cex = 0.9)
      }
      mtext("Mean lower predictive bound (months)", side = 2, outer = TRUE, line = 0.2, cex = 0.95)
    })
  }
  ## ---- Figure 4 (appendix / Web Figure 1): random survival forest nuisances (S6)
  f_rf <- file.path(out_dir, "results_rf.rds")
  if (file.exists(f_rf)) {
    rr <- readRDS(f_rf); rr <- rr[rr$method %in% METH, ]
    ylim <- whisk_range(rr$cov, rr$method)
    save_fig(stub("fig_rf", 4), 3.5, 2.9, function() {
      par(mar = c(3.7, 3.4, 0.6, 0.5), mgp = c(2.3, 0.45, 0))
      box_panel(rr, ylim, NULL); mtext("Coverage", side = 2, line = 2.3, cex = 0.95)
    })
  }
  made
}

## ============================================================================
## RUN
## ============================================================================
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
LOG <- file.path(OUT, "ALL_RESULTS.txt")
cat(sprintf("Reproduction run started %s | %s | cores = %d | stages: %s\n\n",
            format(Sys.time()), R.version.string, cores, paste(stages, collapse = ", ")), file = LOG)
logp <- function(x) { txt <- capture.output(print(x)); writeLines(txt); cat(txt, sep = "\n", file = LOG, append = TRUE) }
logc <- function(...) { txt <- sprintf(...); cat(txt, "\n"); cat(txt, "\n", file = LOG, append = TRUE) }

## Run a list of (scenario, n, R) cells, save per-replication results and the summary
run_plan <- function(tag, plan) {
  all_res <- list()
  for (p in plan) {
    sc <- SCEN[[p$sc]]; sc$n <- p$n; t0 <- Sys.time()
    logc("[%s] %s, n per split = %d, R = %d ...", tag, sc$name, p$n, p$R)
    all_res[[length(all_res) + 1L]] <- run_scen(sc, CFG, p$R, cores)
    logc("   done in %.1f sec", as.numeric(difftime(Sys.time(), t0, units = "secs")))
  }
  res <- do.call(rbind, all_res)
  saveRDS(res, file.path(OUT, sprintf("results_%s.rds", tag)))
  summ <- summarise_res(res, CFG$alpha)
  write.csv(summ, file.path(OUT, sprintf("summary_%s.csv", tag)), row.names = FALSE)
  num <- vapply(summ, is.numeric, TRUE); summ[num] <- lapply(summ[num], round, 4)
  logc("\n===== SUMMARY: %s =====", tag); logp(summ); logc("")
  invisible(summ)
}

## Run one stage if requested; a failing stage is reported and the others continue
stage <- function(name, expr) {
  if (!(name %in% stages)) return(invisible(NULL))
  t0 <- Sys.time()
  ok <- tryCatch({ force(expr); TRUE }, error = function(e) {
    logc("!!! STAGE '%s' FAILED: %s", name, conditionMessage(e)); FALSE })
  logc("--- stage '%s' %s (%.1f min)\n", name, if (ok) "finished" else "FAILED",
       as.numeric(difftime(Sys.time(), t0, units = "mins")))
}

## Quick check (not part of the paper)
stage("smoke", run_plan("smoke", list(list(sc = "S1", n = 1000, R = 3))))

## Table 1 and Figure 1: scenarios S0-S5, n = 1000 per split, 500 replications
stage("main", run_plan("main", lapply(c("S0", "S1", "S2", "S3", "S4", "S5"),
                                      function(s) list(sc = s, n = 1000, R = CFG$R))))

## Figure 2: scenarios S1-S3 with n = 500 and 2000 per split, 300 replications
stage("nsweep", run_plan("nsweep", unlist(lapply(c("S1", "S2", "S3"), function(s)
  lapply(c(500, 2000), function(nn) list(sc = s, n = nn, R = 300))), recursive = FALSE)))

## Scenario S6 (random survival forests), 200 replications
stage("rf", {
  if (!requireNamespace("randomForestSRC", quietly = TRUE))
    stop("package randomForestSRC is required: install.packages('randomForestSRC')")
  run_plan("rf", list(list(sc = "S6", n = 1000, R = 200)))
})

## Table A1 / Web Appendix C
stage("drcheck", {
  logc("===== POPULATION-LEVEL DOUBLE-ROBUSTNESS CHECK =====")
  dc <- dr_check()
  logc("true coverage psi = %.4f (Monte Carlo s.e. of IPCW with true G about %.4f)", dc$psi, dc$mc_se)
  logp(dc$table)
})

## Section 6: mgus2 application at alpha = 0.05 and 0.10, 100 random splits each
stage("app", {
  for (a in c(0.05, 0.10)) {
    ap <- app_mgus2(CFG, n_split = 100L, alpha = a, tau = 180, cores = cores)
    saveRDS(ap, file.path(OUT, sprintf("app_mgus2_alpha%02d.rds", round(100 * a))))
    logc("===== APPLICATION mgus2: alpha = %.2f, tau = %g months =====", a, ap$tau)
    logp(ap$desc)
    sm <- ap$summary; num <- vapply(sm, is.numeric, TRUE); sm[num] <- lapply(sm[num], round, 4)
    logp(sm)
  }
})

## Figures 1-4 (from the saved results)
stage("figs", {
  made <- make_figures(OUT, alpha = CFG$alpha)
  logc("===== FIGURES written to %s: %s", file.path(OUT, "figs"), paste(made, collapse = ", "))
})

logp(sessionInfo())
logc("\nDone %s. Summary of all results: %s", format(Sys.time()), normalizePath(LOG))
