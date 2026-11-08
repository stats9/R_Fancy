#!/usr/bin/env Rscript
## =============================================================================
## Simulation study for:
##   "Doubly robust calibration of lower predictive bounds for the time to a
##    cause-specific event under competing risks"
##
## Target: L(X) such that P{T1* >= L(X)} >= 1 - alpha, where T1* = T if J = 1
##         and T1* = +Inf otherwise, with L(X) capped at a horizon tau.
##
## Usage (run from a terminal; 'cores' is optional, default 1):
##   Rscript sim_cr.R smoke            # 3 replications of S1 and S2 + timing. RUN THIS FIRST.
##   Rscript sim_cr.R main   [cores]   # scenarios S0-S5, 500 replications each
##   Rscript sim_cr.R nsweep [cores]   # S2-S4 with n per split in {500, 2000}
##   Rscript sim_cr.R rf     [cores]   # S6: random survival forests (needs randomForestSRC)
##
## Output: results_<mode>.rds (one row per replication x method) and
##         summary_<mode>.csv (the table to send back), plus sessionInfo_<mode>.txt
##
## Packages: survival (required), parallel (base R), randomForestSRC (only for 'rf').
## =============================================================================

suppressPackageStartupMessages({
  library(survival)
  library(parallel)
})

CFG <- list(
  tau = 3,            # horizon: bounds are capped at tau
  G = 301,            # time-grid points on [0, tau]
  beta_step = 0.002,  # grid for the pseudo-quantile level beta
  alpha = 0.1,        # target miscoverage
  g_min = 0.05,       # truncation of estimated censoring survival (positivity)
  s_min = 0.02,       # truncation of estimated all-cause survival in eta
  n_test = 5000,      # test covariates; coverage is computed exactly from the true CIF
  R = 500,            # replications per scenario (main)
  k1 = 1.5, b1 = 3.0, # cause 1: Weibull cause-specific hazard
  l2 = 0.15,          # cause 2: exponential cause-specific hazard
  lC = 0.80,          # censoring: exponential hazard (about 58% censored)
  seed0 = 20261108L
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

## Random survival forests (scenario S6)
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
  w  <- rowDiff(Hcens) / Gm                    # dLambda_C(u) / G(u)
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
  data.frame(scenario = sc$name, n = n, rep = r, method = rownames(out),
             beta = c(NA, bet), out, row.names = NULL)
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
    beta_mean = mean(d$beta))))
  out$method <- factor(out$method, levels = meth)
  out <- out[order(out$scenario, out$n, out$method), ]
  rownames(out) <- NULL
  out
}

## ----------------------------------------------------------------------------
## Main
## ----------------------------------------------------------------------------
args  <- commandArgs(trailingOnly = TRUE)
## From RStudio / source(): set SIM_MODE and SIM_CORES before sourcing, e.g.
##   SIM_MODE <- "main"; SIM_CORES <- 4; source("sim_cr.R")
mode  <- if (exists("SIM_MODE"))  SIM_MODE  else if (length(args) >= 1L) args[1] else "smoke"
cores <- if (exists("SIM_CORES")) as.integer(SIM_CORES) else if (length(args) >= 2L) as.integer(args[2]) else 1L

plan <- switch(mode,
  smoke  = list(list(sc = "S1", n = 1000, R = 3), list(sc = "S2", n = 1000, R = 3)),
  main   = lapply(c("S0", "S1", "S2", "S3", "S4", "S5"), function(s) list(sc = s, n = 1000, R = CFG$R)),
  nsweep = unlist(lapply(c("S2", "S3", "S4"), function(s)
             lapply(c(500, 2000), function(nn) list(sc = s, n = nn, R = 300))), recursive = FALSE),
  rf     = list(list(sc = "S6", n = 1000, R = 200)),
  stop("mode must be one of: smoke, main, nsweep, rf"))

all_res <- list()
for (p in plan) {
  sc <- SCEN[[p$sc]]; sc$n <- p$n
  t0 <- Sys.time()
  cat(sprintf("Running %s (n per split = %d, R = %d, cores = %d) ...\n", sc$name, p$n, p$R, cores))
  all_res[[length(all_res) + 1L]] <- run_scen(sc, CFG, p$R, cores)
  cat(sprintf("  done in %.1f sec\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
res <- do.call(rbind, all_res)
saveRDS(res, sprintf("results_%s.rds", mode))
summ <- summarise_res(res, CFG$alpha)
write.csv(summ, sprintf("summary_%s.csv", mode), row.names = FALSE)
writeLines(capture.output(sessionInfo()), sprintf("sessionInfo_%s.txt", mode))
num <- vapply(summ, is.numeric, TRUE)
summ[num] <- lapply(summ[num], round, 4)
print(summ, row.names = FALSE)
