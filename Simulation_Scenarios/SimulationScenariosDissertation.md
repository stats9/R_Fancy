# Dissertation Simulation
Habib Ezzatabadi

- [Pre Code](#pre-code)
  - [Implement Models](#implement-models)
  - [Get Simulation 1 Result](#get-simulation-1-result)
- [Simulation I \| Scenario I: \$():~ -0.1, (): ~ 0.012, :
  0.3](#simulation-i--scenario-i---01---0012--03)
  - [CIF Prediction in Eval Time Method Simulation I \| Scenario
    I](#cif-prediction-in-eval-time-method-simulation-i--scenario-i)
    - [Risk I](#risk-i)
    - [Risk II](#risk-ii)
  - [MSE \| Method Simulation I \| Scenario
    I](#mse--method-simulation-i--scenario-i)
  - [Brier Score \| Method Simulation I \| Scenario
    I](#brier-score--method-simulation-i--scenario-i)
    - [Brier Average Result](#brier-average-result)
    - [Risk I](#risk-i-1)
    - [Risk II](#risk-ii-1)
  - [AUC Score \| Method Simulation I \| Scenario
    I](#auc-score--method-simulation-i--scenario-i)
    - [Average Result](#average-result)
    - [Risk I](#risk-i-2)
    - [Risk II](#risk-ii-2)
  - [BIAS \| Method Simulation I \| Scenario
    I](#bias--method-simulation-i--scenario-i)
    - [Risk I](#risk-i-3)
    - [Risk II](#risk-ii-3)
    - [Direct Coefficient Result Scenario
      I](#direct-coefficient-result-scenario-i)
- [Simulation I \| Scenario II: \$():~ 0.1, (): ~ 0.012, :
  0.3](#simulation-i--scenario-ii--01---0012--03)
  - [CIF Prediction in Eval Time Method Simulation I \| Scenario
    II](#cif-prediction-in-eval-time-method-simulation-i--scenario-ii)
    - [Risk I](#risk-i-4)
    - [Risk II](#risk-ii-4)
  - [MSE \| Method Simulation I \| Scenario
    II](#mse--method-simulation-i--scenario-ii)
  - [Brier Score \| Method Simulation I \| Scenario
    II](#brier-score--method-simulation-i--scenario-ii)
    - [Brier Average Result](#brier-average-result-1)
    - [Risk I](#risk-i-5)
    - [Risk II](#risk-ii-5)
  - [AUC Score \| Method Simulation I \| Scenario
    II](#auc-score--method-simulation-i--scenario-ii)
    - [Average Result](#average-result-1)
    - [Risk I](#risk-i-6)
    - [Risk II](#risk-ii-6)
  - [BIAS \| Method Simulation I \| Scenario
    II](#bias--method-simulation-i--scenario-ii)
    - [Risk I](#risk-i-7)
    - [Risk II](#risk-ii-7)
    - [Direct Coefficient Result Scenario
      II](#direct-coefficient-result-scenario-ii)
- [Simulation I \| Scenario III: \$():~ -0.1, (): ~ 0.012, :
  0.5](#simulation-i--scenario-iii---01---0012--05)
  - [CIF Prediction in Eval Time Method Simulation I \| Scenario
    III](#cif-prediction-in-eval-time-method-simulation-i--scenario-iii)
    - [Risk I](#risk-i-8)
    - [Risk II](#risk-ii-8)
  - [MSE \| Method Simulation I \| Scenario
    III](#mse--method-simulation-i--scenario-iii)
  - [Brier Score \| Method Simulation I \| Scenario
    III](#brier-score--method-simulation-i--scenario-iii)
    - [Brier Average Result](#brier-average-result-2)
    - [Risk I](#risk-i-9)
    - [Risk II](#risk-ii-9)
  - [AUC Score \| Method Simulation I \| Scenario
    III](#auc-score--method-simulation-i--scenario-iii)
    - [Average Result](#average-result-2)
    - [Risk I](#risk-i-10)
    - [Risk II](#risk-ii-10)
  - [BIAS \| Method Simulation I \| Scenario
    III](#bias--method-simulation-i--scenario-iii)
    - [Risk I](#risk-i-11)
    - [Risk II](#risk-ii-11)
    - [Direct Coefficient Result Scenario
      III](#direct-coefficient-result-scenario-iii)
- [Simulation I \| Scenario IV: \$():~ 0.1, (): ~ 0.012, :
  0.5](#simulation-i--scenario-iv--01---0012--05)
  - [CIF Prediction in Eval Time Method Simulation I \| Scenario
    IV](#cif-prediction-in-eval-time-method-simulation-i--scenario-iv)
    - [Risk I](#risk-i-12)
    - [Risk II](#risk-ii-12)
  - [MSE \| Method Simulation I \| Scenario
    IV](#mse--method-simulation-i--scenario-iv)
  - [Brier Score \| Method Simulation I \| Scenario
    IV](#brier-score--method-simulation-i--scenario-iv)
    - [Brier Average Result](#brier-average-result-3)
    - [Risk I](#risk-i-13)
    - [Risk II](#risk-ii-13)
  - [AUC Score \| Method Simulation I \| Scenario
    IV](#auc-score--method-simulation-i--scenario-iv)
    - [Average Result](#average-result-3)
    - [Risk I](#risk-i-14)
    - [Risk II](#risk-ii-14)
  - [BIAS \| Method Simulation I \| Scenario
    IV](#bias--method-simulation-i--scenario-iv)
    - [Risk I](#risk-i-15)
    - [Risk II](#risk-ii-15)
    - [Direct Coefficient Result Scenario
      IV](#direct-coefficient-result-scenario-iv)
- [Simulation I \| : \$():~ -0.1, (): ~ 0.012, :
  0.7](#simulation-i-----01---0012--07)
  - [CIF Prediction in Eval Time Method Simulation I \| Scenario
    V](#cif-prediction-in-eval-time-method-simulation-i--scenario-v)
    - [Risk I](#risk-i-16)
    - [Risk II](#risk-ii-16)
  - [MSE \| Method Simulation I \| Scenario
    V](#mse--method-simulation-i--scenario-v)
  - [Brier Score \| Method Simulation I \| Scenario
    V](#brier-score--method-simulation-i--scenario-v)
    - [Brier Average Result](#brier-average-result-4)
    - [Risk I](#risk-i-17)
    - [Risk II](#risk-ii-17)
  - [AUC Score \| Method Simulation I \| Scenario
    V](#auc-score--method-simulation-i--scenario-v)
    - [Average Result](#average-result-4)
    - [Risk I](#risk-i-18)
    - [Risk II](#risk-ii-18)
  - [BIAS \| Method Simulation I \| Scenario
    V](#bias--method-simulation-i--scenario-v)
    - [Risk I](#risk-i-19)
    - [Risk II](#risk-ii-19)
    - [Direct Coefficient Result Scenario
      V](#direct-coefficient-result-scenario-v)
- [Simulation I \| Scenario VI: \$():~ 0.1, (): ~ 0.012, :
  0.7](#simulation-i--scenario-vi--01---0012--07)
  - [CIF Prediction in Eval Time Method Simulation I \| Scenario
    VI](#cif-prediction-in-eval-time-method-simulation-i--scenario-vi)
    - [Risk I](#risk-i-20)
    - [Risk II](#risk-ii-20)
  - [MSE \| Method Simulation I \| Scenario
    VI](#mse--method-simulation-i--scenario-vi)
  - [Brier Score \| Method Simulation I \| Scenario
    VI](#brier-score--method-simulation-i--scenario-vi)
    - [Brier Average Result](#brier-average-result-5)
    - [Risk I](#risk-i-21)
    - [Risk II](#risk-ii-21)
  - [AUC Score \| Method Simulation I \| Scenario
    VI](#auc-score--method-simulation-i--scenario-vi)
    - [Average Result](#average-result-5)
    - [Risk I](#risk-i-22)
    - [Risk II](#risk-ii-22)
  - [BIAS \| Method Simulation I \| Scenario
    VI](#bias--method-simulation-i--scenario-vi)
    - [Risk I](#risk-i-23)
    - [Risk II](#risk-ii-23)
    - [Direct Coefficient Result Scenario
      VI](#direct-coefficient-result-scenario-vi)
- [Simulation II \| Scenario I: \$():~ -0.1, (): ~ 0.012, :
  0.3](#simulation-ii--scenario-i---01---0012--03)
  - [CIF Prediction in Eval Time Method Simulation II \| Scenario
    I](#cif-prediction-in-eval-time-method-simulation-ii--scenario-i)
    - [Risk I](#risk-i-24)
    - [Risk II](#risk-ii-24)
  - [MSE \| Method Simulation II \| Scenario
    I](#mse--method-simulation-ii--scenario-i)
  - [Brier Score \| Method Simulation II \| Scenario
    I](#brier-score--method-simulation-ii--scenario-i)
    - [Brier Average Result \| Simulation
      II](#brier-average-result--simulation-ii)
    - [Risk I](#risk-i-25)
    - [Risk II](#risk-ii-25)
  - [AUC Score \| Method Simulation II \| Scenario
    I](#auc-score--method-simulation-ii--scenario-i)
    - [Average Result](#average-result-6)
    - [Risk I](#risk-i-26)
    - [Risk II](#risk-ii-26)
  - [BIAS \| Method Simulation II \| Scenario
    I](#bias--method-simulation-ii--scenario-i)
    - [Risk I](#risk-i-27)
    - [Risk II](#risk-ii-27)
    - [Direct Coefficient Result Scenario
      I](#direct-coefficient-result-scenario-i-1)
- [Simulation II \| Scenario II: \$():~ 0.1, (): ~ 0.012, :
  0.3](#simulation-ii--scenario-ii--01---0012--03)
  - [CIF Prediction in Eval Time Method Simulation II \| Scenario
    II](#cif-prediction-in-eval-time-method-simulation-ii--scenario-ii)
    - [Risk I](#risk-i-28)
    - [Risk II](#risk-ii-28)
  - [MSE \| Method Simulation II \| Scenario
    II](#mse--method-simulation-ii--scenario-ii)
  - [Brier Score \| Method Simulation II \| Scenario
    II](#brier-score--method-simulation-ii--scenario-ii)
    - [Brier Average Result](#brier-average-result-6)
    - [Risk I](#risk-i-29)
    - [Risk II](#risk-ii-29)
  - [AUC Score \| Method Simulation II \| Scenario
    II](#auc-score--method-simulation-ii--scenario-ii)
    - [Average Result](#average-result-7)
    - [Risk I](#risk-i-30)
    - [Risk II](#risk-ii-30)
  - [BIAS \| Method Simulation II \| Scenario
    II](#bias--method-simulation-ii--scenario-ii)
    - [Risk I](#risk-i-31)
    - [Risk II](#risk-ii-31)
    - [Direct Coefficient Result Scenario
      II](#direct-coefficient-result-scenario-ii-1)
- [Simulation II \| Scenario III: \$():~ -0.1, (): ~ 0.012, :
  0.5](#simulation-ii--scenario-iii---01---0012--05)
  - [CIF Prediction in Eval Time Method Simulation II \| Scenario
    III](#cif-prediction-in-eval-time-method-simulation-ii--scenario-iii)
    - [Risk I](#risk-i-32)
    - [Risk II](#risk-ii-32)
  - [MSE \| Method Simulation II \| Scenario
    III](#mse--method-simulation-ii--scenario-iii)
  - [Brier Score \| Method Simulation II \| Scenario
    III](#brier-score--method-simulation-ii--scenario-iii)
    - [Brier Average Result](#brier-average-result-7)
    - [Risk I](#risk-i-33)
    - [Risk II](#risk-ii-33)
  - [AUC Score \| Method Simulation II \| Scenario
    III](#auc-score--method-simulation-ii--scenario-iii)
    - [Average Result](#average-result-8)
    - [Risk I](#risk-i-34)
    - [Risk II](#risk-ii-34)
  - [BIAS \| Method Simulation II \| Scenario
    III](#bias--method-simulation-ii--scenario-iii)
    - [Risk I](#risk-i-35)
    - [Risk II](#risk-ii-35)
    - [Direct Coefficient Result Scenario
      III](#direct-coefficient-result-scenario-iii-1)
- [Simulation II \| Scenario IV: \$():~ 0.1, (): ~ 0.012, :
  0.5](#simulation-ii--scenario-iv--01---0012--05)
  - [CIF Prediction in Eval Time Method Simulation II \| Scenario
    IV](#cif-prediction-in-eval-time-method-simulation-ii--scenario-iv)
    - [Risk I](#risk-i-36)
    - [Risk II](#risk-ii-36)
  - [MSE \| Method Simulation II \| Scenario
    IV](#mse--method-simulation-ii--scenario-iv)
  - [Brier Score \| Method Simulation II \| Scenario
    IV](#brier-score--method-simulation-ii--scenario-iv)
    - [Brier Average Result](#brier-average-result-8)
    - [Risk I](#risk-i-37)
    - [Risk II](#risk-ii-37)
  - [AUC Score \| Method Simulation II \| Scenario
    IV](#auc-score--method-simulation-ii--scenario-iv)
    - [Average Result](#average-result-9)
    - [Risk I](#risk-i-38)
    - [Risk II](#risk-ii-38)
  - [BIAS \| Method Simulation II \| Scenario
    IV](#bias--method-simulation-ii--scenario-iv)
    - [Risk I](#risk-i-39)
    - [Risk II](#risk-ii-39)
    - [Direct Coefficient Result Scenario
      IV](#direct-coefficient-result-scenario-iv-1)
- [Simulation II \| : \$():~ -0.1, (): ~ 0.012, :
  0.7](#simulation-ii-----01---0012--07)
  - [CIF Prediction in Eval Time Method Simulation II \| Scenario
    V](#cif-prediction-in-eval-time-method-simulation-ii--scenario-v)
    - [Risk I](#risk-i-40)
    - [Risk II](#risk-ii-40)
  - [MSE \| Method Simulation II \| Scenario
    V](#mse--method-simulation-ii--scenario-v)
  - [Brier Score \| Method Simulation II \| Scenario
    V](#brier-score--method-simulation-ii--scenario-v)
    - [Brier Average Result](#brier-average-result-9)
    - [Risk I](#risk-i-41)
    - [Risk II](#risk-ii-41)
  - [AUC Score \| Method Simulation II \| Scenario
    V](#auc-score--method-simulation-ii--scenario-v)
    - [Average Result](#average-result-10)
    - [Risk I](#risk-i-42)
    - [Risk II](#risk-ii-42)
  - [BIAS \| Method Simulation II \| Scenario
    V](#bias--method-simulation-ii--scenario-v)
    - [Risk I](#risk-i-43)
    - [Risk II](#risk-ii-43)
    - [Direct Coefficient Result Scenario
      V](#direct-coefficient-result-scenario-v-1)
- [Simulation II \| Scenario VI: \$():~ 0.1, (): ~ 0.012, :
  0.7](#simulation-ii--scenario-vi--01---0012--07)
  - [CIF Prediction in Eval Time Method Simulation II \| Scenario
    VI](#cif-prediction-in-eval-time-method-simulation-ii--scenario-vi)
    - [Risk I](#risk-i-44)
    - [Risk II](#risk-ii-44)
  - [MSE \| Method Simulation II \| Scenario
    VI](#mse--method-simulation-ii--scenario-vi)
  - [Brier Score \| Method Simulation II \| Scenario
    VI](#brier-score--method-simulation-ii--scenario-vi)
    - [Brier Average Result](#brier-average-result-10)
    - [Risk I](#risk-i-45)
    - [Risk II](#risk-ii-45)
  - [AUC Score \| Method Simulation II \| Scenario
    VI](#auc-score--method-simulation-ii--scenario-vi)
    - [Average Result](#average-result-11)
    - [Risk I](#risk-i-46)
    - [Risk II](#risk-ii-46)
  - [BIAS \| Method Simulation II \| Scenario
    VI](#bias--method-simulation-ii--scenario-vi)
    - [Risk I](#risk-i-47)
    - [Risk II](#risk-ii-47)
    - [Direct Coefficient Result Scenario
      VI](#direct-coefficient-result-scenario-vi-1)



# Pre Code

``` r
library(tidyverse)
library(cmpp)
```

``` r
# --- Silent Gompertz time generator for a single cause ---
rgomp_time <- function(n, lambda, gamma, condition_on_finite = TRUE) {
  draw <- function(m) {
    U <- runif(m)
    z <- 1 + (-log(U)) * gamma / lambda
    t <- rep(Inf, m)
    valid <- which(z > 0)
    t[valid] <- log(z[valid]) / gamma
    t
  }
  t <- draw(n)
  if (condition_on_finite && any(!is.finite(t))) {
    idx <- which(!is.finite(t))
    while (length(idx) > 0) {
      t[idx] <- draw(length(idx))
      idx <- which(!is.finite(t))
    }
  }
  t
}

# --- Competing risks simulation with distinct Gompertz hazards ---
simulate_cr_two_gompertz <- function(n = 200,
                                     lambda1, gamma1,
                                     lambda2, gamma2,
                                     censor_rate,
                                     seed = NULL,
                                     condition_on_finite = TRUE) {
  if (!is.null(seed)) set.seed(seed)

  # Latent times for each cause
  T1 <- rgomp_time(n, lambda1, gamma1, condition_on_finite)
  T2 <- rgomp_time(n, lambda2, gamma2, condition_on_finite)

  # Observed time and cause
  T <- pmin(T1, T2)
  cause <- ifelse(T1 < T2, 1L, 2L)
  ties <- which(T1 == T2)
  if (length(ties) > 0) cause[ties] <- sample(c(1L, 2L), length(ties), replace = TRUE)

  # Exact censoring
  n_cens <- round(n * censor_rate)
  idx_c  <- if (n_cens > 0) sample.int(n, n_cens) else integer(0)
  C_times <- if (n_cens > 0) runif(n_cens, 0, T[idx_c]) else numeric(0)

  time   <- T
  status <- rep(1L, n)
  if (n_cens > 0) {
    time[idx_c]   <- C_times
    status[idx_c] <- 0L
    cause[idx_c]  <- 0L
  }

  data.frame(
    id     = seq_len(n),
    time   = time,
    status = status,
    cause  = cause
  )
}

# --- Define simulation scenarios ---
scenarios_sim1 <- data.frame(
  scenario     = paste0("S", 1:6),
  censor_rate  = c(0.3, 0.3, 0.5, 0.5, 0.7, 0.7),
  gamma1       = c(-0.10, +0.10, -0.10, +0.10, -0.10, +0.10),
  gamma2       = c(-0.06, +0.06, -0.06, +0.06, -0.06, +0.06),
  lambda1      = 0.012,
  lambda2      = 0.010,
  stringsAsFactors = FALSE
)

# --- Run 1000 simulations per scenario 
run_simulations <- function(scenarios_sim1, n_rep = 1000, n = 200, base_seed = 20240810) {
  out <- vector("list", nrow(scenarios_sim1))
  names(out) <- paste0(
    scenarios_sim1$scenario, "_gamma=(", scenarios_sim1$gamma1, ",", scenarios_sim1$gamma2, ")_c=",
    scenarios_sim1$censor_rate
  )
  for (i in seq_len(nrow(scenarios_sim1))) {
    g1 <- scenarios_sim1$gamma1[i]; g2 <- scenarios_sim1$gamma2[i]
    l1 <- scenarios_sim1$lambda1[i]; l2 <- scenarios_sim1$lambda2[i]
    cr <- scenarios_sim1$censor_rate[i]
    sims <- vector("list", n_rep)
    for (r in seq_len(n_rep)) {
      seed_r <- base_seed + i * 100000 + r
      sims[[r]] <- simulate_cr_two_gompertz(
        n = n, lambda1 = l1, gamma1 = g1,
        lambda2 = l2, gamma2 = g2,
        censor_rate = cr, seed = seed_r,
        condition_on_finite = TRUE
      )
    }
    out[[i]] <- sims
  }
  out
}

# --- Execute all simulations ---
# sim_data_all <- run_simulations(scenarios, n_rep = 1000, n = 200)
sim_data_all <- readRDS("simData.rds")
# Example: inspect first simulation of first scenario
head(sim_data_all[[1]][[1]])
sim_data_all |> lengths()
dat11 <- sim_data_all[[1]][[1]]
dat11$cause |> table()
dat31 <- sim_data_all[[3]][[1]]
dat31$cause |> table()
dat31 |> names()
# saveRDS(sim_data_all, file = "simData.rds")
senarioNames <- sim_data_all |> names()
```



``` r
# Simulation script for competing risks CIF (direct Gompertz)
# - For each scenario: calibrate cause-balance (50/50) then generate n_rep datasets of size n
library(data.table)

# --- Direct Gompertz time generator ---
rgomp_direct <- function(n, alpha, beta, condition_on_finite = TRUE) {
  draw_once <- function(m) {
    u <- runif(m)
    t <- rep(Inf, m)
    if (abs(alpha) < 1e-12) {
      t <- -log(1 - u) / beta
      return(t)
    }
    inner <- 1 - (alpha / beta) * log(1 - u)
    valid <- which(inner > 0)
    if (length(valid) > 0) {
      t[valid] <- (1 / alpha) * log(inner[valid])
    }
    t
  }
  t <- draw_once(n)
  if (condition_on_finite && any(!is.finite(t))) {
    idx <- which(!is.finite(t))
    while (length(idx) > 0) {
      t[idx] <- draw_once(length(idx))
      idx <- which(!is.finite(t))
    }
  }
  t
}

# --- Simulate latent times for two causes ---
simulate_latent <- function(n, params1, params2, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  T1 <- rgomp_direct(n, params1["alpha"], params1["beta"])
  T2 <- rgomp_direct(n, params2["alpha"], params2["beta"])
  data.frame(T1 = T1, T2 = T2)
}

# --- Calibrate params2 for ~50/50 cause balance ---
calibrate_balance <- function(n_cal = 50000, params1, params2_base, tol = 0.002, max_iter = 30, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  f_lo <- 0.01; f_hi <- 100
  for (it in 1:max_iter) {
    f_mid <- exp((log(f_lo) + log(f_hi)) / 2)
    params2_try <- params2_base
    params2_try["beta"] <- params2_base["beta"] * f_mid
    lat <- simulate_latent(n_cal, params1, params2_try)
    cause <- ifelse(lat$T1 < lat$T2, 1L, 2L)
    prop1 <- mean(cause == 1L)
    if (abs(prop1 - 0.5) <= tol) {
      return(list(params2 = params2_try, prop1 = prop1))
    }
    if (prop1 > 0.5) {
      f_lo <- f_mid
    } else {
      f_hi <- f_mid
    }
  }
  params2_try <- params2_base
  params2_try["beta"] <- params2_base["beta"] * f_mid
  list(params2 = params2_try, prop1 = mean(ifelse(lat$T1 < lat$T2,1,2) == 1))
}

# --- Apply censoring with exact target proportion ---
apply_censoring <- function(latent_df, target_prop, censor_rate_param, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  n <- nrow(latent_df)
  C <- rexp(n, rate = censor_rate_param)
  T <- pmin(latent_df$T1, latent_df$T2)
  cause <- ifelse(latent_df$T1 < latent_df$T2, 1L, 2L)
  ties <- which(latent_df$T1 == latent_df$T2)
  if (length(ties) > 0) cause[ties] <- sample(c(1L,2L), length(ties), replace = TRUE)
  obsT <- pmin(T, C)
  status <- ifelse(C < T, 0L, cause)

  df <- data.frame(time = obsT, cause = status, trueT = T, trueCause = cause, censor_time = C)

  target_n_cens <- round(n * target_prop)
  cur_n_cens <- sum(df$cause == 0L)
  eps <- 1e-6
  if (cur_n_cens < target_n_cens) {
    need <- target_n_cens - cur_n_cens
    uncens_idx <- which(df$cause != 0L)
    pick <- sample(uncens_idx, size = need)
    newC <- runif(length(pick), 0, pmax(df$trueT[pick] - eps, eps))
    df$censor_time[pick] <- newC
    df$time[pick] <- newC
    df$cause[pick] <- 0L
  } else if (cur_n_cens > target_n_cens) {
    need <- cur_n_cens - target_n_cens
    cens_idx <- which(df$cause == 0L)
    pick <- sample(cens_idx, size = need)
    df$censor_time[pick] <- Inf
    df$time[pick] <- df$trueT[pick]
    df$cause[pick] <- df$trueCause[pick]
  }

  stopifnot(sum(df$cause == 0L) == target_n_cens)
  df[c("time", "cause")]
}

# --- Generate all datasets for one scenario ---
generate_scenario <- function(scenario_name, n, n_rep, censor_target, params1, params2_base, seed_base = 100) {
  cal <- calibrate_balance(params1 = params1, params2_base = params2_base, seed = seed_base)
  params2 <- cal$params2

  # find censor rate parameter roughly matching target
  censor_rate_param <- 0.1

  datasets <- vector("list", n_rep)
  for (rep in 1:n_rep) {
    latent <- simulate_latent(n, params1, params2, seed = seed_base + rep)
    obs <- apply_censoring(latent, target_prop = censor_target, censor_rate_param = censor_rate_param, seed = seed_base + rep + 1000)
    obs$ID <- 1:n
    obs$dataset_id <- rep
    obs$scenario <- scenario_name
    datasets[[rep]] <- obs
  }
  datasets
}

# --- Define scenarios ---
scenarios_sim2 <- data.frame(
  gamma1 = c(-0.10, +0.10, -0.10, +0.10, -0.10, +0.10),
  gamma2 = c(-0.06, +0.06, -0.06, +0.06, -0.06, +0.06),
  lambda1 = c(0.012, 0.012, 0.012, 0.012, 0.012, 0.012),
  lambda2 = c(0.010, 0.010, 0.010, 0.010, 0.010, 0.010),
  censor_rate = c(0.30, 0.30, 0.50, 0.50, 0.70, 0.70),
  stringsAsFactors = FALSE
)

# --- Run all scenarios ---
n <- 200
n_rep <- 1000
all_data <- list()

for (i in seq_len(nrow(scenarios_sim2))) {
  sc <- scenarios_sim2[i, ]
  name <- sprintf("gamma=(%.2f,%.2f)_c=%.2f", sc$gamma1, sc$gamma2, sc$censor_rate)
  params1 <- c(alpha = sc$gamma1, beta = sc$lambda1)
  params2 <- c(alpha = sc$gamma2, beta = sc$lambda2)
  cat("Running", name, "\n")
  all_data[[name]] <- generate_scenario(name, n, n_rep, sc$censor_rate, params1, params2, seed_base = 1000 * i)
}

# --- Save all datasets ---
saveRDS(all_data, file = "sim_data2.rds")
saveRDS(scenarios_sim2, file = "scenario_specs.rds")
cat("Saved sim_data_all.rds and scenario_specs.rds\n")
```

## Implement Models

``` r
# Required: install.packages("survival")
library(survival)

# Estimate Ĝ(t): the probability of being uncensored at time t
make_Ghat <- function(time, event, eps = 1e-6) {
  stopifnot(length(time) == length(event))
  censored <- as.integer(event == 0)  # 1 for censored, 0 otherwise
  fit <- survfit(Surv(time, censored) ~ 1)
  
  # Create a right-continuous step function for Ĝ(t)
  G_times <- c(0, fit$time)
  G_surv  <- c(1, fit$surv)
  G_step  <- stepfun(G_times, c(G_surv, tail(G_surv, 1)), right = TRUE)
  
  # Return function to evaluate Ĝ(t)
  function(u) {
    pmax(G_step(pmax(u, 0)), eps)  # avoid divide-by-zero
  }
}

# Time-dependent Brier Score for cause k at time t using IPCW (Brier Score)
brier_competing <- function(time, event, p_pred, t_eval, cause_k, Ghat = NULL) {
  n <- length(time)
  stopifnot(length(event) == n, length(p_pred) == n)
  if (is.null(Ghat)) Ghat <- make_Ghat(time, event)
  
  G_t <- Ghat(t_eval)
  Yt <- as.integer(time <= t_eval & event == cause_k)  # observed label
  
  # IPCW weights
  w1 <- as.numeric(time <= t_eval & event != 0) / Ghat(time)  # for events
  w2 <- as.numeric(time > t_eval) / G_t                       # for censored
  
  error1 <- w1 * (Yt - p_pred)^2  # error for observed events
  error2 <- w2 * (0 - p_pred)^2  # error for censored
  
  score <- (error1 + error2) |> mean()
  return(score)
}
```

``` r
(scenariosUpdate <- scenarios_sim1 |> mutate(ID = senarioNames))
(Senario <- senarioNames[1])
TimeGenSen1 <- vapply(sim_data_all[[senarioNames[1]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TimeGenSen2 <- vapply(sim_data_all[[senarioNames[2]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TimeGenSen3 <- vapply(sim_data_all[[senarioNames[3]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TimeGenSen4 <- vapply(sim_data_all[[senarioNames[4]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TimeGenSen5 <- vapply(sim_data_all[[senarioNames[5]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TimeGenSen6 <- vapply(sim_data_all[[senarioNames[6]]], 
    function(x) x$time, numeric(200)) |> as.vector()

TimeEval1 <- quantile(x = TimeGenSen1, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEval2 <- quantile(x = TimeGenSen2, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEval3 <- quantile(x = TimeGenSen3, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEval4 <- quantile(x = TimeGenSen4, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEval5 <- quantile(x = TimeGenSen5, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEval6 <- quantile(x = TimeGenSen6, 
  probs = seq(0.05, 0.95, by = 0.05))
```

``` r
TImeGenSim2_Sen1 <- vapply(all_data[[senarioNames2[1]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TImeGenSim2_Sen2 <- vapply(all_data[[senarioNames2[2]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TImeGenSim2_Sen3 <- vapply(all_data[[senarioNames2[3]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TImeGenSim2_Sen4 <- vapply(all_data[[senarioNames2[4]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TImeGenSim2_Sen5 <- vapply(all_data[[senarioNames2[5]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TImeGenSim2_Sen6 <- vapply(all_data[[senarioNames2[6]]], 
    function(x) x$time, numeric(200)) |> as.vector()

TimeEvalSim2_1 <- quantile(x = TImeGenSim2_Sen1, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEvalSim2_2 <- quantile(x = TImeGenSim2_Sen2, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEvalSim2_3 <- quantile(x = TImeGenSim2_Sen3, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEvalSim2_4 <- quantile(x = TImeGenSim2_Sen4, probs = 
  seq(0.05, 0.95, by = 0.05))
TimeEvalSim2_5 <- quantile(x = TImeGenSim2_Sen5, 
  probs = seq(0.05, 0.95, by = 0.05))
TimeEvalSim2_6 <- quantile(x = TImeGenSim2_Sen6, 
  probs = seq(0.05, 0.95, by = 0.05))
```

``` r
library(progress)
senarioNames1 <- sim_data_all |> names()
senarioNames2 <- all_data |> names()
scenariosUpdate2 <- scenarios_sim2 |> mutate(ID = senarioNames2)
TimeEvalSim1 <- list(
  TimeEval1, TimeEval2, TimeEval3, 
  TimeEval4, TimeEval5, TimeEval6
)
TimeEvalSim2 <- list(
  TimeEvalSim2_1, TimeEvalSim2_2, 
  TimeEvalSim2_3, TimeEvalSim2_4, 
  TimeEvalSim2_5, TimeEvalSim2_6
)
```



``` r
GetResult <- function(Senario, TimeEval = TimeEval1, IndexSim = 1) {
  if(IndexSim == 2) {
    DataList <- sim_data_all[[Senario]]
    Par <- scenariosUpdate |> subset(subset = ID == Senario) |> 
    mutate(ID = NULL)
  } else {
    DataList <- all_data[[Senario]]
    Par <- scenariosUpdate2 |> subset(subset = ID == Senario) |>
      mutate(ID = NULL)
  }
    parNames <- names(Par)
    ftemp <- function(Dat) {
        n <- nrow(Dat)
        d1 <- 1 * (Dat$cause == 1)
        d2 <- 1 * (Dat$cause == 2)
        FeatTemp <- matrix(rnorm(nrow(Dat)), nrow = nrow(Dat), ncol = 1)
        Time <- Dat$time 
        Initialize(features = FeatTemp, x = Time, 
            delta1 = d1, delta2 = d2, h = 1e-9)
        initial_params <- rep(0.025, 4)
        restemp <- CIF_res1(initial_params)
        result <- restemp$Estimation
        Directestcoef1 <- result[1:2]
        Directestcoef2 <- result[3:4]
        Directpredcif1 <- lapply(Time, cdf_gomp, 
            alpha = Directestcoef1[1], beta = Directestcoef1[2]) |> unlist()
        Directpredcif2 <- lapply(Time, cdf_gomp, 
            alpha = Directestcoef2[1], beta = Directestcoef2[2]) |> unlist()
        DirectpredcifEval1 <- lapply(TimeEval, cdf_gomp, 
            alpha = Directestcoef1[1], beta = Directestcoef1[2]) |> unlist()
        DirectpredcifEval2 <- lapply(TimeEval, cdf_gomp, 
            alpha = Directestcoef2[1], beta = Directestcoef2[2]) |> unlist()
        finefit <- cmprsk::cuminc(ftime = Dat$time, fstatus = Dat$cause)
        finepredcif <- cmprsk::timepoints(finefit, times = Dat$time)$est
        finecifpredEval <- cmprsk::timepoints(finefit, times = TimeEval1)$est
        finecifpred1 <- finepredcif[1, ]
        finecifpred2 <- finepredcif[2, ]
        finecifpredEval1 <- finecifpredEval[1, ]
        finecifpredEval2 <- finecifpredEval[2, ]
        (DirectBrierAve1 <- lapply(Time, function(x) brier_competing(time = Time, 
          event = Dat$cause, 
          p_pred = Directpredcif1, 
          t_eval = x, cause_k = 1)) |> unlist() |> mean())
        (DirectBrierEval1 <- lapply(TimeEval, function(x) brier_competing(time = Time, 
          event = Dat$cause, 
          p_pred = Directpredcif1, 
          t_eval = x, cause_k = 1)) |> unlist())
        
        (DirectBrierEval2 <- lapply(TimeEval, function(x) brier_competing(time = Time, 
          event = Dat$cause, 
          p_pred = Directpredcif2, 
          t_eval = x, cause_k = 2)) |> unlist())

        (DirectBrierAve2 <- lapply(Time, function(x) brier_competing(time = Time, 
          event = Dat$cause, 
          p_pred = Directpredcif2, 
          t_eval = x, cause_k = 2)) |> unlist() |> mean())

        (fineBrierAve1 <- lapply(Time, function(x) brier_competing(time = Time, 
          event = Dat$cause, 
          p_pred = finecifpred1, 
          t_eval = x, cause_k = 1)) |> unlist() |> mean())
        (fineBrierAve2 <- lapply(Time, function(x) brier_competing(time = Time, 
          event = Dat$cause, 
          p_pred = finecifpred2, 
          t_eval = x, cause_k = 2)) |> unlist() |> mean())
        
        
        (fineBrierEval1 <- lapply(TimeEval, function(x) brier_competing(time = Time, 
          event = Dat$cause, 
          p_pred = finecifpred1, 
          t_eval = x, cause_k = 1)) |> unlist())
        (fineBrierEval2 <- lapply(TimeEval, function(x) brier_competing(time = Time, 
          event = Dat$cause, 
          p_pred = finecifpred2, 
          t_eval = x, cause_k = 2)) |> unlist())

        (DirectAUCAve1 <- timeROC::timeROC(T = Time,
               delta = Dat$cause,
               marker = Directpredcif1,
               cause = 1,
               times = Time) |> _$AUC_2 |> mean(na.rm = TRUE))
        (DirectAUCAve2 <- timeROC::timeROC(T = Time,
               delta = Dat$cause,
               marker = Directpredcif2,
               cause = 2,
               times = Time) |> _$AUC_2 |> mean(na.rm = TRUE))

        (DirectAUCEval1 <- timeROC::timeROC(T = Time,
               delta = Dat$cause,
               marker = Directpredcif1,
               cause = 1,
               times = TimeEval) |> _$AUC_2)
        (DirectAUCEval2 <- timeROC::timeROC(T = Time,
               delta = Dat$cause,
               marker = Directpredcif2,
               cause = 2,
               times = TimeEval) |> _$AUC_2)

        (fineAUCAve1 <- timeROC::timeROC(T = Time,
               delta = Dat$cause,
               marker = finecifpred1,
               cause = 1,
               times = Time) |> _$AUC_2 |> mean(na.rm = TRUE))
        (fineAUCAve2 <- timeROC::timeROC(T = Time,
               delta = Dat$cause,
               marker = finecifpred2,
               cause = 2,
               times = Time) |> _$AUC_2 |> mean(na.rm = TRUE))

        (fineAUCEval1 <- timeROC::timeROC(T = Time,
               delta = Dat$cause,
               marker = finecifpred1,
               cause = 1,
               times = TimeEval) |> _$AUC_2)
        (fineAUCEval2 <- timeROC::timeROC(T = Time,
               delta = Dat$cause,
               marker = finecifpred2,
               cause = 2,
               times = TimeEval) |> _$AUC_2)
      
        Realcoef1 <- Par[parNames %in% c('gamma1', 'lambda1')] |> 
          unlist()
  
        Realcoef2 <- Par[parNames %in% c('gamma2', 'lambda2')] |> 
          unlist()
  
        Realcif1 <- lapply(Time, function(y) cdf_gomp(y, 
          alpha = Realcoef1[1], beta = Realcoef1[2])) |> unlist()
        Realcif2 <- lapply(Time, function(y) cdf_gomp(y, 
          alpha = Realcoef2[1], beta = Realcoef2[2])) |> unlist()
        
        
        RealcifEval1 <- lapply(TimeEval, function(y) cdf_gomp(y, 
          alpha = Realcoef1[1], beta = Realcoef1[2])) |> unlist()
        RealcifEval2 <- lapply(TimeEval, function(y) cdf_gomp(y, 
          alpha = Realcoef2[1], beta = Realcoef2[2])) |> unlist()

      DirectMSE1 <- ((Directpredcif1 - Realcif1)^2) |> mean()
      DirectMSE2 <- ((Directpredcif2 - Realcif2)^2) |> mean()
      
      fineMSE1 <- ((finecifpred1 - Realcif1)^2) |> mean()
      fineMSE2 <- ((finecifpred2 - Realcif2)^2) |> mean()

      DirectBIAS1 <- abs(Realcif1 - Directpredcif1)
      DirectBIAS2 <- abs(Realcif2 - Directpredcif2)
      
      DirectBIASEval1 <- abs(RealcifEval1 - DirectpredcifEval1)
      DirectBIASEval2 <- abs(RealcifEval2 - DirectpredcifEval2)

      fineBIAS1 <- abs(Realcif1 - finecifpred1)
      fineBIAS2 <- abs(Realcif2 - finecifpred2)
      
      fineBIASEval1 <- abs(RealcifEval1 - finecifpredEval1)
      fineBIASEval2 <- abs(RealcifEval2 - finecifpredEval2)
      FResult <- list(
        FineGrayResult = list(
          Risk1PredEvalTime = finecifpredEval1,
          Risk2PredEvalTime = finecifpredEval2,
          Risk1MSE = fineMSE1, 
          Risk2MSE = fineMSE2, 
          Risk1BrierTotalTime = fineBrierAve1,
          Risk2BrierTotalTime = fineBrierAve2,
          Risk1BrierEvalTime = fineBrierEval1, 
          Risk2BrierEvalTime = fineBrierEval2, 
          Risk1AUCTotalTime = fineAUCAve1, 
          Risk2AUCTotalTime = fineAUCAve2, 
          Risk1AUCEvalTime = fineAUCEval1, 
          Risk2AUCEvalTime = fineAUCEval2, 
          Risk1BiasEvalTime = fineBIASEval1, 
          Risk2BiasEvalTime = fineBIASEval2
        ),
        DirectResult = list(
          Risk1PredEvalTime = DirectpredcifEval1,
          Risk2PredEvalTime = DirectpredcifEval2,
          Risk1MSE = DirectMSE1, 
          Risk2MSE = DirectMSE2, 
          Risk1BrierTotalTime = DirectBrierAve1,
          Risk2BrierTotalTime = DirectBrierAve2,
          Risk1BrierEvalTime = DirectBrierEval1, 
          Risk2BrierEvalTime = DirectBrierEval2, 
          Risk1AUCTotalTime = DirectAUCAve1, 
          Risk2AUCTotalTime = DirectAUCAve2, 
          Risk1AUCEvalTime = DirectAUCEval1, 
          Risk2AUCEvalTime = DirectAUCEval2, 
          Risk1BiasEvalTime = DirectBIASEval1, 
          Risk2BiasEvalTime = DirectBIASEval2, 
          Risk1Coef = Directestcoef1,
          Risk2Coef = Directestcoef2
      )
    )
  Cleanup()
  return (FResult)
  }

    # Run ftemp for all datasets
  total_datasets <- length(DataList)
  pb <- progress_bar$new(
    format = "  Processing [:bar] :percent in :elapsed ETA: :eta",
    total = total_datasets, clear = FALSE, width = 60
  )

  all_results <- lapply(DataList, function(dataset) {
    pb$tick()
    ftemp(dataset)
  })

  # Aggregate results across datasets
  level1_names <- names(all_results[[1]])
  final_output <- list()
  
  for (lvl1 in level1_names) {
    sub_names <- names(all_results[[1]][[lvl1]])
    final_output[[lvl1]] <- list()
    for (sub in sub_names) {
      all_items <- lapply(all_results, function(x) x[[lvl1]][[sub]])
      if (is.numeric(all_items[[1]]) && length(all_items[[1]]) == 1) {
        final_output[[lvl1]][[sub]] <- unlist(all_items)
      } else {
        final_output[[lvl1]][[sub]] <- do.call(rbind, all_items)
      }
    }
  }
  return(final_output)
}
```



## Get Simulation 1 Result

``` r
SimulationResult_1 <- vector('list', 6) |> 
  setNames(senarioNames1)
SimulationResult_2 <- vector('list', 6) |> 
  setNames(senarioNames2)
all_data <- readRDS("sim_data2.rds")
sim_data_all <- readRDS("simData.rds")
```

``` r
startt <- Sys.time()
for (Index in 1:2) {
  if (Index == 1) {
    for(i in 1:length(senarioNames1)) {
      Scenario <- senarioNames1[i]
      TimeEval <- TimeEvalSim1[[i]]
      TempResult <- GetResult(Senario = Scenario, 
        TimeEval = TimeEval, IndexSim = Index)
      SimulationResult_1[[Scenario]] <- TempResult
  }
} else {
  for(i in 1:length(senarioNames2)) {
      Scenario <- senarioNames2[i]
      TimeEval <- TimeEvalSim2[[i]]
      TempResult2 <- GetResult(Senario = Scenario, 
        TimeEval = TimeEval, IndexSim = Index)
      SimulationResult_2[[Scenario]] <- TempResult2
  }
}
} 
endd <- Sys.time()
(Elapsed <- difftime(endd, startt, unit = 'secs'))/60
# saveRDS(SimulationResult_1, "TotalResult_Simulation_1_Editted.rds")
```

``` r
for(i in seq_along(senarioNames2)) {
      Scenario <- senarioNames2[i]
      TimeEval <- TimeEvalSim2[[i]]
      TempResult2 <- GetResult(Senario = Scenario, 
        TimeEval = TimeEval, IndexSim = 2)
      SimulationResult_2[[Scenario]] <- TempResult2 
}

# saveRDS(SimulationResult_2, "TotalResult_Simulation_2_Editted.rds")
```



``` r
all_data <- readRDS("sim_data2.rds")
sim_data_all <- readRDS("simData.rds")
resSim1 <- readRDS('TotalResult_Simulation_1.rds')
resSim2 <- readRDS("TotalResult_Simulation_2.rds")
Scenarios <- readRDS("scenario_specs.rds")
senarioNames1 <- sim_data_all |> names()
senarioNames2 <- all_data |> names()
```



``` r
TimeGenSen1 <- vapply(sim_data_all[[senarioNames1[1]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TimeGenSen2 <- vapply(sim_data_all[[senarioNames1[2]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TimeGenSen3 <- vapply(sim_data_all[[senarioNames1[3]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TimeGenSen4 <- vapply(sim_data_all[[senarioNames1[4]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TimeGenSen5 <- vapply(sim_data_all[[senarioNames1[5]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TimeGenSen6 <- vapply(sim_data_all[[senarioNames1[6]]], 
    function(x) x$time, numeric(200)) |> as.vector()

TimeEval1 <- quantile(x = TimeGenSen1, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEval2 <- quantile(x = TimeGenSen2, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEval3 <- quantile(x = TimeGenSen3, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEval4 <- quantile(x = TimeGenSen4, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEval5 <- quantile(x = TimeGenSen5, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEval6 <- quantile(x = TimeGenSen6, 
  probs = seq(0.05, 0.95, by = 0.05))



TImeGenSim2_Sen1 <- vapply(all_data[[senarioNames2[1]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TImeGenSim2_Sen2 <- vapply(all_data[[senarioNames2[2]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TImeGenSim2_Sen3 <- vapply(all_data[[senarioNames2[3]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TImeGenSim2_Sen4 <- vapply(all_data[[senarioNames2[4]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TImeGenSim2_Sen5 <- vapply(all_data[[senarioNames2[5]]], 
    function(x) x$time, numeric(200)) |> as.vector()
TImeGenSim2_Sen6 <- vapply(all_data[[senarioNames2[6]]], 
    function(x) x$time, numeric(200)) |> as.vector()

TimeEvalSim2_1 <- quantile(x = TImeGenSim2_Sen1, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEvalSim2_2 <- quantile(x = TImeGenSim2_Sen2, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEvalSim2_3 <- quantile(x = TImeGenSim2_Sen3, 
  probs = seq(0.05, 0.95, by = 0.05))
    
TimeEvalSim2_4 <- quantile(x = TImeGenSim2_Sen4, probs = 
  seq(0.05, 0.95, by = 0.05))
TimeEvalSim2_5 <- quantile(x = TImeGenSim2_Sen5, 
  probs = seq(0.05, 0.95, by = 0.05))
TimeEvalSim2_6 <- quantile(x = TImeGenSim2_Sen6, 
  probs = seq(0.05, 0.95, by = 0.05))
```



# Simulation I \| Scenario I: \$():~ -0.1, (): ~ 0.012, : 0.3

``` r
FineResult <- resSim1[[1]]$FineGrayResult
DirectResult <- resSim1[[1]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 
DirectResult |> names()
nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval1 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation I \| Scenario I

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation I \| Scenario I

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation I \| Scenario I

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation I \| Scenario I

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation I \| Scenario I

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```



### Direct Coefficient Result Scenario I

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(-0.1, -0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario I

``` r
Scenario1_Sim1_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario1_Sim1_Result, './FResult/Fresult_Scenario1_Simulation1.rds')
```



# Simulation I \| Scenario II: \$():~ 0.1, (): ~ 0.012, : 0.3

``` r
FineResult <- resSim1[[2]]$FineGrayResult
DirectResult <- resSim1[[2]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 

nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval2 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation I \| Scenario II

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation I \| Scenario II

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation I \| Scenario II

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation I \| Scenario II

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation I \| Scenario II

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```





### Direct Coefficient Result Scenario II

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(0.1, 0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario II

``` r
Scenario2_Sim1_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario2_Sim1_Result, './FResult/Fresult_Scenario2_Simulation1.rds')
```



# Simulation I \| Scenario III: \$():~ -0.1, (): ~ 0.012, : 0.5

``` r
FineResult <- resSim1[[3]]$FineGrayResult
DirectResult <- resSim1[[3]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 

nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval2 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation I \| Scenario III

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation I \| Scenario III

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation I \| Scenario III

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation I \| Scenario III

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation I \| Scenario III

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```





### Direct Coefficient Result Scenario III

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(-0.1, -0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario III

``` r
Scenario3_Sim1_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario3_Sim1_Result, './FResult/Fresult_Scenario3_Simulation1.rds')
```



# Simulation I \| Scenario IV: \$():~ 0.1, (): ~ 0.012, : 0.5

``` r
FineResult <- resSim1[[4]]$FineGrayResult
DirectResult <- resSim1[[4]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 

nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval2 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation I \| Scenario IV

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation I \| Scenario IV

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation I \| Scenario IV

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation I \| Scenario IV

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation I \| Scenario IV

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```





### Direct Coefficient Result Scenario IV

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(0.1, 0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario IV

``` r
Scenario4_Sim1_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario4_Sim1_Result, './FResult/Fresult_Scenario4_Simulation1.rds')
```



# Simulation I \| : \$():~ -0.1, (): ~ 0.012, : 0.7

``` r
FineResult <- resSim1[[5]]$FineGrayResult
DirectResult <- resSim1[[5]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 

nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval2 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation I \| Scenario V

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation I \| Scenario V

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation I \| Scenario V

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation I \| Scenario V

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation I \| Scenario V

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```





### Direct Coefficient Result Scenario V

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(-0.1, -0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario V

``` r
Scenario5_Sim1_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario5_Sim1_Result, './FResult/Fresult_Scenario5_Simulation1.rds')
```



# Simulation I \| Scenario VI: \$():~ 0.1, (): ~ 0.012, : 0.7

``` r
FineResult <- resSim1[[6]]$FineGrayResult
DirectResult <- resSim1[[6]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 

nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval2 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation I \| Scenario VI

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation I | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation I \| Scenario VI

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation I \| Scenario VI

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation I \| Scenario VI

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation I \| Scenario VI

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```





### Direct Coefficient Result Scenario VI

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(0.1, 0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario VI

``` r
Scenario6_Sim1_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario6_Sim1_Result, './FResult/Fresult_Scenario6_Simulation1.rds')
```



# Simulation II \| Scenario I: \$():~ -0.1, (): ~ 0.012, : 0.3

``` r
FineResult <- resSim2[[1]]$FineGrayResult
DirectResult <- resSim2[[1]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 
DirectResult |> names()
nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval1 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation II \| Scenario I

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation II \| Scenario I

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation II \| Scenario I

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result \| Simulation II

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation II \| Scenario I

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation II \| Scenario I

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```



### Direct Coefficient Result Scenario I

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(-0.1, -0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario I

``` r
Scenario1_Sim2_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario1_Sim2_Result, './FResult/Fresult_Scenario1_Simulation2.rds')
```



# Simulation II \| Scenario II: \$():~ 0.1, (): ~ 0.012, : 0.3

``` r
FineResult <- resSim2[[2]]$FineGrayResult
DirectResult <- resSim2[[2]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 

nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval2 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation II \| Scenario II

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation II \| Scenario II

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation II \| Scenario II

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation II \| Scenario II

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation II \| Scenario II

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```





### Direct Coefficient Result Scenario II

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(0.1, 0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario II

``` r
Scenario2_Sim2_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario2_Sim2_Result, './FResult/Fresult_Scenario2_Simulation2.rds')
```



# Simulation II \| Scenario III: \$():~ -0.1, (): ~ 0.012, : 0.5

``` r
FineResult <- resSim2[[3]]$FineGrayResult
DirectResult <- resSim2[[3]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 

nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval2 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation II \| Scenario III

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation II \| Scenario III

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation II \| Scenario III

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation II \| Scenario III

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation II \| Scenario III

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```





### Direct Coefficient Result Scenario III

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(-0.1, -0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario III

``` r
Scenario3_Sim2_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario3_Sim2_Result, './FResult/Fresult_Scenario3_Simulation2.rds')
```



# Simulation II \| Scenario IV: \$():~ 0.1, (): ~ 0.012, : 0.5

``` r
FineResult <- resSim2[[4]]$FineGrayResult
DirectResult <- resSim2[[4]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 

nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval2 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation II \| Scenario IV

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation II \| Scenario IV

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation II \| Scenario IV

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation II \| Scenario IV

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation II \| Scenario IV

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```





### Direct Coefficient Result Scenario IV

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(0.1, 0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario IV

``` r
Scenario4_Sim2_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario4_Sim2_Result, './FResult/Fresult_Scenario4_Simulation2.rds')
```



# Simulation II \| : \$():~ -0.1, (): ~ 0.012, : 0.7

``` r
FineResult <- resSim2[[5]]$FineGrayResult
DirectResult <- resSim2[[5]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 

nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval2 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation II \| Scenario V

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation II \| Scenario V

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation II \| Scenario V

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation II \| Scenario V

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation II \| Scenario V

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```





### Direct Coefficient Result Scenario V

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(-0.1, -0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario V

``` r
Scenario5_Sim2_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario5_Sim2_Result, './FResult/Fresult_Scenario5_Simulation2.rds')
```



# Simulation II \| Scenario VI: \$():~ 0.1, (): ~ 0.012, : 0.7

``` r
FineResult <- resSim2[[6]]$FineGrayResult
DirectResult <- resSim2[[6]]$DirectResult
finepredrisk1 <- FineResult$Risk1PredEvalTime
finepredrisk2 <- FineResult$Risk2PredEvalTime 
directpredrisk1 <-DirectResult$Risk1PredEvalTime 
directpredrisk2 <-DirectResult$Risk2PredEvalTime 

nk <- 1e+3
finepredrisk1 |> dim()
fineSDrisk1 <- apply(finepredrisk1, 2, sd) |> setNames(NULL)
fineSDrisk2 <- apply(finepredrisk2, 2, sd) |> setNames(NULL)

directSDrisk1 <- apply(directpredrisk1, 2, sd) |> setNames(NULL)
directSDrisk2 <- apply(directpredrisk2, 2, sd) |> setNames(NULL)

fineMEANrisk1 <- apply(finepredrisk1, 2, mean) |> setNames(NULL)
fineMEANrisk2 <- apply(finepredrisk2, 2, mean) |> setNames(NULL)

directMEANrisk1 <- apply(directpredrisk1, 2, mean) |> setNames(NULL)
directMEANrisk2 <- apply(directpredrisk2, 2, mean) |> setNames(NULL)

nks <- sqrt(nk)
Z <- qnorm(0.975)
fineCILBONDrisk1 <- fineMEANrisk1 - fineSDrisk1/nks * Z
fineCIUBONDrisk1 <- fineMEANrisk1 + fineSDrisk1/nks * Z
fineCILBONDrisk2 <- fineMEANrisk2 - fineSDrisk2/nks * Z
fineCIUBONDrisk2 <- fineMEANrisk2 + fineSDrisk2/nks * Z

directCILBONDrisk1 <- directMEANrisk1 - directSDrisk1/nks * Z
directCIUBONDrisk1 <- directMEANrisk1 + directSDrisk1/nks * Z
directCILBONDrisk2 <- directMEANrisk2 - directSDrisk2/nks * Z
directCIUBONDrisk2 <- directMEANrisk2 + directSDrisk2/nks * Z

Time <- TimeEval2 |> setNames(NULL)
```



## CIF Prediction in Eval Time Method Simulation II \| Scenario VI

### Risk I

``` r
cifRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk1, 
  Direct_AveEst = directMEANrisk1, 
  FineGray_CI_Lbond = fineCILBONDrisk1, 
  FineGray_CI_UBond = fineCIUBONDrisk1, 
  Direct_CI_Lbond = directCILBONDrisk1, 
  Direct_CI_Ubond = directCIUBONDrisk1
)
cifRisk1 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk I: CIF Prediction for Eval Time")
```



### Risk II

``` r
cifRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray_AveEst = fineMEANrisk2, 
  Direct_AveEst = directMEANrisk2, 
  FineGray_CI_Lbond = fineCILBONDrisk2, 
  FineGray_CI_UBond = fineCIUBONDrisk2, 
  Direct_CI_Lbond = directCILBONDrisk2, 
  Direct_CI_Ubond = directCIUBONDrisk2
)
cifRisk2 |> knitr::kable(align = "c", 
  caption = "Method Simulation II | Scenario I | Risk II: CIF Prediction for Eval Time")
```



## MSE \| Method Simulation II \| Scenario VI

``` r
FineResult |> names()
fineMSErisk1 <- FineResult$Risk1MSE |> mean()
directMSErisk1 <- DirectResult$Risk1MSE |> mean()

fineMSErisk2 <- FineResult$Risk2MSE |> mean()
directMSErisk2 <- DirectResult$Risk2MSE |> mean()

fineMSESDrisk1 <- FineResult$Risk1MSE |> sd()
directMSESDrisk1 <- DirectResult$Risk1MSE |> sd()

fineMSESDrisk2 <- FineResult$Risk2MSE |> sd()
directMSESDrisk2 <- DirectResult$Risk2MSE |> sd()


fineMSELBONDrisk1 <- fineMSErisk1 - Z * fineMSESDrisk1/nks 
fineMSEUBONDrisk1 <- fineMSErisk1 + Z * fineMSESDrisk1/nks 
fineMSELBONDrisk2 <- fineMSErisk2 - Z * fineMSESDrisk2/nks 
fineMSEUBONDrisk2 <- fineMSErisk2 + Z * fineMSESDrisk2/nks 

directMSELBONDrisk1 <- directMSErisk1 - Z * directMSESDrisk1/nks
directMSEUBONDrisk1 <- directMSErisk1 + Z * directMSESDrisk1/nks
directMSELBONDrisk2 <- directMSErisk2 - Z * directMSESDrisk2/nks
directMSEUBONDrisk2 <- directMSErisk2 + Z * directMSESDrisk2/nks
```

``` r
TotalMSE <- data.frame(Risk = c('Risk1', 'Risk2'), 
  FineGray = c(fineMSErisk1, fineMSErisk2), 
  Direct = c(directMSErisk1, directMSErisk2), 
  FineGray_LBOND = c(fineMSELBONDrisk1, fineMSELBONDrisk2), 
  FineGray_UBOND = c(fineMSEUBONDrisk1, fineMSEUBONDrisk2), 
  Direct_LBOND = c(directMSELBONDrisk1, directMSELBONDrisk2), 
  Direct_UBOND = c(directMSEUBONDrisk1, directMSEUBONDrisk2))

TotalMSE |> 
knitr::kable(align = 'c', caption = "MSE Results")
```



## Brier Score \| Method Simulation II \| Scenario VI

``` r
fineBRIERTOTALrisk1 <- FineResult$Risk1BrierTotalTime |> mean()
fineBRIERTOTALrisk2 <- FineResult$Risk2BrierTotalTime |> mean()


directBRIERTOTALrisk1 <- DirectResult$Risk1BrierTotalTime |> mean()
directBRIERTOTALrisk2 <- DirectResult$Risk2BrierTotalTime |> mean()

fineBRIERTOTALSDrisk1 <- FineResult$Risk1BrierTotalTime |> sd()
fineBRIERTOTALSDrisk2 <- FineResult$Risk2BrierTotalTime |> sd()

directBRIERTOTALSDrisk1 <- DirectResult$Risk1BrierTotalTime |> sd()
directBRIERTOTALSDrisk2 <- DirectResult$Risk2BrierTotalTime |> sd()

fineBRIERTOTALLBONDrisk1 = fineBRIERTOTALrisk1 - fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALUBONDrisk1 = fineBRIERTOTALrisk1 + fineBRIERTOTALSDrisk1 * Z/nks
fineBRIERTOTALLBONDrisk2 = fineBRIERTOTALrisk2 - fineBRIERTOTALSDrisk2 * Z/nks
fineBRIERTOTALUBONDrisk2 = fineBRIERTOTALrisk2 + fineBRIERTOTALSDrisk2 * Z/nks

directBRIERTOTALLBONDrisk1 <- directBRIERTOTALrisk1 - directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALUBONDrisk1 <- directBRIERTOTALrisk1 + directBRIERTOTALSDrisk1 * Z/nks
directBRIERTOTALLBONDrisk2 <- directBRIERTOTALrisk2 - directBRIERTOTALSDrisk2 * Z/nks
directBRIERTOTALUBONDrisk2 <- directBRIERTOTALrisk2 + directBRIERTOTALSDrisk2 * Z/nks

BrierAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineBRIERTOTALrisk1, fineBRIERTOTALrisk2), 
  Direct = c(directBRIERTOTALrisk1, directBRIERTOTALrisk2), 
  FineGray_LBOND = c(fineBRIERTOTALLBONDrisk1, fineBRIERTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineBRIERTOTALUBONDrisk1, fineBRIERTOTALUBONDrisk2), 
  Direct_LBOND = c(directBRIERTOTALLBONDrisk1, directBRIERTOTALLBONDrisk2),
  Direct_UBOND = c(directBRIERTOTALUBONDrisk1, directBRIERTOTALUBONDrisk2)
)



fineBRIEREVALrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBRIEREVALrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBRIEREVALrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBRIEREVALrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBRIEREVALSDrisk1 <- FineResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBRIEREVALSDrisk2 <- FineResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBRIEREVALSDrisk1 <- DirectResult$Risk1BrierEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBRIEREVALSDrisk2 <- DirectResult$Risk2BrierEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBRIEREVALLBONDrisk1 = fineBRIEREVALrisk1 - fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALUBONDrisk1 = fineBRIEREVALrisk1 + fineBRIEREVALSDrisk1 * Z/nks
fineBRIEREVALLBONDrisk2 = fineBRIEREVALrisk2 - fineBRIEREVALSDrisk2 * Z/nks
fineBRIEREVALUBONDrisk2 = fineBRIEREVALrisk2 + fineBRIEREVALSDrisk2 * Z/nks

directBRIEREVALLBONDrisk1 <- directBRIEREVALrisk1 - directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALUBONDrisk1 <- directBRIEREVALrisk1 + directBRIEREVALSDrisk1 * Z/nks
directBRIEREVALLBONDrisk2 <- directBRIEREVALrisk2 - directBRIEREVALSDrisk2 * Z/nks
directBRIEREVALUBONDrisk2 <- directBRIEREVALrisk2 + directBRIEREVALSDrisk2 * Z/nks


BrierEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk1, 
  Direct = directBRIEREVALrisk1, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk1, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk1, 
  Direct_LBOND = directBRIEREVALLBONDrisk1, 
  Direct_UBOND = directBRIEREVALUBONDrisk1
)

BrierEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBRIEREVALrisk2, 
  Direct = directBRIEREVALrisk2, 
  FineGray_LBOND = fineBRIEREVALLBONDrisk2, 
  FineGray_UBOND = fineBRIEREVALLBONDrisk2, 
  Direct_LBOND = directBRIEREVALLBONDrisk2, 
  Direct_UBOND = directBRIEREVALUBONDrisk2
)
```

### Brier Average Result

``` r
BrierAveResult |> 
knitr::kable(
  align = 'c', caption = "Average of Brier Score Result"
)
```

### Risk I

``` r
BrierEVALRisk1 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



### Risk II

``` r
BrierEVALRisk2 |> 
  knitr::kable(
    align = 'c', 
    caption = "Brier Score for Risk I at Eval Time"
  )
```



## AUC Score \| Method Simulation II \| Scenario VI

``` r
fineAUCTOTALrisk1 <- FineResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
fineAUCTOTALrisk2 <- FineResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)


directAUCTOTALrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  mean(na.rm = TRUE)
directAUCTOTALrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  mean(na.rm = TRUE)

fineAUCTOTALSDrisk1 <- FineResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
fineAUCTOTALSDrisk2 <- FineResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)


directAUCTOTALSDrisk1 <- DirectResult$Risk1AUCTotalTime |> 
  sd(na.rm = TRUE)
directAUCTOTALSDrisk2 <- DirectResult$Risk2AUCTotalTime |> 
  sd(na.rm = TRUE)

fineAUCTOTALLBONDrisk1 <- fineAUCTOTALrisk1 - fineAUCTOTALSDrisk1 * Z/nks
fineAUCTOTALUBONDrisk1 <- fineAUCTOTALrisk1 + fineAUCTOTALSDrisk1 * Z/nks

directAUCTOTALLBONDrisk1 <- directAUCTOTALrisk1 - directAUCTOTALSDrisk1 * Z/nks
directAUCTOTALUBONDrisk1 <- directAUCTOTALrisk1 + directAUCTOTALSDrisk1 * Z/nks

fineAUCTOTALLBONDrisk2 <- fineAUCTOTALrisk2 - fineAUCTOTALSDrisk2 * Z/nks
fineAUCTOTALUBONDrisk2 <- fineAUCTOTALrisk2 + fineAUCTOTALSDrisk2 * Z/nks

directAUCTOTALLBONDrisk2 <- directAUCTOTALrisk2 - directAUCTOTALSDrisk2 * Z/nks
directAUCTOTALUBONDrisk2 <- directAUCTOTALrisk2 + directAUCTOTALSDrisk2 * Z/nks

AUCAveResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  FineGray = c(fineAUCTOTALrisk1, fineAUCTOTALrisk2), 
  Direct = c(directAUCTOTALrisk1, directAUCTOTALrisk2), 
  FineGray_LBOND = c(fineAUCTOTALLBONDrisk1, fineAUCTOTALLBONDrisk2), 
  FineGray_UBOND = c(fineAUCTOTALUBONDrisk1, fineAUCTOTALUBONDrisk2), 
  Direct_LBOND = c(directAUCTOTALLBONDrisk1, directAUCTOTALLBONDrisk2), 
  Direct_UBOND = c(directAUCTOTALUBONDrisk1, directAUCTOTALUBONDrisk2) 
)

fineAUCEVALrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) mean(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALSDrisk1 <- FineResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineAUCEVALSDrisk2 <- FineResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directAUCEVALSDrisk1 <- DirectResult$Risk1AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directAUCEVALSDrisk2 <- DirectResult$Risk2AUCEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)


fineAUCEVALLBONDrisk1 = fineAUCEVALrisk1 - fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALUBONDrisk1 = fineAUCEVALrisk1 + fineAUCEVALSDrisk1 * Z/nks
fineAUCEVALLBONDrisk2 = fineAUCEVALrisk2 - fineAUCEVALSDrisk2 * Z/nks
fineAUCEVALUBONDrisk2 = fineAUCEVALrisk2 + fineAUCEVALSDrisk2 * Z/nks

directAUCEVALLBONDrisk1 <- directAUCEVALrisk1 - directAUCEVALSDrisk1 * Z/nks
directAUCEVALUBONDrisk1 <- directAUCEVALrisk1 + directAUCEVALSDrisk1 * Z/nks
directAUCEVALLBONDrisk2 <- directAUCEVALrisk2 - directAUCEVALSDrisk2 * Z/nks
directAUCEVALUBONDrisk2 <- directAUCEVALrisk2 + directAUCEVALSDrisk2 * Z/nks


AUCEvalResultrisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk1, 
  Direct = directAUCEVALrisk1, 
  FineGray_LBOND = fineAUCEVALLBONDrisk1, 
  FineGray_UBOND = fineAUCEVALLBONDrisk1, 
  Direct_LBOND = directAUCEVALLBONDrisk1, 
  Direct_UBOND = directAUCEVALUBONDrisk1
)

AUCEvalResultrisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineAUCEVALrisk2, 
  Direct = directAUCEVALrisk2, 
  FineGray_LBOND = fineAUCEVALLBONDrisk2, 
  FineGray_UBOND = fineAUCEVALLBONDrisk2, 
  Direct_LBOND = directAUCEVALLBONDrisk2, 
  Direct_UBOND = directAUCEVALUBONDrisk2
)
```



### Average Result

``` r
AUCAveResult |> 
knitr::kable(
  align = 'c', 
  caption = "AUC Average Result"
)
```



### Risk I

``` r
AUCEvalResultrisk1 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk I"
)
```



### Risk II

``` r
AUCEvalResultrisk2 |> 
knitr::kable(
  align = 'c', 
  caption = "AUC of Eval Time Result For Risk II"
)
```



## BIAS \| Method Simulation II \| Scenario VI

``` r
fineBiasEVALrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
fineBiasEVALrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

directBiasEVALrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)
directBiasEVALrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , FUN = mean) |> setNames(NULL)

fineBiasEVALSDrisk1 <- FineResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
  FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
fineBiasEVALSDrisk2 <- FineResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

directBiasEVALSDrisk1 <- DirectResult$Risk1BiasEvalTime |> 
  apply(X = _, MARGIN = 2 ,
     FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)
directBiasEVALSDrisk2 <- DirectResult$Risk2BiasEvalTime |> 
  apply(X = _, MARGIN = 2 , 
    FUN = function(x) sd(x, na.rm = TRUE)) |> setNames(NULL)

fineBiasEVALLBONDrisk1 = fineBiasEVALrisk1 - fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALUBONDrisk1 = fineBiasEVALrisk1 + fineBiasEVALSDrisk1 * Z/nks
fineBiasEVALLBONDrisk2 = fineBiasEVALrisk2 - fineBiasEVALSDrisk2 * Z/nks
fineBiasEVALUBONDrisk2 = fineBiasEVALrisk2 + fineBiasEVALSDrisk2 * Z/nks

directBiasEVALLBONDrisk1 <- directBiasEVALrisk1 - directBiasEVALSDrisk1 * Z/nks
directBiasEVALUBONDrisk1 <- directBiasEVALrisk1 + directBiasEVALSDrisk1 * Z/nks
directBiasEVALLBONDrisk2 <- directBiasEVALrisk2 - directBiasEVALSDrisk2 * Z/nks
directBiasEVALUBONDrisk2 <- directBiasEVALrisk2 + directBiasEVALSDrisk2 * Z/nks


BiasEVALRisk1 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk1, 
  Direct = directBiasEVALrisk1, 
  FineGray_LBOND = fineBiasEVALLBONDrisk1, 
  FineGray_UBOND = fineBiasEVALLBONDrisk1, 
  Direct_LBOND = directBiasEVALLBONDrisk1, 
  Direct_UBOND = directBiasEVALUBONDrisk1
)

BiasEVALRisk2 <- data.frame(
  Quantile = seq(0.05, 0.95, by = 0.05), 
  Time = Time, 
  FineGray = fineBiasEVALrisk2, 
  Direct = directBiasEVALrisk2, 
  FineGray_LBOND = fineBiasEVALLBONDrisk2, 
  FineGray_UBOND = fineBiasEVALLBONDrisk2, 
  Direct_LBOND = directBiasEVALLBONDrisk2, 
  Direct_UBOND = directBiasEVALUBONDrisk2
)
```

### Risk I

``` r
BiasEVALRisk1 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk I"
)
```



### Risk II

``` r
BiasEVALRisk2 |> knitr::kable(
  align = 'c', 
  caption = "Bias of Eval Time For Risk II"
)
```





### Direct Coefficient Result Scenario VI

``` r
Scenarios
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefrisk1 <- DirectResult$Risk1Coef |> colMeans()
directCoefSDrisk1 <- apply(DirectResult$Risk1Coef, 
  MARGIN = 2, FUN = sd)

directCoefrisk2 <- DirectResult$Risk2Coef |> colMeans()
directCoefSDrisk2 <- apply(DirectResult$Risk2Coef, 
  MARGIN = 2, FUN = sd)

directCoefLBONDrisk1 = directCoefrisk1 - directCoefSDrisk1 * Z/nks 
directCoefUBONDrisk1 = directCoefrisk1 + directCoefSDrisk1 * Z/nks 
directCoefLBONDrisk2 = directCoefrisk2 - directCoefSDrisk2 * Z/nks 
directCoefUBONDrisk2 = directCoefrisk2 + directCoefSDrisk2 * Z/nks 

CoefResult <- data.frame(
  Risk = c("Risk1", "Risk2"), 
  gammaEST = c(directCoefrisk1[1], directCoefrisk2[1]), 
  lambdaEST = c(directCoefrisk1[2], directCoefrisk2[2]), 
  gammaReal = c(0.1, 0.06), 
  lambdaReal = c(0.012, 0.01),
  gammaLBOND = c(directCoefLBONDrisk1[1], directCoefLBONDrisk2[1]), 
  gammaUBOND = c(directCoefUBONDrisk1[1], directCoefUBONDrisk2[1]), 
  lambdaLBOND = c(directCoefLBONDrisk1[2], directCoefLBONDrisk2[2]), 
  lambdaUBOND = c(directCoefUBONDrisk1[2], directCoefUBONDrisk2[2]) 
)
```

``` r
CoefResult |> 
knitr::kable(
  align = 'c', 
  caption = "Coefficient Result For Risk I and Risk II"
)
```

#### Save Result For Scenario VI

``` r
Scenario6_Sim2_Result <- list(
  Quantiles = seq(0.05, 0.95, by = 0.05),
  Time = Time, 
  CIFPredRisk1 = cifRisk1, 
  CIFPredRisk2 = cifRisk2, 
  MSE = TotalMSE, 
  BrierScoreAverage = BrierAveResult, 
  BrierScoreRisk1 = BrierEVALRisk1, 
  BrierScoreRisk2 = BrierEVALRisk2, 
  AucAverageResult = AUCAveResult,
  AucScoreRisk1 = AUCEvalResultrisk1, 
  AucScoreRisk2 = AUCEvalResultrisk2, 
  BiasRisk1 = BiasEVALRisk1, 
  BiasRisk2 = BiasEVALRisk2, 
  DirectCoef = CoefResult
)

saveRDS(Scenario6_Sim2_Result, './FResult/Fresult_Scenario6_Simulation2.rds')
```
