# Doubly robust lower predictive bounds under competing risks

Reproduction code for

> H. Ezzatabadipour and R. Delino. *Doubly robust calibration of lower predictive bounds for a cause-specific event time under competing risks.*

The whole analysis is in one file, **`cr_all.R`**. Sourcing it reproduces every number, table and figure in the paper.

## Quick start

Start a **fresh R session** (variables left over from an earlier session can override the settings), set the working directory to this folder, and run:

```r
SIM_CORES <- 4          # optional; default 1
source("cr_all.R")
```

or, from a terminal:

```sh
Rscript cr_all.R 4
```

With 4 cores the full run takes about 50 minutes. Everything is written to the folder `out_cr/`.

## Requirements

- R (≥ 4.2)
- `survival`
- `parallel` (part of base R)
- `randomForestSRC` (only for stage `rf`)

```r
install.packages(c("survival", "randomForestSRC"))
```

The results in the paper were produced with R 4.6.0, survival 3.8-6 and randomForestSRC 3.10.0 on Windows 11.

## What is produced

| Stage | Files in `out_cr/` | Result in the paper |
|---|---|---|
| `main` | `results_main.rds`, `summary_main.csv` | Table 1, Figure 1 |
| `nsweep` | `results_nsweep.rds`, `summary_nsweep.csv` | Figure 2 |
| `rf` | `results_rf.rds`, `summary_rf.csv` | Scenario S6, Figure 4 (Web Figure 1) |
| `drcheck` | printed in the log | Table A1 (Web Appendix C) |
| `app` | `app_mgus2_alpha05.rds`, `app_mgus2_alpha10.rds` | Section 6, Figure 3 |
| `figs` | `figs/Fig1`–`Fig4` (`.pdf` and `.eps`) | Figures 1–4 |
| all | `ALL_RESULTS.txt` | every summary table and `sessionInfo()` |

`smoke` is an optional 10-second check (3 replications). It is not run by default.

## Settings

Define any of these **before** `source()`; otherwise the defaults are used.

| Variable | Default | Meaning |
|---|---|---|
| `SIM_CORES` | `1` | number of cores |
| `RUN_STAGES` | all except `"smoke"` | any of `"smoke"`, `"main"`, `"nsweep"`, `"rf"`, `"drcheck"`, `"app"`, `"figs"` |
| `OUT_DIR` | `"out_cr"` | output folder |
| `FIG_DEVICE` | `"cairo"` | `"cairo"` embeds fonts (Helvetica is drawn as Arial); `"base"` uses `pdf()`/`postscript()` |
| `FIG_FAMILY` | `"Helvetica"` | font family, e.g. `"Times"` |
| `FIG_POINTSIZE` | `9` | base font size |
| `FIG_FORMAT` | `"both"` | `"pdf"`, `"eps"` or `"both"` |
| `FIG_NAMES` | `"springer"` | `"springer"` (Fig1–Fig4) or `"descriptive"` (fig_coverage, …) |
| `FIG_SCALE` | `1` | size multiplier |
| `FIG_COLOR` | `TRUE` | `FALSE` gives greyscale figures |
| `FIG_EMBED` | `FALSE` | embed fonts with Ghostscript (only with `FIG_DEVICE = "base"`) |

Examples:

```r
# only the application and the figures
RUN_STAGES <- c("app", "figs"); source("cr_all.R")

# redraw the figures in Times, PDF only, from saved results
RUN_STAGES <- "figs"; FIG_FAMILY <- "Times"; FIG_FORMAT <- "pdf"; source("cr_all.R")
```

## Reproducibility

Every simulation replication and every data split has its own fixed seed, so the results do not depend on the number of cores. If a stage fails (for example, `randomForestSRC` is not installed), the error is written to `ALL_RESULTS.txt` and the remaining stages still run.

## Data

The application uses the `mgus2` data distributed with the R package `survival`. No other data are needed.
