# bootstrap using R


## loading require packages

``` r
if (!require(tidymodels)) {
    chooseCRANmirror(ind = 1, graphics = FALSE)
    install.packages("tidymodels")
    library(tidymodels)
}

if (!require(boot)) {
    chooseCRANmirror(ind = 1, graphics = FALSE)
    install.packages("boot")
    library(boot)
}

if (!require(bootstrap)) {
    chooseCRANmirror(ind = 1, graphics = FALSE)
    install.packages("bootstrap")
    library(bootstrap)
}
```

## Generate data

``` r
seed <- 1234
n <- 30
set.seed(seed)


dat <- tibble(
    x1 = rgamma(n, 2, 2), 
    x2 = rnorm(n, 3, 1),
    x3 = rbinom(n, size = 10, prob = .4), 
    y = 2 * x1 + 3 * x2 - x3 + rnorm(n)
)
```

## split data into train and test

``` r
data_split <- initial_split(dat, prop = 0.75)
train_dat <- training(data_split)
test_dat <- testing(data_split)
```

## implement regression model using boot package

``` r
startt <- Sys.time()
coef_fun <- function(dat, Ind) {
    d_temp <- dat[Ind, ]
    m_temp <- lm(y ~ ., data = d_temp)
    return(coef(m_temp))
}
res1 <- boot :: boot(data = train_dat, statistic = coef_fun, 
R = 1e+4)
(boot_coef <- res1$t  |> colMeans() |> setNames(
    c("Intercept", paste0("x", 1:3)))
)
```

    Intercept        x1        x2        x3 
     0.978266  1.655473  2.930032 -1.092260 

``` r
endd <- Sys.time()
(boot_time <- difftime(endd, startt, unit = 'sec'))
```

    Time difference of 4.190735 secs

------------------------------------------------------------------------

------------------------------------------------------------------------

## implement regression model using bootstrap package

``` r
startt2 <- Sys.time()
N <- nrow(train_dat)
coef_funn <- function(x, train_dat) { 
    temp_dat <- train_dat
    temp_m <- lm(y ~ ., data = temp_dat)
    return(coef(temp_m)) 
}

res2 <- bootstrap(1:N, 2e+3, coef_funn, train_dat)
(bootstrap_coef <- res2$thetastar  |> rowMeans())
```

    (Intercept)          x1          x2          x3 
      0.7461783   1.8393976   2.9338510  -1.0632986 

``` r
endd2 <- Sys.time()
(bootstrap_time <- difftime(endd2, startt2, unit = 'sec'))
```

    Time difference of 0.7262728 secs

------------------------------------------------------------------------

------------------------------------------------------------------------

## implement regression model using rsample package

``` r
startt3 <- Sys.time()

bootstraps <- bootstraps(train_dat, times = 2e+3)

temp_model_3 <- function(split) {
    analysis <- analysis(split)
    model <- lm(y ~ ., data = analysis)
    return(coef(model))
}


model_coefficients <- bootstraps$splits |> 
                    map_dfr(temp_model_3, .id = "bootstrap_sample")
(rsample_coef <- model_coefficients |> _[, -1] |> colMeans())
```

    (Intercept)          x1          x2          x3 
      0.9755329   1.6721255   2.9303827  -1.0935066 

``` r
endd3 <- Sys.time()
(rsample_time <- difftime(endd3, startt3, unit = 'sec'))
```

    Time difference of 1.047765 secs

## report result

``` r
tibble(package = c("boot", "bootstrap", "rsample"), 
        Elapsed_Time = c(boot_time, bootstrap_time, rsample_time), 
        Intercept_bias = c(boot_coef[1], bootstrap_coef[1], rsample_coef[1]), 
        x1_bias = c(boot_coef[2] - 2, bootstrap_coef[2] - 2, rsample_coef[2] - 2),
        x2_bias =  c(boot_coef[3] - 3, bootstrap_coef[3] - 3, rsample_coef[3] - 3),
        x3_bias = c(boot_coef[4] + 1, bootstrap_coef[4] + 1, rsample_coef[4] + 1)
) |> 
knitr :: kable(align = "c", caption = "Table of Results")
```

|  package  |  Elapsed_Time  | Intercept_bias |  x1_bias   |  x2_bias   |  x3_bias   |
|:---------:|:--------------:|:--------------:|:----------:|:----------:|:----------:|
|   boot    | 4.1907351 secs |   0.9782660    | -0.3445268 | -0.0699677 | -0.0922602 |
| bootstrap | 0.7262728 secs |   0.7461783    | -0.1606024 | -0.0661490 | -0.0632986 |
|  rsample  | 1.0477648 secs |   0.9755329    | -0.3278745 | -0.0696173 | -0.0935066 |

Table of Results
