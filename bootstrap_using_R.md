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
    m_temp <- lm(y ~ . - 1, data = d_temp)
    return(coef(m_temp))
}
res1 <- boot :: boot(data = train_dat, statistic = coef_fun, 
R = 1e+4)
(boot_coef <- res1$t  |> colMeans() |> setNames(
                paste0("x", 1:3))
)
```

           x1        x2        x3 
     1.821370  3.087255 -1.015949 

``` r
endd <- Sys.time()
(boot_time <- difftime(endd, startt, unit = 'sec'))
```

    Time difference of 4.127518 secs

``` r
y_test <- test_dat['y'] |> unlist()
x_test = test_dat[, -4]
boot_pred_test <- boot_coef %*% (x_test |> as.data.frame() |> data.matrix() |> t()) |> as.numeric()
(RMSE_boot <- sqrt(mean((y_test - boot_pred_test)^2)))
```

    [1] 1.041219

------------------------------------------------------------------------

------------------------------------------------------------------------

## implement regression model using bootstrap package

``` r
startt2 <- Sys.time()
N <- nrow(train_dat)
coef_funn <- function(x, train_dat) { 
    temp_dat <- train_dat[x, ]
    temp_m <- lm(y ~ . -1, data = temp_dat)
    return(coef(temp_m)) 
}

res2 <- bootstrap(1:N, 1e+4, coef_funn, train_dat)
(bootstrap_coef <- res2$thetastar  |> rowMeans())
```

           x1        x2        x3 
     1.823545  3.088103 -1.017617 

``` r
endd2 <- Sys.time()
(bootstrap_time <- difftime(endd2, startt2, unit = 'sec'))
```

    Time difference of 4.129818 secs

``` r
bootstrap_pred_test <- bootstrap_coef %*% 
                    (x_test |> as.data.frame() |> data.matrix() |> t()) |> 
                    as.numeric()
(RMSE_bootstrap <- sqrt(mean((y_test - bootstrap_pred_test)^2)))
```

    [1] 1.044444

------------------------------------------------------------------------

------------------------------------------------------------------------

## implement regression model using rsample package

``` r
startt3 <- Sys.time()

bootstraps <- bootstraps(train_dat, times = 1e+4)

temp_model_3 <- function(split) {
    analysis <- analysis(split)
    model <- lm(y ~ . -1, data = analysis)
    return(coef(model))
}


model_coefficients <- bootstraps$splits |> 
                    map_dfr(temp_model_3, .id = "bootstrap_sample")
(rsample_coef <- model_coefficients |> _[, -1] |> colMeans())
```

           x1        x2        x3 
     1.821007  3.090123 -1.017255 

``` r
endd3 <- Sys.time()
(rsample_time <- difftime(endd3, startt3, unit = 'sec'))
```

    Time difference of 4.704723 secs

``` r
rsample_pred_test <- rsample_coef %*% 
                    (x_test |> as.data.frame() |> data.matrix() |> t()) |> 
                    as.numeric()
(RMSE_rsample <- sqrt(mean((y_test - rsample_pred_test)^2)))
```

    [1] 1.040529

------------------------------------------------------------------------

------------------------------------------------------------------------

## Using python

``` python
import pandas as pd
import numpy as np
from sklearn.utils import resample
from sklearn.linear_model import LinearRegression
import time

py_dat = r.train_dat




startt4 = time.process_time()
## implement regression model 
py_model = LinearRegression()

n_iterations = int(1e+4)
python_coef = pd.DataFrame()

for i in range(n_iterations):
    X_train = resample(py_dat.drop('y', axis=1), replace=True, n_samples=len(py_dat))
    y_train = py_dat.loc[X_train.index, 'y']
    # Fit model
    py_model.fit(X_train, y_train)
    # Store coefficients
    coefs = pd.DataFrame([py_model.coef_], columns=X_train.columns)
    python_coef = pd.concat([python_coef, coefs])
```

``` python

# Now, bootstrap_coefs contains the coefficients from each bootstrap sample

python_coef_boot = python_coef.mean().values
endd4 = time.process_time()
python_time = endd4 - startt4

real_coef = np.array([2, 3, -1])
python_bias = python_coef_boot - real_coef


py_test_dat = r.test_dat 
x_test = py_test_dat.drop(columns = 'y')
py_pred_test = python_coef_boot @ x_test.T
y_test = py_test_dat['y'].values
py_err_test = (py_pred_test.values - y_test)**2
RMSE_py = np.sqrt(py_err_test.mean())
```

------------------------------------------------------------------------

------------------------------------------------------------------------

## report result

``` r
tibble(package = c("boot", "bootstrap", "rsample", "python_bootstrap"), 
        Elapsed_Time = c(boot_time, bootstrap_time, rsample_time, py$python_time), 
        x1_bias = c(boot_coef[1] - 2, bootstrap_coef[1] - 2, rsample_coef[1] - 2, py$python_bias[1]),
        x2_bias =  c(boot_coef[2] - 3, bootstrap_coef[2] - 3, rsample_coef[2] - 3, py$python_bias[2]),
        x3_bias = c(boot_coef[3] + 1, bootstrap_coef[3] + 1, rsample_coef[3] + 1, py$python_bias[3]), 
        RMSE_test = c(RMSE_boot, RMSE_bootstrap, RMSE_rsample, py$RMSE_py)
) |> 
knitr :: kable(align = "c", caption = "Table of Results")
```

|     package      | Elapsed_Time  |  x1_bias   |  x2_bias   |  x3_bias   | RMSE_test |
|:----------------:|:-------------:|:----------:|:----------:|:----------:|:---------:|
|       boot       | 4.127518 secs | -0.1786297 | 0.0872549  | -0.0159486 | 1.041219  |
|    bootstrap     | 4.129818 secs | -0.1764553 | 0.0881029  | -0.0176172 | 1.044444  |
|     rsample      | 4.704723 secs | -0.1789929 | 0.0901226  | -0.0172552 | 1.040529  |
| python_bootstrap | 9.875000 secs | -0.3385282 | -0.0661995 | -0.0937691 | 1.770228  |

Table of Results
