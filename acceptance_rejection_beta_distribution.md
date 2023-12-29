# acceptance-rejection method with R

## We want to generate data using R from the following distribution

$$
Y \sim \text{Beta}(shape_1: 2.7, shape_2: 6.3)
$$

``` r
a <- 2.7
b <- 6.3
f_max <- function(x) {
    temp <- dbeta(x, shape1 = a, shape2 = b)
    return(temp)
}
res <- optimize(f_max, interval = c(0, 1), maximum = TRUE)
C <- res$objective
sim_fun <- function(n) {
    i <- 0
    j <- 0
    simul <- c()
    while(i < n) {
        j <- j + 1
        uv <- runif(2)
        v <- uv[2]; u <- uv[1]
        ratio <- f_max(v) / C
        temp <- ifelse(u <= ratio, TRUE, FALSE)
        if (temp) {
            i <- i + 1
            simul[i] <- v
        }
    }

    result <- list(nc = j, sim_result = simul)
}

res1 <- sim_fun(n = 1e+4)


hist(res1$sim_result, freq = FALSE)
xx <- seq(0, 1, len = 1e+4)
yy <- f_max(xx)
lines(xx, yy, col = "red", lwd = 2)
```

![](acceptance_rejection_beta_distribution_files/figure-commonmark/unnamed-chunk-1-1.png)

``` r
M <- mean(res1$sim_result)
varr <- var(res1$sim_result)
cat("Mean simulation: ", M, "\n", 
    "Mean real: ", a / (a + b), "\n", 
    "Variance simulation: ", varr, "\n", 
    "variance Real: ", (a*b) / ((a + b)**2 * (a + b + 1)), 
    sep = "")
```

    Mean simulation: 0.2987756
    Mean real: 0.3
    Variance simulation: 0.02070279
    variance Real: 0.021

``` r
res1 <- sim_fun(n = 1e+5)


hist(res1$sim_result, freq = FALSE)
xx <- seq(0, 1, len = 1e+4)
yy <- f_max(xx)
lines(xx, yy, col = "red", lwd = 2)
```

![](acceptance_rejection_beta_distribution_files/figure-commonmark/unnamed-chunk-1-2.png)

``` r
M <- mean(res1$sim_result)
varr <- var(res1$sim_result)
cat("Mean simulation: ", M, "\n", 
    "Mean real: ", a / (a + b), "\n", 
    "Variance simulation: ", varr, "\n", 
    "variance Real: ", (a*b) / ((a + b)**2 * (a + b + 1)), 
    sep = "")
```

    Mean simulation: 0.3006579
    Mean real: 0.3
    Variance simulation: 0.0210233
    variance Real: 0.021
