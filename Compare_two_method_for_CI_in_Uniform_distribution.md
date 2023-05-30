Compare Two Method for create confidence Interval for parameter theta in
Uniform(0, theta)
================
Habib Ezzatabadi
2023-05-28

## First Method

$$
\begin{aligned}
& \text{if}~~ X_1, X_2, \dots, X_n \overset{iid}{\sim} \text{Unif}(0, \theta), \\
& \implies Y_i = \frac{X_i}{\theta} \sim \text{Unif}(0, 1)  \implies -2\ln(Y_i) \sim \chi^2_{(2)}, \\
& -2\sum_{i = 1}^n \ln(Y_i) \sim \chi^2_{(2n)} \implies \\
& \mathcal{P}\left(\chi^2_{(2n,~\frac{\alpha}{2})} \leq -2\sum_{i = 1}^n \ln(Y_i)\leq \chi^2_{(2n, ~1-\frac{\alpha}{2})}  \right) = 1 - \alpha \\
& \implies 
\mathcal{P}\left(\chi^2_{(2n,~\frac{\alpha}{2})} \leq -2\sum_{i = 1}^n \ln\left(\frac{X_i}{\theta}\right)\leq \chi^2_{(2n, ~1-\frac{\alpha}{2})}  \right) = 1 - \alpha \\
& \implies \mathcal{P}\left(\frac{\chi^2_{(2n,~\frac{\alpha}{2})} + 2\sum_{i = 1}^n \ln(X_i)}{2n} \leq \ln(\theta) \leq 
\frac{\chi^2_{(2n,~1-\frac{\alpha}{2})} + 2\sum_{i = 1}^n \ln(X_i)}{2n}  \right) = 1 - \alpha \\
& \implies \mathcal{P}\left[\exp\left( \frac{\chi^2_{(2n,~\frac{\alpha}{2})} + 2\sum_{i = 1}^n \ln(X_i)}{2n}\right)\leq \theta \leq \exp\left( \frac{\chi^2_{(2n,~1-\frac{\alpha}{2})} + 2\sum_{i = 1}^n \ln(X_i)}{2n} \right)\right] = 1 - \alpha
\end{aligned}
$$

## code in R for method I with theta = 1

``` r
set.seed(1234)
get_conf_sim_uniform <- function(theta = 1, alpha = 0.05, nsamp = 25, R = 10000) {
    Lower <- c()
    Upper <- c()
    temp1 <- qchisq(alpha/2, df = 2*nsamp)
    temp2 <- qchisq(1 - alpha/2, df = 2 * nsamp)
    count <- 0
    repeat{
        count = count + 1
        U <- runif(n = nsamp, min = 0, max = theta)
        sumlogx_2 <- 2 * sum(log(U))
        a <- (temp1 + sumlogx_2) / (2 * nsamp)
        b <- (temp2 + sumlogx_2) / (2 * nsamp)
        res1 <- exp(a); 
        res2 <- exp(b)
        Lower <- c(Lower, res1)
        Upper <- c(Upper, res2)
        if (count == R) break
    }
    Average_Length <- (Upper - Lower) |> mean()
    mat_result <- cbind(Lower = Lower, Upper = Upper)
    conf_res <- apply(mat_result, 1, function(x){
        a <- x[1]; b <- x[2]
        cond <- (a < theta & b > theta)
        return(1 * cond)
    })
    simulate_conf <- mean(conf_res)
    final_result <- c(theta = theta, Average_CI = Average_Length, Simulated_Conf = simulate_conf)
    attr(final_result, "Method" ) <-  "Method I" 
    return(final_result)
}

get_conf_sim_uniform()
```

    ##          theta     Average_CI Simulated_Conf 
    ##      1.0000000      0.8494663      0.9507000 
    ## attr(,"Method")
    ## [1] "Method I"

------------------------------------------------------------------------

------------------------------------------------------------------------

$$
\begin{aligned}
& \text{if}~~ X_1, X_2, \dots, X_n \overset{iid}{\sim} \text{Unif}(0, \theta)\implies \\
& Y = \frac{X_{(n)}}{\theta} \sim \text{Beta}(n, 1) \implies \\
& \mathcal{P}(\text{Beta}(n, 1, \alpha/2) \leq Y \leq \text{Beta}(n, 1, 1-\alpha/2)) = 1-\alpha \\
& \mathcal{P}\left(\frac{X_{(n)}}{\text{Beta}(n, 1, 1-\alpha/2)} \leq \theta \leq \frac{X_{(n)}}{\text{Beta}(n, 1, \alpha/2)}\right) = 1-\alpha 
\end{aligned}
$$

## Method II with theta = 1

``` r
set.seed(1234)
get_conf_sim_uniform2 <- function(theta = 1, alpha = 0.05, nsamp = 25, R = 10000) {
    Lower <- c()
    Upper <- c()
    temp1 <- qbeta(alpha/2, nsamp, 1)
    temp2 <- qbeta(1 - alpha/2, nsamp, 1)
    count <- 0
    repeat{
        count = count + 1
        U <- runif(n = nsamp, min = 0, max = theta)
        yy <- max(U)
        a <- yy / temp2
        b <- yy / temp1
        Lower <- c(Lower, a)
        Upper <- c(Upper, b)
        if (count == R) break
    }
    Average_Length <- (Upper - Lower) |> mean()
    mat_result <- cbind(Lower = Lower, Upper = Upper)
    conf_res <- apply(mat_result, 1, function(x){
        a <- x[1]; b <- x[2]
        cond <- (a < theta & b > theta)
        return(1 * cond)
    })
    simulate_conf <- mean(conf_res)
    final_result <- c(theta = theta, Average_CI = Average_Length, Simulated_Conf = simulate_conf)
    attr(final_result, "Method" ) <-  "Method II" 
    return(final_result)
}

get_conf_sim_uniform2()
```

    ##          theta     Average_CI Simulated_Conf 
    ##       1.000000       0.151954       0.947800 
    ## attr(,"Method")
    ## [1] "Method II"

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

## Method I with theta = 3, nsample = 35

``` r
get_conf_sim_uniform(theta = 3, nsamp = 35)
```

    ##          theta     Average_CI Simulated_Conf 
    ##       3.000000       2.101802       0.948200 
    ## attr(,"Method")
    ## [1] "Method I"

## Method II with theta = 3, nsample = 35

``` r
get_conf_sim_uniform2(theta = 3, nsamp = 35)
```

    ##          theta     Average_CI Simulated_Conf 
    ##       3.000000       0.322127       0.946700 
    ## attr(,"Method")
    ## [1] "Method II"
