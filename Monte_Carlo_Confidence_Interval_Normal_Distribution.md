Using Monte Carlo simulation to estimate the confidence interval and
estimate the length of the confidence interval in a normal population in
the cases of equal and unequal tails, as well as the standard deviation
being known and the standard deviation being unknown
================
habib Ezzatabadi

## Define a function for simulate CI and get length of CI in Normal population with equal tails and unequal tails

``` r
lfun_ci <- function(
                mu = 1, ## Mean of population
                sig = 1, ## standard devitaion of population
                state_sig = NA,
                ## if state_sig = NULL, function calculate CI with 
                #unknown statndard deviation
                nsim = 1e+4, ## size of simulation, default = 10000
                nsamp = 20,  ## size of sample, default = 20
                alpha = 0.05, ## alpha value for get 100(1-alpha) % oc CI, 
                ## alpha default = 0.05
                left_tail = 1/2, 
                seed = 12334 # for get same results
                )   {
                    set.seed(seed)
                    Mat_sim <- matrix(rnorm(nsim * nsamp, mean = mu, sd = sig), 
                    nsim, nsamp) ## simulate matrix of samples

                    ## calculate standard deviation of simulated samples
                    ss <- apply(Mat_sim, 1, sd) 
                    ## calculate mean of simulated samples
                    Xbar <- rowMeans(Mat_sim) 
                    a1 <- alpha * left_tail ## calculate alpha for lower tail

                    a2 <- alpha - a1 ## calculate alpha for upper tail
                    ## get z quantile for left bond
                    z1 <- qnorm(a1, mean = 0, sd = 1, lower.tail = FALSE) 
                    ## get z quantile for right bond
                    z2 <- qnorm(a2, , mean = 0, sd = 1, lower.tail = FALSE) 
                    ## get t quantile for left bond
                    t1 <- qt(a1, df = nsamp-1, lower.tail = FALSE) 
                    ## get t quantile for right bond
                    t2 <- qt(a2, df = nsamp-1, lower.tail = FALSE) 
                    if (! is.null(state_sig)) { ## check standard 
                    ## deviatioan known or unknown
                        ## get lower bond of CI
                        lower_bond = Xbar - z1 * sig/sqrt(nsamp) 
                        ## get upper bond of CI
                        upper_bond = Xbar + z2 * sig/sqrt(nsamp)
                    } else { ## if standard deviation is unknown use these bonds
                        ## get lower bond 
                        lower_bond = Xbar - t1 * ss / sqrt(nsamp)
                        ## get upper bond
                        upper_bond = Xbar + t2 * ss / sqrt(nsamp)
                    }
                    L <- upper_bond - lower_bond ## calculate length of bonds
                    ## Calculate the confidence percentage of the 
                    ## simulated confidence interval
                    ## condition 1: mu > lower_bond
                    temp1 <- 1 * (mu > lower_bond) 
                    ## condition 2: mu < upper_bond
                    temp2 <- 1 * (mu < upper_bond)
                    ## get intersection between condition 1 and condition 2
                    temp3 <- temp1 * temp2
                    ci_sim <- mean(temp3)
                    ## calculate output of function 
                    Result <- list(
                        Tails = sprintf("Tails : %s", ifelse(left_tail == 1/2, 
                        "Equal", "Unequal")),
                        alpha = sprintf("alpha = %.2f", alpha), 
                        sample_size = sprintf("size of sample = %d", nsamp), 
                        simulation_size = 
                        sprintf("size of Simulation = %d", nsim), 
                        standard_deviation = 
                        sprintf("Standard Deviation = %s", 
                        ifelse(is.null(state_sig), "Unknown", 
                        as.character(sig))), 
                        Mean = sprintf("Mean = %.2f", mu),
                        Result = c("Average of CI simulation" = ci_sim, 
                    "Average of Length of CI" = mean(L)))
                    return (Result)
                }
```

------------------------------------------------------------------------

------------------------------------------------------------------------

## Example I:

#### $\mu = 2~~\sigma = 3$, sample size = 15, sample of simulation = 10000,

#### Standard Deviation = Known and Tails are Equal

``` r
### EX I
## Mean = 2, Standard Deviation = 3, sample size = 15
lfun_ci(nsamp = 15, mu = 2, sig = 3)
```

    ## $Tails
    ## [1] "Tails : Equal"
    ## 
    ## $alpha
    ## [1] "alpha = 0.05"
    ## 
    ## $sample_size
    ## [1] "size of sample = 15"
    ## 
    ## $simulation_size
    ## [1] "size of Simulation = 10000"
    ## 
    ## $standard_deviation
    ## [1] "Standard Deviation = 3"
    ## 
    ## $Mean
    ## [1] "Mean = 2.00"
    ## 
    ## $Result
    ## Average of CI simulation  Average of Length of CI 
    ##                 0.952900                 3.036363

------------------------------------------------------------------------

## Example II:

#### $\mu = 10, ~~\sigma = 2.5$, sample size = 15,

#### size of simulation = 1000, Standard Deviaion = Known and Tails are Equal

``` r
lfun_ci(nsamp = 10, mu = 10, sig = 2.5, nsim = 1000)
```

    ## $Tails
    ## [1] "Tails : Equal"
    ## 
    ## $alpha
    ## [1] "alpha = 0.05"
    ## 
    ## $sample_size
    ## [1] "size of sample = 10"
    ## 
    ## $simulation_size
    ## [1] "size of Simulation = 1000"
    ## 
    ## $standard_deviation
    ## [1] "Standard Deviation = 2.5"
    ## 
    ## $Mean
    ## [1] "Mean = 10.00"
    ## 
    ## $Result
    ## Average of CI simulation  Average of Length of CI 
    ##                 0.953000                 3.098975

------------------------------------------------------------------------

## Example III:

#### $\mu = 2, ~~\sigma = 3$, sample size = 15,

#### size of simulation = 10000, Standard Deviaion = Known and tails are Unequal

``` r
## Mean = 2, Standard Deviation = 3, sample size = 15, 
## alpha of left tail = alpha / 3

lfun_ci(nsamp = 15, mu = 2, sig = 3, left_tail = 1 / 3)
```

    ## $Tails
    ## [1] "Tails : Unequal"
    ## 
    ## $alpha
    ## [1] "alpha = 0.05"
    ## 
    ## $sample_size
    ## [1] "size of sample = 15"
    ## 
    ## $simulation_size
    ## [1] "size of Simulation = 10000"
    ## 
    ## $standard_deviation
    ## [1] "Standard Deviation = 3"
    ## 
    ## $Mean
    ## [1] "Mean = 2.00"
    ## 
    ## $Result
    ## Average of CI simulation  Average of Length of CI 
    ##                 0.952300                 3.068921

------------------------------------------------------------------------

## Example IV:

#### $\mu = 10, ~~\sigma = 2.5$, sample size = 10,

#### size of simulation = 1000, Standard Deviaion = Known and tails are Unequal

``` r
## Mean = 10, Standard Deviation = 2.5, sample size = 15, 
## size of simulation = 1000, alpha of left tail = alpha / 4

lfun_ci(nsamp = 10, mu = 10, sig = 2.5, nsim = 1000, 
left_tail = 1/4)
```

    ## $Tails
    ## [1] "Tails : Unequal"
    ## 
    ## $alpha
    ## [1] "alpha = 0.05"
    ## 
    ## $sample_size
    ## [1] "size of sample = 10"
    ## 
    ## $simulation_size
    ## [1] "size of Simulation = 1000"
    ## 
    ## $standard_deviation
    ## [1] "Standard Deviation = 2.5"
    ## 
    ## $Mean
    ## [1] "Mean = 10.00"
    ## 
    ## $Result
    ## Average of CI simulation  Average of Length of CI 
    ##                 0.955000                 3.179565

------------------------------------------------------------------------

## Example V:

#### $\mu = 2, ~~\sigma = 3$, sample size = 15,

#### size of simulation = 10000, Standard Deviaion = UnKnown and tails are Equal

``` r
## Mean = 2, Standard Deviation = 3, sample size = 15, 
## Standard Deviation is Unknown
lfun_ci(nsamp = 15, mu = 2, sig = 3, state_sig = NULL)
```

    ## $Tails
    ## [1] "Tails : Equal"
    ## 
    ## $alpha
    ## [1] "alpha = 0.05"
    ## 
    ## $sample_size
    ## [1] "size of sample = 15"
    ## 
    ## $simulation_size
    ## [1] "size of Simulation = 10000"
    ## 
    ## $standard_deviation
    ## [1] "Standard Deviation = Unknown"
    ## 
    ## $Mean
    ## [1] "Mean = 2.00"
    ## 
    ## $Result
    ## Average of CI simulation  Average of Length of CI 
    ##                 0.951500                 3.263993

------------------------------------------------------------------------

## Example VI:

#### $\mu = 10, ~~\sigma = 2.5$, sample size = 10,

#### size of simulation = 1000, Standard Deviaion = UnKnown and tails are Equal

``` r
## Mean = 10, Standard Deviation = 2.5, sample size = 15, 
## size of simulation = 1000, Standard Deviation is Unknown

lfun_ci(nsamp = 10, mu = 10, sig = 2.5, nsim = 1000, state_sig = NULL)
```

    ## $Tails
    ## [1] "Tails : Equal"
    ## 
    ## $alpha
    ## [1] "alpha = 0.05"
    ## 
    ## $sample_size
    ## [1] "size of sample = 10"
    ## 
    ## $simulation_size
    ## [1] "size of Simulation = 1000"
    ## 
    ## $standard_deviation
    ## [1] "Standard Deviation = Unknown"
    ## 
    ## $Mean
    ## [1] "Mean = 10.00"
    ## 
    ## $Result
    ## Average of CI simulation  Average of Length of CI 
    ##                 0.956000                 3.468615

------------------------------------------------------------------------

## Example VII:

#### $\mu = 2, ~~\sigma = 3$, sample size = 15,

#### size of simulation = 10000, Standard Deviaion = UnKnown and tails are Unequal

``` r
## Mean = 2, Standard Deviation = 3, sample size = 15, 
## alpha of left tail = alpha / 3, Standard Deviaion is Unknown

lfun_ci(nsamp = 15, mu = 2, sig = 3, left_tail = 1 / 3, state_sig = NULL)
```

    ## $Tails
    ## [1] "Tails : Unequal"
    ## 
    ## $alpha
    ## [1] "alpha = 0.05"
    ## 
    ## $sample_size
    ## [1] "size of sample = 15"
    ## 
    ## $simulation_size
    ## [1] "size of Simulation = 10000"
    ## 
    ## $standard_deviation
    ## [1] "Standard Deviation = Unknown"
    ## 
    ## $Mean
    ## [1] "Mean = 2.00"
    ## 
    ## $Result
    ## Average of CI simulation  Average of Length of CI 
    ##                 0.951500                 3.308714

------------------------------------------------------------------------

## Example VIII:

#### $\mu = 10, ~~\sigma = 2.5$, sample size = 10,

#### size of simulation = 1000, Standard Deviaion = UnKnown and tails are Unequal

``` r
## Mean = 10, Standard Deviation = 2.5, sample size = 15, 
## size of simulation = 1000, alpha of left tail = alpha / 4, 
## Standard Deviation is Unknown

lfun_ci(nsamp = 10, mu = 10, sig = 2.5, nsim = 1000, 
left_tail = 1/4, state_sig = NULL)
```

    ## $Tails
    ## [1] "Tails : Unequal"
    ## 
    ## $alpha
    ## [1] "alpha = 0.05"
    ## 
    ## $sample_size
    ## [1] "size of sample = 10"
    ## 
    ## $simulation_size
    ## [1] "size of Simulation = 1000"
    ## 
    ## $standard_deviation
    ## [1] "Standard Deviation = Unknown"
    ## 
    ## $Mean
    ## [1] "Mean = 10.00"
    ## 
    ## $Result
    ## Average of CI simulation  Average of Length of CI 
    ##                 0.960000                 3.601555

------------------------------------------------------------------------

------------------------------------------------------------------------

## Compare results

``` r
## compare results

temp1 <- lfun_ci(nsamp = 15, mu = 2, sig = 3)$Result
temp2 <- lfun_ci(nsamp = 15, mu = 2, sig = 3, left_tail = 1 / 3)$Result
temp3 <- lfun_ci(nsamp = 15, mu = 2, sig = 3, state_sig = NULL)$Result
temp4 <- lfun_ci(nsamp = 15, mu = 2, sig = 3, left_tail = 1 / 3, 
state_sig = NULL)$Result

Compare_Result_1 <- rbind(temp1, temp2, temp3, temp4)
row_names <- c("Known sd with equal tails", 
"Known sd with unequal tails", 
"UnKnown sd with equal tails", 
"UnKnown sd with Unequal tails")
res1 <- as.data.frame(Compare_Result_1)
rownames(res1) <- row_names

knitr :: kable(res1, align = "c", caption = "Compare of Results for 
sample size = 15, Mean = 2, Standard Deviation = 3")
```

|                               | Average of CI simulation | Average of Length of CI |
|:------------------------------|:------------------------:|:-----------------------:|
| Known sd with equal tails     |          0.9529          |        3.036363         |
| Known sd with unequal tails   |          0.9523          |        3.068921         |
| UnKnown sd with equal tails   |          0.9515          |        3.263993         |
| UnKnown sd with Unequal tails |          0.9515          |        3.308714         |

Compare of Results for sample size = 15, Mean = 2, Standard Deviation =
3

``` r
## For Ex II

temp21 <- lfun_ci(nsamp = 10, mu = 10, sig = 2.5, nsim = 1000)$Result
temp22 <- lfun_ci(nsamp = 10, mu = 10, sig = 2.5, nsim = 1000, 
            left_tail = 1/4)$Result
temp23 <- lfun_ci(nsamp = 10, mu = 10, sig = 2.5, nsim = 1000, 
state_sig = NULL)$Result
temp24 <- lfun_ci(nsamp = 10, mu = 10, sig = 2.5, nsim = 1000, 
            left_tail = 1/4, state_sig = NULL)$Result

Compare_Result_2 <- rbind(temp21, temp22, temp23, temp24)
res2 <- as.data.frame(Compare_Result_2)
rownames(res2) <- row_names
knitr :: kable(res2, align = "c", caption = "Compare of Results for 
sample size = 10, Mean = 10, Standard Deviation = 2.5")
```

|                               | Average of CI simulation | Average of Length of CI |
|:------------------------------|:------------------------:|:-----------------------:|
| Known sd with equal tails     |          0.953           |        3.098975         |
| Known sd with unequal tails   |          0.955           |        3.179565         |
| UnKnown sd with equal tails   |          0.956           |        3.468615         |
| UnKnown sd with Unequal tails |          0.960           |        3.601555         |

Compare of Results for sample size = 10, Mean = 10, Standard Deviation =
2.5
