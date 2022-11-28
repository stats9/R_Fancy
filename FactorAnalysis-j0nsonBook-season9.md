FactorAnalysis-johnsonBook-season9
================

# Problem 9.12

``` r
# call libraries ----------------------------------
### install.packages("psych") ------------------------
### install.packages("tidyverse") -----------------------
### install.packages("GPArotation") --------------------------
## install.packages(devtools) ------------------------------
## devtools :: install_github("stats9/contigencyTable2", subdir = "contigencyTable2") -------------------------
library(psych)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ ggplot2::%+%()   masks psych::%+%()
    ## ✖ ggplot2::alpha() masks psych::alpha()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()

``` r
library(GPArotation)
library(contigencyTable2)
```

## section a

##### perepare data—————————-

``` r
S <- matrix(c(11.555, 8.367, 8.508, 8.367, 6.697, 6.264, 
8.508, 6.264, 7.061), 3, 3)
S <- 10^(-3) * S
lam <- c(.107, 0.078, .08)
```

``` r
variance_khas <- (S - tcrossprod(lam)) %>% diag %>% diag(3)
variance_khas
```

    ##          [,1]     [,2]     [,3]
    ## [1,] 0.000106 0.000000 0.000000
    ## [2,] 0.000000 0.000613 0.000000
    ## [3,] 0.000000 0.000000 0.000661

## section b ——————

``` r
variance_eshteraki <- lam^2 %>% setNames(paste0("h", 1:3))
variance_eshteraki
```

    ##       h1       h2       h3 
    ## 0.011449 0.006084 0.006400

## section c ——————–

``` r
eigen(S)$values -> E
E[1] / sum(E) 
```

    ## [1] 0.9603571

## section d ————————-

``` r
n <- 24
Sn <- (n - 1) / n * S
residulas <- Sn - tcrossprod(lam) - variance_khas
residulas
```

    ##               [,1]          [,2]          [,3]
    ## [1,] -0.0004814583 -0.0003276250 -0.0004065000
    ## [2,] -0.0003276250 -0.0002790417 -0.0002370000
    ## [3,] -0.0004065000 -0.0002370000 -0.0002942083

# Problem 16 ——————————-

### prepate data

``` r
Nam <- c(
"sales growth",
"sales profitability",
"new-account sales",
"creativity test",
"mechanical reasoning test",
"abstract reasoning test",
"mathematics test"
)
dat <- read_table(file.choose(), col_names = Nam)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   `sales growth` = col_double(),
    ##   `sales profitability` = col_double(),
    ##   `new-account sales` = col_double(),
    ##   `creativity test` = col_double(),
    ##   `mechanical reasoning test` = col_double(),
    ##   `abstract reasoning test` = col_double(),
    ##   `mathematics test` = col_double()
    ## )

``` r
list_to_dataframe(dat) %>%
setNames(abbreviate(Nam, minlength = 6, method = "both.sides")) -> dat2
head(dat2)
```

    ##   slsgrw slsprf nw-ccs crtvtt mchnrt abstrt mthmtt
    ## 1   93.0   96.0   97.8      9     12      9     20
    ## 2   88.8   91.8   96.8      7     10     10     15
    ## 3   95.0  100.3   99.0      8     12      9     26
    ## 4  101.3  103.8  106.8     13     14     12     29
    ## 5  102.0  107.8  103.0     10     15     12     32
    ## 6   95.8   97.5   99.3     10     14     11     21

``` r
tail(dat2)
```

    ##    slsgrw slsprf nw-ccs crtvtt mchnrt abstrt mthmtt
    ## 45   94.8  101.8   99.8      7     16     11     24
    ## 46  103.5  112.0  110.8     18     13     12     37
    ## 47   89.5   96.0   97.3      7     15     11     14
    ## 48   84.3   89.8   94.3      8      8      8      9
    ## 49  104.3  109.5  106.5     14     12     12     36
    ## 50  106.0  118.5  105.0     12     16     11     39

## section a ——————————-

``` r
dat_scale <- sapply(dat2, scale)
n <- nrow(dat_scale)
S <- cov(dat_scale)
Sn <- S * (n-1)/n
Model1 <- fa(dat_scale, nfactors = 2, residuals = TRUE, fm = "ml", 
rotate = "none")
Model2 <- fa(dat_scale, nfactors = 3, residuals = TRUE, fm = "ml", 
rotate = "none")
Model1
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = dat_scale, nfactors = 2, rotate = "none", residuals = TRUE, 
    ##     fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##         ML1   ML2   h2    u2 com
    ## slsgrw 0.70  0.67 0.93 0.069 2.0
    ## slsprf 0.67  0.69 0.93 0.070 2.0
    ## nw-ccs 0.80  0.49 0.88 0.123 1.7
    ## crtvtt 0.98 -0.17 1.00 0.005 1.1
    ## mchnrt 0.65  0.31 0.53 0.474 1.4
    ## abstrt 0.25  0.57 0.39 0.614 1.4
    ## mthmtt 0.56  0.81 0.97 0.029 1.8
    ## 
    ##                        ML1  ML2
    ## SS loadings           3.33 2.28
    ## Proportion Var        0.48 0.33
    ## Cumulative Var        0.48 0.80
    ## Proportion Explained  0.59 0.41
    ## Cumulative Proportion 0.59 1.00
    ## 
    ## Mean item complexity =  1.6
    ## Test of the hypothesis that 2 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  21  and the objective function was  10.9 with Chi Square of  499.66
    ## The degrees of freedom for the model are 8  and the objective function was  2.63 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.06 
    ## The df corrected root mean square of the residuals is  0.09 
    ## 
    ## The harmonic number of observations is  50 with the empirical chi square  6.79  with prob <  0.56 
    ## The total number of observations was  50  with Likelihood Chi Square =  117.2  with prob <  1.3e-21 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.382
    ## RMSEA index =  0.522  and the 90 % confidence intervals are  0.446 0.614
    ## BIC =  85.9
    ## Fit based upon off diagonal values = 0.99
    ## Measures of factor score adequacy             
    ##                                                    ML1  ML2
    ## Correlation of (regression) scores with factors   1.00 0.99
    ## Multiple R square of scores with factors          1.00 0.98
    ## Minimum correlation of possible factor scores     0.99 0.96

``` r
fa.diagram(Model1)
```

![](FactorAnalysis-j0nsonBook-season9_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
Model2
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = dat_scale, nfactors = 3, rotate = "none", residuals = TRUE, 
    ##     fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##         ML1   ML3   ML2   h2    u2 com
    ## slsgrw 0.90  0.38 -0.07 0.96 0.039 1.4
    ## slsprf 0.78  0.60  0.07 0.97 0.034 1.9
    ## nw-ccs 0.93  0.20  0.06 0.91 0.088 1.1
    ## crtvtt 0.73 -0.12  0.67 1.00 0.005 2.0
    ## mchnrt 0.69  0.22  0.17 0.55 0.447 1.3
    ## abstrt 0.76 -0.13 -0.64 1.00 0.005 2.0
    ## mthmtt 0.76  0.61 -0.11 0.96 0.038 2.0
    ## 
    ##                        ML1  ML3  ML2
    ## SS loadings           4.44 1.00 0.90
    ## Proportion Var        0.63 0.14 0.13
    ## Cumulative Var        0.63 0.78 0.91
    ## Proportion Explained  0.70 0.16 0.14
    ## Cumulative Proportion 0.70 0.86 1.00
    ## 
    ## Mean item complexity =  1.7
    ## Test of the hypothesis that 3 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  21  and the objective function was  10.9 with Chi Square of  499.66
    ## The degrees of freedom for the model are 3  and the objective function was  1.42 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.03 
    ## The df corrected root mean square of the residuals is  0.07 
    ## 
    ## The harmonic number of observations is  50 with the empirical chi square  1.43  with prob <  0.7 
    ## The total number of observations was  50  with Likelihood Chi Square =  62.18  with prob <  2e-13 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.093
    ## RMSEA index =  0.628  and the 90 % confidence intervals are  0.503 0.776
    ## BIC =  50.44
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                    ML1  ML3  ML2
    ## Correlation of (regression) scores with factors   1.00 0.98 1.00
    ## Multiple R square of scores with factors          1.00 0.97 0.99
    ## Minimum correlation of possible factor scores     0.99 0.94 0.99

``` r
fa.diagram(Model2)
```

![](FactorAnalysis-j0nsonBook-season9_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

## section b ———————————–

``` r
Model1_rotating <- fa(dat_scale, nfactors = 2, rotate = "oblimin", 
fm = "ml")
Model2_rotating <- fa(dat_scale, nfactors = 3, rotate = "oblimin", 
fm = "ml")

Model1_rotating
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = dat_scale, nfactors = 2, rotate = "oblimin", fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##         ML2   ML1   h2    u2 com
    ## slsgrw 0.91  0.10 0.93 0.069 1.0
    ## slsprf 0.94  0.05 0.93 0.070 1.0
    ## nw-ccs 0.74  0.32 0.88 0.123 1.4
    ## crtvtt 0.04  0.98 1.00 0.005 1.0
    ## mchnrt 0.50  0.33 0.53 0.474 1.7
    ## abstrt 0.70 -0.21 0.39 0.614 1.2
    ## mthmtt 1.04 -0.13 0.97 0.029 1.0
    ## 
    ##                        ML2  ML1
    ## SS loadings           4.23 1.39
    ## Proportion Var        0.60 0.20
    ## Cumulative Var        0.60 0.80
    ## Proportion Explained  0.75 0.25
    ## Cumulative Proportion 0.75 1.00
    ## 
    ##  With factor correlations of 
    ##     ML2 ML1
    ## ML2 1.0 0.5
    ## ML1 0.5 1.0
    ## 
    ## Mean item complexity =  1.2
    ## Test of the hypothesis that 2 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  21  and the objective function was  10.9 with Chi Square of  499.66
    ## The degrees of freedom for the model are 8  and the objective function was  2.63 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.06 
    ## The df corrected root mean square of the residuals is  0.09 
    ## 
    ## The harmonic number of observations is  50 with the empirical chi square  6.79  with prob <  0.56 
    ## The total number of observations was  50  with Likelihood Chi Square =  117.2  with prob <  1.3e-21 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.382
    ## RMSEA index =  0.522  and the 90 % confidence intervals are  0.446 0.614
    ## BIC =  85.9
    ## Fit based upon off diagonal values = 0.99
    ## Measures of factor score adequacy             
    ##                                                    ML2  ML1
    ## Correlation of (regression) scores with factors   0.99 1.00
    ## Multiple R square of scores with factors          0.99 0.99
    ## Minimum correlation of possible factor scores     0.97 0.99

``` r
fa.diagram(Model1_rotating)
```

![](FactorAnalysis-j0nsonBook-season9_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
Model2_rotating
```

    ## Factor Analysis using method =  ml
    ## Call: fa(r = dat_scale, nfactors = 3, rotate = "oblimin", fm = "ml")
    ## Standardized loadings (pattern matrix) based upon correlation matrix
    ##          ML1   ML2   ML3   h2    u2 com
    ## slsgrw  0.74  0.15  0.24 0.96 0.039 1.3
    ## slsprf  1.01  0.04 -0.10 0.97 0.034 1.0
    ## nw-ccs  0.49  0.40  0.30 0.91 0.088 2.6
    ## crtvtt -0.01  1.01 -0.04 1.00 0.005 1.0
    ## mchnrt  0.47  0.34  0.06 0.55 0.447 1.9
    ## abstrt -0.01 -0.04  1.01 1.00 0.005 1.0
    ## mthmtt  1.02 -0.12  0.02 0.96 0.038 1.0
    ## 
    ##                        ML1  ML2  ML3
    ## SS loadings           3.43 1.56 1.35
    ## Proportion Var        0.49 0.22 0.19
    ## Cumulative Var        0.49 0.71 0.91
    ## Proportion Explained  0.54 0.25 0.21
    ## Cumulative Proportion 0.54 0.79 1.00
    ## 
    ##  With factor correlations of 
    ##      ML1  ML2  ML3
    ## ML1 1.00 0.54 0.57
    ## ML2 0.54 1.00 0.23
    ## ML3 0.57 0.23 1.00
    ## 
    ## Mean item complexity =  1.4
    ## Test of the hypothesis that 3 factors are sufficient.
    ## 
    ## The degrees of freedom for the null model are  21  and the objective function was  10.9 with Chi Square of  499.66
    ## The degrees of freedom for the model are 3  and the objective function was  1.42 
    ## 
    ## The root mean square of the residuals (RMSR) is  0.03 
    ## The df corrected root mean square of the residuals is  0.07 
    ## 
    ## The harmonic number of observations is  50 with the empirical chi square  1.43  with prob <  0.7 
    ## The total number of observations was  50  with Likelihood Chi Square =  62.18  with prob <  2e-13 
    ## 
    ## Tucker Lewis Index of factoring reliability =  0.093
    ## RMSEA index =  0.628  and the 90 % confidence intervals are  0.503 0.776
    ## BIC =  50.44
    ## Fit based upon off diagonal values = 1
    ## Measures of factor score adequacy             
    ##                                                    ML1  ML2  ML3
    ## Correlation of (regression) scores with factors   0.99 1.00 1.00
    ## Multiple R square of scores with factors          0.99 1.00 1.00
    ## Minimum correlation of possible factor scores     0.97 0.99 0.99

``` r
fa.diagram(Model2_rotating)
```

![](FactorAnalysis-j0nsonBook-season9_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

## section c ———————————-

``` r
variance_khas_1 <- Model1_rotating$uniquenesses 
variance_khas
```

    ##          [,1]     [,2]     [,3]
    ## [1,] 0.000106 0.000000 0.000000
    ## [2,] 0.000000 0.000613 0.000000
    ## [3,] 0.000000 0.000000 0.000661

``` r
variance_khas_2 <- Model2_rotating$uniquenesses
variance_khas_2
```

    ##      slsgrw      slsprf      nw-ccs      crtvtt      mchnrt      abstrt 
    ## 0.038571357 0.034481797 0.088124265 0.004956641 0.446619476 0.004968310 
    ##      mthmtt 
    ## 0.037508685

``` r
loading_1 <- Model1_rotating$loadings[]
loading_2 <- Model2_rotating$loadings[]
loading_1
```

    ##               ML2         ML1
    ## slsgrw 0.91278369  0.09751918
    ## slsprf 0.93592533  0.05468613
    ## nw-ccs 0.73836890  0.31647820
    ## crtvtt 0.03532516  0.97954451
    ## mchnrt 0.50134240  0.33205259
    ## abstrt 0.69899882 -0.21377863
    ## mthmtt 1.04358718 -0.13062231

``` r
loading_2
```

    ##                 ML1         ML2         ML3
    ## slsgrw  0.736331948  0.15070582  0.24005295
    ## slsprf  1.013070552  0.04223324 -0.10116802
    ## nw-ccs  0.489008127  0.39547103  0.29599376
    ## crtvtt -0.014367664  1.01330840 -0.03950114
    ## mchnrt  0.465271163  0.34261625  0.06235652
    ## abstrt -0.005322588 -0.03718594  1.00835458
    ## mthmtt  1.024569806 -0.11630124  0.02473713

``` r
variance_eshteraki_1 <- loading_1 %*% t(loading_1) 
h21 <- variance_eshteraki_1 %>% diag

variance_eshteraki_2 <- loading_2 %*% t(loading_2) 
h22 <- variance_eshteraki_2 %>% diag
h22
```

    ##    slsgrw    slsprf    nw-ccs    crtvtt    mchnrt    abstrt    mthmtt 
    ## 0.6225224 1.0383306 0.4831386 1.0285607 0.3377515 1.0181901 1.0638812

``` r
psi_1 <- diag(variance_khas_1, 7)
psi_2 <- diag(variance_khas_2, 7)

shat1 <- variance_eshteraki_1 + psi_1
shat1
```

    ##           slsgrw     slsprf    nw-ccs      crtvtt    mchnrt     abstrt
    ## slsgrw 0.9118756 0.85963032 0.7048338  0.12776861 0.4899987  0.6171872
    ## slsprf 0.8596303 0.94932931 0.7083651  0.08662921 0.4873777  0.6425200
    ## nw-ccs 0.7048338 0.70836512 0.7686574  0.33608749 0.4752630  0.4484627
    ## crtvtt 0.1277686 0.08662921 0.3360875  0.96574320 0.3429703 -0.1847134
    ## mchnrt 0.4899987 0.48737772 0.4752630  0.34297029 0.8351915  0.2794520
    ## abstrt 0.6171872 0.64251997 0.4484627 -0.18471344 0.2794520  1.1479378
    ## mthmtt 0.9398312 0.96957644 0.7292132 -0.09108548 0.4798210  0.7573905
    ##             mthmtt
    ## slsgrw  0.93983117
    ## slsprf  0.96957644
    ## nw-ccs  0.72921320
    ## crtvtt -0.09108548
    ## mchnrt  0.47982102
    ## abstrt  0.75739046
    ## mthmtt  1.13495361

``` r
shat2 <- variance_eshteraki_2 + psi_2
shat2
```

    ##           slsgrw      slsprf    nw-ccs      crtvtt     mchnrt      abstrt
    ## slsgrw 0.6610938  0.72803533 0.4907263  0.13264974 0.40919715  0.23253517
    ## slsprf 0.7280353  1.07281236 0.4821567  0.03223609 0.47951382 -0.10897588
    ## nw-ccs 0.4907263  0.48215665 0.5712629  0.38201612 0.38147333  0.28115791
    ## crtvtt 0.1326497  0.03223609 0.3820161  1.03351731 0.33802791 -0.07743551
    ## mchnrt 0.4091972  0.47951382 0.3814733  0.33802791 0.78437096  0.04766053
    ## abstrt 0.2325352 -0.10897588 0.2811579 -0.07743551 0.04766053  1.02315840
    ## mthmtt 0.7428344  1.03054711 0.4623512 -0.13354685 0.43839861  0.02381521
    ##             mthmtt
    ## slsgrw  0.74283443
    ## slsprf  1.03054711
    ## nw-ccs  0.46235123
    ## crtvtt -0.13354685
    ## mchnrt  0.43839861
    ## abstrt  0.02381521
    ## mthmtt  1.10138988

``` r
diff1 <- sum(abs(Sn - shat1))
diff2 <- sum(abs(Sn - shat2))
diff1
```

    ## [1] 9.165762

``` r
diff2
```

    ## [1] 14.99471

## section d—————————————–

### bartlette test

``` r
p <- ncol(dat_scale)
m1 <- 2; m2 = 3
c1 <- (n-1-(2*p + 4*m1 + 5)/6) * log(det(shat1)/det(Sn))
c2 <- (n-1-(2*p + 4*m2 + 5)/6) * log(det(shat2)/det(Sn))
df1 <- ((p-m1)^2-p-m1)/2
df2 <- ((p-m2)^2-p-m2)/2
c1
```

    ## [1] 135.7996

``` r
c2
```

    ## [1] 101.3708

``` r
chi1 <- qchisq(.99, df = df1)
chi2 <- qchisq(.99, df = df2)
ifel(c1 > chi1, "H0 rejected", "H0 accepted")
```

    ## [1] "H0 rejected"

``` r
ifel(c2 > chi2, "H0 rejected", "H0 accepted")
```

    ## [1] "H0 rejected"

## section e ————————————

``` r
sii <- apply(dat2, 2, sd)
xbar <- apply(dat2, 2, mean)
list_loading <- list(loading_1, loading_2)
list_psi <- list(psi_1, psi_2)
get_score <- function(x, method = "weighted", index_model = 1){
    z <- (x - xbar)/sii
    L <- list_loading[[index_model]]
    psi <- list_psi[[index_model]]
    ind <- ifel(index_model == 1, c(1, 2), c(1, 2, 3))
f1 <- function(z){
    A1 <- (t(L) %*% solve(psi) %*% L) %>% solve
    A2 <- t(L) %*% solve(psi) %*% z
    fhat <- A1 %*% A2 %>% as.numeric %>% 
    setNames(paste("fhat-weighted", ind, sep = ":"))
    return(fhat)
}
f2 <- function(z){
    fhat <- t(L) %*% solve(S) %*% z %>% as.numeric %>% 
    setNames(paste("fhat-regression", ind, sep = ":"))
    return(fhat)
}
fhat <- switch(method, 
"weighted" = f1(z), 
f2(z))
return(fhat)
}
x <- c(110, 98, 105, 15, 18, 12, 35)

get_score(x = x, method = "weighted", index_model = 1)
```

    ## fhat-weighted:1 fhat-weighted:2 
    ##       0.4733469       0.9572682

``` r
get_score(x = x, method = "regression", index_model = 1)
```

    ## fhat-regression:1 fhat-regression:2 
    ##       0.003095139       0.950810773

``` r
get_score(x = x, method = "weighted", index_model = 2)
```

    ## fhat-weighted:1 fhat-weighted:2 fhat-weighted:3 
    ##       0.2000672       0.9830470       0.7491498

``` r
get_score(x = x, method = "regression", index_model = 2)
```

    ## fhat-regression:1 fhat-regression:2 fhat-regression:3 
    ##         -1.126675          1.338112          1.080083
