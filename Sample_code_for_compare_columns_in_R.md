Habib Ezatabadi Sample Code
================

## sample code

#### (a) Get khesarat ——————–

``` r
set.seed(1)
library(tidyverse)

n <- 1000
National_number <- sample(1:20, size = n, replace = TRUE)
khedmat <- sample(1:6, size = n, replace = TRUE)
sex <- sample(c("female", "male"), size = n, replace = TRUE, 
prob = c(.4, .6))
FR_CR <- sample(c("Ten", "Twenty"), size = n, replace = TRUE)

dat <- tibble(National_number, khedmat, sex, FR_CR) %>%
                mutate(khesarat = case_when(
                    khedmat == 1 ~ 1000, 
                    khedmat == 2 ~ 2000, 
                    khedmat == 3 ~ 3000, 
                    khedmat == 4 ~ 4000,
                    khedmat == 5 ~ 5000, 
                    TRUE ~ 6000 
                )) %>% mutate(across(c(2, 3, 4), as.factor))



head(dat)
```

    # A tibble: 6 × 5
      National_number khedmat sex    FR_CR  khesarat
                <int> <fct>   <fct>  <fct>     <dbl>
    1               4 4       male   Twenty     4000
    2               7 5       male   Twenty     5000
    3               1 4       female Twenty     4000
    4               2 2       male   Ten        2000
    5              11 1       female Ten        1000
    6              14 3       male   Twenty     3000

``` r
get_khesarat <- function(x) {    # x is National_number
    if (! (x %in% dat$National_number)) {
        stop ("this value is not in data set")
    }
    result <- dat %>%
    filter (National_number == x) %>%
    group_by (khedmat) %>%
    summarise(Khesarat_kol = sum(khesarat))
    return (result)
}

get_khesarat(2)
```

    # A tibble: 6 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1               6000
    2 2              22000
    3 3              21000
    4 4              32000
    5 5              45000
    6 6              48000

## quantile regression

``` r
dat2 <- dat %>% 
        group_by(khedmat, FR_CR, sex) %>%
            summarise(TOT_khesarat = sum(khesarat))
library(quantreg)

## q = 0.5 

Median_Model  <- rq(TOT_khesarat ~ ., data = dat2, tau = 0.5)

summary(Median_Model)
```


    Call: rq(formula = TOT_khesarat ~ ., tau = 0.5, data = dat2)

    tau: [1] 0.5

    Coefficients:
                coefficients lower bd   upper bd  
    (Intercept)   2000.000   -64527.103  20431.266
    khedmat2     42000.000   -28566.733 132823.990
    khedmat3     72000.000    25557.057 134283.367
    khedmat4    102000.000    82605.898 193097.681
    khedmat5    153000.000    93915.374 256478.728
    khedmat6    192000.000   163557.057 255986.945
    FR_CRTwenty  36000.000    -4553.048  61414.786
    sexmale      64000.000    41170.428  88244.359

``` r
## q = 0.75

Model2  <- rq(TOT_khesarat ~ ., data = dat2, tau = 0.75)

summary(Model2)
```


    Call: rq(formula = TOT_khesarat ~ ., tau = 0.75, data = dat2)

    tau: [1] 0.75

    Coefficients:
                coefficients   lower bd       upper bd      
    (Intercept)   3.800000e+04  -4.130941e+04  1.797693e+308
    khedmat2      4.200000e+04 -1.797693e+308  1.797693e+308
    khedmat3      4.900000e+04 -1.797693e+308  1.797693e+308
    khedmat4      1.020000e+05 -1.797693e+308  1.797693e+308
    khedmat5      1.170000e+05 -1.797693e+308  1.797693e+308
    khedmat6      1.780000e+05 -1.797693e+308  1.797693e+308
    FR_CRTwenty   0.000000e+00  -1.703579e+03   8.785179e+04
    sexmale       8.700000e+04   1.511243e+04   1.130358e+05
