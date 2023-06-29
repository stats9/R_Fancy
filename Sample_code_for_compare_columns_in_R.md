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

------------------------------------------------------------------------

------------------------------------------------------------------------

## Update code for new data set ———————

## import data and get summary

## Update new:1 ——————

``` r
library(tidyverse)
library(readxl)
dat <- read_xlsx(file.choose(), col_names = TRUE)
head(dat)
```

    # A tibble: 6 × 6
      National_number   sex   age Khesarat khedmat FR_CR
                <dbl> <dbl> <dbl>    <dbl> <chr>   <dbl>
    1        10023429     0    34  6165000 8          10
    2        10023429     0    34  1500000 1          10
    3        10023429     0    34  1500000 1          10
    4        10023429     0    34  2000000 1          10
    5        10026185     0    35   934000 2          20
    6        10026185     0    35   660000 1          20

``` r
tail(dat)
```

    # A tibble: 6 × 6
      National_number   sex   age Khesarat khedmat FR_CR
                <dbl> <dbl> <dbl>    <dbl> <chr>   <dbl>
    1      7750096744     1     4   457800 1          10
    2      7750096744     1     4   837000 1          10
    3      7750181555     1     1 12327120 8          10
    4      7900028625     0     3  4308680 8          10
    5      7900028625     0     3  1746300 8          10
    6      7990008963     1     1  3270000 6          10

``` r
dim(dat)
```

    [1] 173951      6

``` r
names(dat)
```

    [1] "National_number" "sex"             "age"             "Khesarat"       
    [5] "khedmat"         "FR_CR"          

``` r
glimpse(dat)
```

    Rows: 173,951
    Columns: 6
    $ National_number <dbl> 10023429, 10023429, 10023429, 10023429, 10026185, 1002…
    $ sex             <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, …
    $ age             <dbl> 34, 34, 34, 34, 35, 35, 35, 35, 35, 35, 35, 35, 35, 35…
    $ Khesarat        <dbl> 6165000, 1500000, 1500000, 2000000, 934000, 660000, 17…
    $ khedmat         <chr> "8", "1", "1", "1", "2", "1", "8", "1", "1", "8", "1",…
    $ FR_CR           <dbl> 10, 10, 10, 10, 20, 20, 20, 20, 20, 20, 20, 20, 20, 20…

``` r
if (! require(summarytools)) {
    install.packages ("summarytools")
    library (summarytools)
}

dfSummary(dat)
```

    Data Frame Summary  
    dat  
    Dimensions: 173951 x 6  
    Duplicates: 35483  

    -----------------------------------------------------------------------------------------------------------------------------
    No   Variable          Stats / Values                        Freqs (% of Valid)      Graph               Valid      Missing  
    ---- ----------------- ------------------------------------- ----------------------- ------------------- ---------- ---------
    1    National_number   Mean (sd) : 2159230922 (1759160466)   13743 distinct values   :                   173951     0        
         [numeric]         min < med < max:                                              : : .               (100.0%)   (0.0%)   
                           10023429 < 1881839982 < 7990008963                            : : :     .                             
                           IQR (CV) : 2880548452 (0.8)                                   : : : :   :                             
                                                                                         : : : : : : : .                         

    2    sex               Min  : 0                              0 : 132056 (75.9%)      IIIIIIIIIIIIIII     173951     0        
         [numeric]         Mean : 0.2                            1 :  41895 (24.1%)      IIII                (100.0%)   (0.0%)   
                           Max  : 1                                                                                              

    3    age               Mean (sd) : 35.7 (11.3)               73 distinct values              :           173951     0        
         [numeric]         min < med < max:                                                      :           (100.0%)   (0.0%)   
                           0 < 35 < 72                                                           : :                             
                           IQR (CV) : 11 (0.3)                                                 : : :                             
                                                                                         .   . : : : : . .                       

    4    Khesarat          Mean (sd) : 5238283 (31938428)        51414 distinct values   :                   173951     0        
         [numeric]         min < med < max:                                              :                   (100.0%)   (0.0%)   
                           100 < 1285930 < 2555466255                                    :                                       
                           IQR (CV) : 2170000 (6.1)                                      :                                       
                                                                                         :                                       

    5    khedmat           1. 1                                  106174 (61.0%)          IIIIIIIIIIII        173951     0        
         [character]       2. 8                                   29882 (17.2%)          III                 (100.0%)   (0.0%)   
                           3. 2                                   24811 (14.3%)          II                                      
                           4. 7                                    4864 ( 2.8%)                                                  
                           5. 6                                    2752 ( 1.6%)                                                  
                           6. 3                                    2285 ( 1.3%)                                                  
                           7. 5                                    2241 ( 1.3%)                                                  
                           8. زايمان                                566 ( 0.3%)                                                  
                           9. هزينه هاي درمان                       224 ( 0.1%)                                                  
                           نازایي و                                  87 ( 0.1%)                                                  
                           10. 4                                     65 ( 0.0%)                                                  
                           [ 5 others ]                                                                                          

    6    FR_CR             Mean (sd) : 15.4 (5)                  10 : 80019 (46.0%)      IIIIIIIII           173951     0        
         [numeric]         min < med < max:                      20 : 93869 (54.0%)      IIIIIIIIII          (100.0%)   (0.0%)   
                           10 < 20 < 30                          30 :    63 ( 0.0%)                                              
                           IQR (CV) : 10 (0.3)                                                                                   
    -----------------------------------------------------------------------------------------------------------------------------

``` r
dat$khedmat %>% table
```

    .
                                    1                                 2 
                               106174                             24811 
                                    3                                 4 
                                 2285                                87 
                                    5                                 6 
                                 2241                              2752 
                                    7                                 8 
                                 4864                             29882 
                             آمبولانس                    خدمات توانبخشي 
                                   12                                 3 
                               زايمان                              سمعک 
                                  566                                 9 
                            لنز ،عينک   هزينه رفع عيوب انکساري ديددوچشم 
                                    2                                39 
    هزينه هاي درمان نازایي و ناباروري 
                                  224 

``` r
dat2 <- dat %>%
    filter(khedmat %in% as.character(1:8))


dat2 %>% .$khedmat %>% table
```

    .
         1      2      3      4      5      6      7      8 
    106174  24811   2285     87   2241   2752   4864  29882 

``` r
dat2 <- dat2 %>%
mutate(across(c(2, 5, 6), factor)) 
```

------------------------------------------------------------------------

``` r
get_khesarat <- function(x) {# x is National_number or vector of national number
    if (length(x) == 1) {
        if (! (x %in% dat2$National_number)) {
            stop ("this value is not in data set")
        } else {
            result <- dat2 %>%
            filter (National_number == x) %>%
            group_by (khedmat) %>%
            summarise(Khesarat_kol = sum(Khesarat))
        }
    } else {
        result <- list()
        for (i in x) {
            if (! (i %in% dat2$National_number)) {
            result[[as.character(i)]] = "this value is not in data set"
        } else {
            result[[as.character(i)]] <- dat2 %>%
            filter (National_number == i) %>%
            group_by (khedmat) %>%
            summarise(Khesarat_kol = sum(Khesarat))
            }
        }
    }
    return (result)
}

id_sample <- sample(unique(dat2$National_number), 10)
get_khesarat(id_sample)
```

    $`45826080`
    # A tibble: 4 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1            3325000
    2 2           29182499
    3 3            9480240
    4 8            7203970

    $`520295153`
    # A tibble: 2 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1           24917500
    2 8            9338890

    $`70892067`
    # A tibble: 3 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1            6163800
    2 2           10000000
    3 8            5680000

    $`941788601`
    # A tibble: 4 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1           21238100
    2 2           13800053
    3 4          366761658
    4 8             517000

    $`521291488`
    # A tibble: 2 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1            1040000
    2 8             850000

    $`33920461`
    # A tibble: 5 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1           34270040
    2 2           42330460
    3 3            3708500
    4 6            1500000
    5 8           23300000

    $`77991044`
    # A tibble: 2 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1            3005000
    2 3            7600000

    $`1229948236`
    # A tibble: 1 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1             303000

    $`6199751957`
    # A tibble: 4 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1            6417000
    2 6            2150000
    3 7           17000000
    4 8             333000

    $`2700197666`
    # A tibble: 3 × 2
      khedmat Khesarat_kol
      <fct>          <dbl>
    1 1            4151760
    2 2            8070000
    3 8            2124370

------------------------------------------------------------------------

------------------------------------------------------------------------

## Quantile Regression —————————-

``` r
dat3 <- dat2 %>% 
        group_by(khedmat, FR_CR, sex) %>%
            summarise(TOT_khesarat = sum(Khesarat))
library(quantreg)

## q = 0.5 

Median_Model  <- rq(TOT_khesarat ~ ., data = dat3, tau = 0.5)

summary(Median_Model)
```


    Call: rq(formula = TOT_khesarat ~ ., tau = 0.5, data = dat3)

    tau: [1] 0.5

    Coefficients:
                coefficients   lower bd       upper bd      
    (Intercept)   3.117373e+10   1.844930e+10   6.038150e+10
    khedmat2     -8.822445e+08  -2.152645e+10   1.795078e+10
    khedmat3     -2.868721e+10  -5.941248e+10  -1.254772e+10
    khedmat4     -1.233416e+10 -1.797693e+308  -8.255265e+09
    khedmat5      3.910237e+10   1.239452e+10   5.613935e+10
    khedmat6     -1.903196e+10  -5.244799e+10  -1.267098e+10
    khedmat7     -4.997239e+08  -2.595557e+10   8.054648e+09
    khedmat8     -8.784698e+08  -2.607564e+10   2.563111e+10
    FR_CR20       6.394990e+09  -6.774190e+09   1.564976e+10
    FR_CR30      -3.028251e+10  -3.630485e+10  -2.222082e+10
    sex1         -1.534731e+10  -2.178602e+10  -2.139897e+09

``` r
## q = 0.75

Model2  <- rq(TOT_khesarat ~ ., data = dat3, tau = 0.75)

summary(Model2)
```


    Call: rq(formula = TOT_khesarat ~ ., tau = 0.75, data = dat3)

    tau: [1] 0.75

    Coefficients:
                coefficients   lower bd       upper bd      
    (Intercept)   5.543332e+10   2.893180e+10  1.797693e+308
    khedmat2     -1.987207e+10  -3.004075e+10   7.226527e+09
    khedmat3     -3.539000e+10  -5.843722e+10  1.797693e+308
    khedmat4     -2.945712e+10  -5.231140e+10  1.797693e+308
    khedmat5      1.784049e+10   8.723702e+09  1.797693e+308
    khedmat6     -4.036927e+10  -5.632266e+10  1.797693e+308
    khedmat7     -2.183703e+10  -2.747188e+10   3.075638e+10
    khedmat8     -7.032829e+09  -3.099233e+10   3.446260e+10
    FR_CR20       6.470416e+09   7.748951e+08   1.473200e+10
    FR_CR30      -3.320479e+10  -4.605784e+10  1.797693e+308
    sex1         -1.834502e+10  -3.511203e+10  -8.687505e+08

------------------------------------------------------------------------

------------------------------------------------------------------------

## for get more details —————-

``` r
dat3 %>%
ggplot(aes(y = TOT_khesarat, group = sex, fill = sex)) + 
geom_boxplot() + 
theme_bw()
```

<img
src="Sample_code_for_compare_columns_in_R_files/figure-commonmark/unnamed-chunk-8-1.png"
data-fig-align="center" />

------------------------------------------------------------------------

``` r
dat3 %>%
ggplot(aes(y = TOT_khesarat, group = FR_CR, fill = FR_CR)) + 
geom_boxplot() + 
theme_bw()
```

<img
src="Sample_code_for_compare_columns_in_R_files/figure-commonmark/unnamed-chunk-9-1.png"
data-fig-align="center" />

------------------------------------------------------------------------

``` r
dat3 %>%
ggplot(aes(y = TOT_khesarat, group = khedmat, fill = khedmat)) + 
geom_boxplot() + 
theme_bw()
```

<img
src="Sample_code_for_compare_columns_in_R_files/figure-commonmark/unnamed-chunk-10-1.png"
data-fig-align="center" />
