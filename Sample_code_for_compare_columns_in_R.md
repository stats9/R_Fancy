Habib Ezatabadi Sample Code
================

## sample code

``` r
library(tidyverse)
n <- 1000
code_meli <- sample(1:20, size = n, replace = TRUE)
khedmat <- sample(1:6, size = n, replace = TRUE)
dat <- tibble(code_meli, khedmat) %>%
                mutate(khesarat = case_when(
                    khedmat == 1 ~ 1000, 
                    khedmat == 2 ~ 2000, 
                    khedmat == 3 ~ 3000, 
                    khedmat == 4 ~ 4000,
                    khedmat == 5 ~ 5000, 
                    TRUE ~ 6000 
                ))

head(dat)
```

    # A tibble: 6 × 3
      code_meli khedmat khesarat
          <int>   <int>    <dbl>
    1        20       2     2000
    2         6       1     1000
    3        11       6     6000
    4         9       5     5000
    5         1       3     3000
    6        10       3     3000

``` r
get_khesarat <- function(x) {    # x is code melli 
    if (! (x %in% dat$code_meli)) {
        stop ("this value is not in data set")
    }
    result <- dat %>%
    filter (code_meli == x) %>%
    group_by (khedmat) %>%
    summarise(Khesarat_kol = sum(khesarat))
    return (result)
}

get_khesarat(2)
```

    # A tibble: 6 × 2
      khedmat Khesarat_kol
        <int>        <dbl>
    1       1         9000
    2       2        22000
    3       3        36000
    4       4        36000
    5       5        15000
    6       6        36000
