# template code


- [read data](#read-data)
- [extract year from datatime
  column](#extract-year-from-datatime-column)
- [filter data](#filter-data)
- [visualize](#visualize)
- [save figures](#save-figures)
- [define caption](#define-caption)

# read data

``` r
library(tidyverse)
dat <- readxl :: read_xlsx("sim_dat.xlsx")

dat |> head()
```

    # A tibble: 6 × 3
      year                temperature rainFall
      <dttm>                    <dbl>    <dbl>
    1 1990-01-01 00:00:00          52      105
    2 1990-01-08 00:00:00          58       99
    3 1990-01-15 00:00:00          61      107
    4 1990-01-22 00:00:00          48       96
    5 1990-01-29 00:00:00          59      102
    6 1990-02-05 00:00:00          59       96

# extract year from datatime column

``` r
dat_update <- dat |> 
        mutate(col_year = lubridate :: year(year)) |> 
        relocate('col_year', .before = 'year') 
dat_update |> head()
```

    # A tibble: 6 × 4
      col_year year                temperature rainFall
         <dbl> <dttm>                    <dbl>    <dbl>
    1     1990 1990-01-01 00:00:00          52      105
    2     1990 1990-01-08 00:00:00          58       99
    3     1990 1990-01-15 00:00:00          61      107
    4     1990 1990-01-22 00:00:00          48       96
    5     1990 1990-01-29 00:00:00          59      102
    6     1990 1990-02-05 00:00:00          59       96

# filter data

``` r
filter_data <- dat_update |> 
    dplyr :: filter (col_year == params$year)

filter_data |> head() 
```

    # A tibble: 6 × 4
      col_year year                temperature rainFall
         <dbl> <dttm>                    <dbl>    <dbl>
    1     2019 2019-01-07 00:00:00          58      105
    2     2019 2019-01-14 00:00:00          51      102
    3     2019 2019-01-21 00:00:00          69      100
    4     2019 2019-01-28 00:00:00          61      103
    5     2019 2019-02-04 00:00:00          62       97
    6     2019 2019-02-11 00:00:00          49      100

year: 2019

# visualize

``` r
P <- filter_data |> 
        ggplot(aes(x = temperature, y = rainFall)) + 
        geom_point(color = params$color, size = 4, shape = 19) + 
        ggtitle(paste0("year: ", params$year)) + 
        theme_bw() 
```

# save figures

``` r
temp1 <- paste(paste(params$year, params$color, sep = "-"), "png", sep = ".")
temp2 <- paste("Figures", temp1, sep = "//")


png(filename = temp2, width = 7, height = 7, units = "in", res = 300)
    P 
dev.off()
```

# define caption

``` r
capp <- sprintf("scatter of Temperature Vs RainFall in year: %d with color %s", params$year, params$color)
```

![](Figures//2019-orange.png) scatter of Temperature Vs RainFall in
year: 2019 with color orange
