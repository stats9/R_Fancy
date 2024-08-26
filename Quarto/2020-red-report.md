# How to create parameterized report


- [read data](#read-data)
- [extract year from datatime
  column](#extract-year-from-datatime-column)
- [filter data by year in params](#filter-data-by-year-in-params)
- [visualize data by color in
  params](#visualize-data-by-color-in-params)
- [save figures](#save-figures)

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

# filter data by year in params

``` r
filtered_data <- dat_update |> 
                dplyr :: filter (col_year == params$year) 

filtered_data |> head()
```

    # A tibble: 6 × 4
      col_year year                temperature rainFall
         <dbl> <dttm>                    <dbl>    <dbl>
    1     2020 2020-01-06 00:00:00          61      106
    2     2020 2020-01-13 00:00:00          54      119
    3     2020 2020-01-20 00:00:00          63       88
    4     2020 2020-01-27 00:00:00          57       85
    5     2020 2020-02-03 00:00:00          57       93
    6     2020 2020-02-10 00:00:00          59      107

# visualize data by color in params

``` r
P <- filtered_data |> 
        ggplot(aes(x = temperature, y = rainFall)) + 
        geom_point(color = params$color, size = 3, shape = 16) + 
        ggtitle(paste0("year: ", params$year)) + 
        theme_bw() 
```

# save figures

``` r
temp1 <- paste(paste(params$year, params$color, sep = "-"), "png", sep = ".")
temp2 <- paste("Figures", temp1, sep = "//")
png(filename = temp2, 
    width = 7, height = 7, units = "in", , res = 300)
    P 
dev.off()
```

    png 
      2 

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

![](Figures//2020-red.png) scatter of Temperature Vs RainFall in year:
2020 with color: red
