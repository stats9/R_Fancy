# Polars package examples


- [install package](#install-package)
- [get start](#get-start)
- [groupby and aggregate](#groupby-and-aggregate)
- [mutate](#mutate)
- [inexing](#inexing)
- [generate a sample data](#generate-a-sample-data)
- [compare with data.table](#compare-with-datatable)



## install package

``` r
Sys.setenv(NOT_CRAN = "true")
install.packages("polars", repos = "https://community.r-multiverse.org")
```

## get start

``` r
library(polars)
df <- pl$DataFrame(
    x = 1:5,
    y = 10:14,
    z = 5:1
)
df
```

    shape: (5, 3)
    ┌─────┬─────┬─────┐
    │ x   ┆ y   ┆ z   │
    │ --- ┆ --- ┆ --- │
    │ i32 ┆ i32 ┆ i32 │
    ╞═════╪═════╪═════╡
    │ 1   ┆ 10  ┆ 5   │
    │ 2   ┆ 11  ┆ 4   │
    │ 3   ┆ 12  ┆ 3   │
    │ 4   ┆ 13  ┆ 2   │
    │ 5   ┆ 14  ┆ 1   │
    └─────┴─────┴─────┘

``` r
df$mean()
```

    shape: (1, 3)
    ┌─────┬──────┬─────┐
    │ x   ┆ y    ┆ z   │
    │ --- ┆ ---  ┆ --- │
    │ f64 ┆ f64  ┆ f64 │
    ╞═════╪══════╪═════╡
    │ 3.0 ┆ 12.0 ┆ 3.0 │
    └─────┴──────┴─────┘

``` r
df$sum()
```

    shape: (1, 3)
    ┌─────┬─────┬─────┐
    │ x   ┆ y   ┆ z   │
    │ --- ┆ --- ┆ --- │
    │ i32 ┆ i32 ┆ i32 │
    ╞═════╪═════╪═════╡
    │ 15  ┆ 60  ┆ 15  │
    └─────┴─────┴─────┘

``` r
df$select('x', 'y')$mean()
```

    shape: (1, 2)
    ┌─────┬──────┐
    │ x   ┆ y    │
    │ --- ┆ ---  │
    │ f64 ┆ f64  │
    ╞═════╪══════╡
    │ 3.0 ┆ 12.0 │
    └─────┴──────┘

``` r
# help(package = 'polars')

## select 
df$ 
select('z')$
mean()
```

    shape: (1, 1)
    ┌─────┐
    │ z   │
    │ --- │
    │ f64 │
    ╞═════╡
    │ 3.0 │
    └─────┘

``` r
## filter
df$
filter(pl$col('x') > 2)
```

    shape: (3, 3)
    ┌─────┬─────┬─────┐
    │ x   ┆ y   ┆ z   │
    │ --- ┆ --- ┆ --- │
    │ i32 ┆ i32 ┆ i32 │
    ╞═════╪═════╪═════╡
    │ 3   ┆ 12  ┆ 3   │
    │ 4   ┆ 13  ┆ 2   │
    │ 5   ┆ 14  ┆ 1   │
    └─────┴─────┴─────┘

## groupby and aggregate

``` r
dat = pl$DataFrame(iris)
dat$group_by('Species')$
agg(
    mean_Peta_width = pl$col('Petal.Width')$mean(), 
    sum_Sepal_Width = pl$col("Sepal.Width")$sum()
)
```

    shape: (3, 3)
    ┌────────────┬─────────────────┬─────────────────┐
    │ Species    ┆ mean_Peta_width ┆ sum_Sepal_Width │
    │ ---        ┆ ---             ┆ ---             │
    │ cat        ┆ f64             ┆ f64             │
    ╞════════════╪═════════════════╪═════════════════╡
    │ setosa     ┆ 0.246           ┆ 171.4           │
    │ virginica  ┆ 2.026           ┆ 148.7           │
    │ versicolor ┆ 1.326           ┆ 138.5           │
    └────────────┴─────────────────┴─────────────────┘

## mutate

``` r
dat <- pl$DataFrame(
    Id = 1:10, 
    x1 = rnorm(10) |> round(2), 
    x2 = rgamma(10, 2, 2) |> round(2), 
    x3 = rbinom(10, size = 5, prob = 0.4)
)

dat2 = dat$with_columns(
    val1 = pl$when(pl$col('x1') > 1)
    $then(1)
    $otherwise(-1)
)
```

## inexing

``` r
dat2[1, 2]
```

    polars Series: shape: (1,)
    Series: 'x1' [f64]
    [
        0.85
    ]

``` r
dat2[1, 1:2]
```

    shape: (1, 2)
    ┌─────┬──────┐
    │ Id  ┆ x1   │
    │ --- ┆ ---  │
    │ i32 ┆ f64  │
    ╞═════╪══════╡
    │ 1   ┆ 0.85 │
    └─────┴──────┘

``` r
dat2[-1, 1:2]
```

    shape: (9, 2)
    ┌─────┬───────┐
    │ Id  ┆ x1    │
    │ --- ┆ ---   │
    │ i32 ┆ f64   │
    ╞═════╪═══════╡
    │ 2   ┆ -1.21 │
    │ 3   ┆ -1.97 │
    │ 4   ┆ -1.23 │
    │ 5   ┆ -0.46 │
    │ 6   ┆ 0.43  │
    │ 7   ┆ 0.32  │
    │ 8   ┆ 1.2   │
    │ 9   ┆ -2.04 │
    │ 10  ┆ 0.47  │
    └─────┴───────┘

## generate a sample data

``` r
set.seed(1234)
datt <- pl$DataFrame(
    x1 = rnorm(5e+7, 2, 1), 
    x2 = rgamma(5e+7, 2, 2), 
    x3 = rbinom(5e+7, 15, 0.6), 
    x4 = rnorm(5e+7, 5, 2), 
    x5 = rnorm(5e+7, -10, 3), 
    x6 = rnorm(5e+7, -20, 2), 
    x7 = rgamma(5e+7, 5, 3), 
    x8 = rexp(5e+7, rate = 1/4), 
    x9 = rnorm(5e+7, -7, 2), 
    x10 = rnorm(5e+7, 25, 2)
)


datt$write_csv("Data_samp.csv")
```

## compare with data.table

``` r
rm(list = ls())
gc()
```

              used (Mb) gc trigger   (Mb)  max used   (Mb)
    Ncells  710713 38.0    1269752   67.9   1269752   67.9
    Vcells 1427642 10.9  459045465 3502.3 476528618 3635.7

``` r
## data.table section get elapsed time for read and run function along columns
library(data.table)

start_time1 <- Sys.time();
dat1 <- fread('Data_samp.csv', header = T);
dat1[, lapply(.SD, mean)];
```

             x1        x2       x3      x4        x5        x6      x7       x8
    1: 1.999782 0.9998909 8.999525 5.00011 -10.00005 -19.99996 1.66678 4.000198
             x9      x10
    1: -6.99986 25.00029

``` r
end_time1 <- Sys.time();
(Diff_time_data_table <- difftime(end_time1, 
            start_time1, units = "secs"))
```

    Time difference of 14.29901 secs

``` r
(Memory_used_data_table <- object.size(dat1))
```

    3800002952 bytes

------------------------------------------------------------------------

------------------------------------------------------------------------

``` r
rm(list = ls())
gc()
```

              used (Mb) gc trigger   (Mb)  max used   (Mb)
    Ncells  765540 40.9    1269752   67.9   1269752   67.9
    Vcells 1560921 12.0  451900016 3447.8 524469680 4001.4

``` r
## polars section get elapsed time for read and run function along columns
start_time2 <- Sys.time();
dat2 <- pl$read_csv('Data_samp.csv');
dat2$mean()
```

    shape: (1, 10)
    ┌──────────┬──────────┬──────────┬─────────┬───┬─────────┬──────────┬──────────┬───────────┐
    │ x1       ┆ x2       ┆ x3       ┆ x4      ┆ … ┆ x7      ┆ x8       ┆ x9       ┆ x10       │
    │ ---      ┆ ---      ┆ ---      ┆ ---     ┆   ┆ ---     ┆ ---      ┆ ---      ┆ ---       │
    │ f64      ┆ f64      ┆ f64      ┆ f64     ┆   ┆ f64     ┆ f64      ┆ f64      ┆ f64       │
    ╞══════════╪══════════╪══════════╪═════════╪═══╪═════════╪══════════╪══════════╪═══════════╡
    │ 1.999782 ┆ 0.999891 ┆ 8.999525 ┆ 5.00011 ┆ … ┆ 1.66678 ┆ 4.000198 ┆ -6.99986 ┆ 25.000289 │
    └──────────┴──────────┴──────────┴─────────┴───┴─────────┴──────────┴──────────┴───────────┘

``` r
end_time2 <- Sys.time();
(Diff_time_polars <- difftime(end_time2, 
            start_time2, units = "secs"))
```

    Time difference of 10.74136 secs

``` r
(Memory_used_polars <- object.size(dat2))
```

    464 bytes
