Untitled
================

## Ex I.

#### A function that takes a square matrix and sets it to zero above the main diagonal

``` r
fun_tri_mat <- function(A){
    D <- dim(A)
    if(D[1] != D[2]) stop("A must be square matrix")
    n <- D[1]
    for(i in 1:(n-1)){
        for(j in (i+1):n){
            A[i, j] <- 0
        }
    }
    return(A)
}
A <- matrix(1, 4, 4)
A
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    1    1    1
    ## [2,]    1    1    1    1
    ## [3,]    1    1    1    1
    ## [4,]    1    1    1    1

``` r
A2 <- fun_tri_mat(A)
A2
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    0    0    0
    ## [2,]    1    1    0    0
    ## [3,]    1    1    1    0
    ## [4,]    1    1    1    1

------------------------------------------------------------------------

## EX II.

#### A function that is a normal sample of the given limits, in such a way that the simulated

#### values must be larger than the specified value of A and smaller than the specified value of B.

#### In this function, the size of the production sample and the user’s desired mean and standard deviation,

#### as well as the user’s desired limits, are given in the function arguments.

``` r
create_normal_restrict <- function(size, 
Mu, Sig, a, b){
    i = 0
    Sample <- c()
    while(i <= size){
        temp1 <- rnorm(1, Mu, Sig)
        if(temp1 < b && temp1 > a){
            i = i + 1; Sample[i] <- temp1
        }
    }
    return(Sample)
}
s <- create_normal_restrict(size = 10, Mu = 0, Sig = 2, a = 2, b = 4)
s
```

    ##  [1] 2.787215 2.483093 3.952442 2.785211 3.214247 3.154735 3.557060 2.265754
    ##  [9] 2.034313 2.456821 2.925736

``` r
all(s > 2) && all(s < 4)
```

    ## [1] TRUE

------------------------------------------------------------------------

## EX III.

#### Edit code for EX II.

``` r
create_normal_restrict <- function(size, 
Mu, Sig, a, b){
    Sample <- c()
    for(i in 1:size){
        temp1 <- rnorm(1, Mu, Sig)
        while(temp1 < a || temp1 > b){
            temp1 <- rnorm(1, Mu, Sig)
        }
        Sample[i] <- temp1
    }
    return(Sample)
}
s <- create_normal_restrict(size = 20, Mu = 2, Sig = 3, a = 3, b = 5)
s
```

    ##  [1] 3.180813 3.069575 4.206359 3.441129 4.582400 4.540356 3.187712 3.147905
    ##  [9] 4.811751 4.117554 3.527012 4.537769 3.922505 4.016576 3.244397 3.476991
    ## [17] 4.983215 3.394077 3.072002 3.887971

``` r
all(s > 3) && all(s < 5)
```

    ## [1] TRUE

------------------------------------------------------------------------

## EX IV.

#### Simulation to prove the weak law of large numbers

``` r
Weak_law_large_num <- function(size, Mu, showPlot = TRUE){
    n <- size
    E <- list()
    for(i in 1:n){
        E[[i]] <- rnorm(i, mean = Mu, sd = 1)
    }
    E_mean <- unlist(lapply(E, mean))
    list_result <- list(Mean_result = E_mean, list_result = E)
    if(showPlot){
        plot(x = 1:n, y = E_mean, col = "darkblue", type = "l", xlab = "size of sample", ylab = "Sample Mean")
        abline(h = Mu, lwd = 3, lty = 2, col = "yellow")
        legend("topright", legend = expression(mu), lty = 3, lwd = 2, col = "yellow", bg = "black", cex = 3, 
        text.col = "white")
    }
    return(list_result)
}

Result <- Weak_law_large_num(size = 1e+4, Mu = 2.5, showPlot = T) 
```

![](create_matrix_triangular_simulate_Normal_Restricted_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
E <- Result$list_result
E[[5]]
```

    ## [1] 3.262878 1.593083 2.795251 3.138652 2.581531

``` r
E[[10]]
```

    ##  [1] 1.7229436 2.0240644 2.4916426 0.9277582 2.8907203 0.9146274 1.9194793
    ##  [8] 4.9567248 0.5545158 2.5850719

``` r
E[[20]]
```

    ##  [1] 3.5193450 4.4503566 5.3910323 2.6142054 0.5388997 2.7319906 1.5840976
    ##  [8] 2.6734690 3.1669053 3.6409381 2.9389939 3.3371811 3.3421878 3.8735887
    ## [15] 1.6812619 0.6395904 2.7290516 2.6239015 2.0979262 4.1868497

``` r
E[[50]]
```

    ##  [1] 2.79630961 2.07174192 3.08921644 1.77748164 2.66617963 1.71480973
    ##  [7] 2.89334283 1.60189604 0.14033021 3.82304949 3.50272324 2.48509268
    ## [13] 2.40204977 2.57144720 0.93044675 2.49935304 3.11511673 2.90565437
    ## [19] 3.49775894 3.31119155 5.11401586 2.64866640 2.46460890 3.05071311
    ## [25] 0.01764354 2.13542374 3.26614672 2.69267938 3.26295078 2.20214606
    ## [31] 2.12777702 2.24205620 1.88394126 2.10860434 3.52891703 1.93377206
    ## [37] 1.56448768 3.13220410 3.16915131 2.20174537 2.79845354 1.91000061
    ## [43] 3.20742671 2.61299206 2.61415469 4.25214172 3.67260927 3.10890179
    ## [49] 1.63799093 4.46721902
