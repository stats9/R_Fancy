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

    ##  [1] 3.329284 3.418870 2.880448 2.044628 2.981418 2.177651 3.456726 3.403046
    ##  [9] 2.942322 3.665778 3.821326

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

    ##  [1] 4.563002 4.024528 3.527224 3.115864 3.741727 3.464179 3.975816 4.690984
    ##  [9] 4.826396 3.248170 3.877956 3.975064 4.936459 4.307681 3.868485 3.121748
    ## [17] 3.921882 3.894073 3.142837 3.201987

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
        abline(h = Mu, lwd = 1.5, lty = 2, col = "yellow")
        legend("topright", legend = expression(mu), lty = 2, lwd = 3, col = "yellow", bg = "black", cex = 3, 
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

    ## [1] 1.077492 4.030520 3.131422 2.348707 3.589382

``` r
E[[10]]
```

    ##  [1] 1.5053689 3.4744653 3.5785658 3.1360079 2.1990726 3.1302398 0.9256082
    ##  [8] 2.1428948 1.9284358 2.3137186

``` r
E[[20]]
```

    ##  [1] 0.8796887 2.0430578 2.1153536 2.5230684 1.2837680 4.0416681 3.5384266
    ##  [8] 1.3555942 3.0631830 4.3333647 2.1293527 3.6434273 1.1474375 1.2430225
    ## [15] 1.7483939 3.0845755 1.2710736 2.4864752 1.1460746 3.1234155

``` r
E[[50]]
```

    ##  [1] 4.4855392 2.5046952 2.2110471 2.8011893 2.6338804 2.0414188 3.1678348
    ##  [8] 2.7086089 2.4284821 1.1241315 5.5334067 3.0017154 4.9487260 1.4250147
    ## [15] 2.8714870 1.5139882 3.7594581 3.5861644 2.2428904 1.9280998 4.0433153
    ## [22] 0.8743913 2.5985979 2.1737700 0.8060065 3.3565356 3.3496028 1.4197788
    ## [29] 1.5461643 3.2008576 1.2369647 3.1719405 3.5419376 0.3918326 1.9666093
    ## [36] 2.3988044 1.8150393 1.7359145 0.7726154 2.4403573 3.6271459 1.9444124
    ## [43] 0.3582610 2.7814769 2.0676327 2.7773501 2.6520091 1.2943793 2.1698242
    ## [50] 3.3558391
