create_matrix_triangular_simulate_Normal_Restricted
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

    ##  [1] 2.243753 2.141859 2.499287 2.411613 2.786077 3.007454 2.016532 2.194721
    ##  [9] 2.960471 3.566795 3.004377

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

    ##  [1] 3.911585 3.921723 3.122110 3.948795 4.774382 3.076863 3.469231 4.365678
    ##  [9] 3.269514 4.202419 3.463050 3.377031 3.380355 3.799234 4.243608 4.837978
    ## [17] 3.189766 4.140438 3.525532 3.183216

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

    ## [1] 3.116940 4.857782 1.882710 1.226727 1.481583

``` r
E[[10]]
```

    ##  [1] 3.292993 2.268598 3.157134 3.571759 3.159709 1.713299 4.043080 3.037837
    ##  [9] 2.692407 1.889819

``` r
E[[20]]
```

    ##  [1] -0.6664324  4.8782451  2.9060415  0.6526347  1.9626721  0.7180531
    ##  [7]  3.2976689  3.4396635  1.9731405  2.4000012  1.4585400  3.0614244
    ## [13]  2.0365139  3.8850386  2.3145039  3.2419005  4.0532249  2.6540564
    ## [19]  2.6158590  3.2020568

``` r
E[[50]]
```

    ##  [1] 2.0276034 2.1878928 4.3126485 2.5441813 0.3146525 2.4537864 3.5692615
    ##  [8] 3.7117593 0.9467995 2.6343303 0.5337894 2.4028012 1.1944789 2.2382669
    ## [15] 1.6622360 2.8248932 3.2131675 3.0547236 3.0619314 1.9556758 2.4052841
    ## [22] 2.4501912 1.2803878 2.2348861 1.8950170 2.2464262 0.9141530 2.1164518
    ## [29] 3.2233743 1.5031934 5.0491144 2.7211629 2.6247484 2.4374962 0.8734581
    ## [36] 2.7386009 1.3735067 1.2746713 0.8909878 2.0065375 2.0193940 0.4261524
    ## [43] 2.3482436 2.7726120 2.3382846 2.5743570 3.2558597 0.8673530 3.0893295
    ## [50] 1.3701027
