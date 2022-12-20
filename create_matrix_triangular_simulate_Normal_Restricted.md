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

    ##  [1] 3.052545 3.152677 2.841796 2.597047 3.216420 3.803350 3.361150 2.828218
    ##  [9] 2.246507 3.528270 2.176596

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

    ##  [1] 4.279872 3.157285 4.485590 3.151667 3.479212 3.408565 4.241121 3.784406
    ##  [9] 3.970894 3.320588 3.551378 3.783441 3.564179 4.177438 3.124133 4.996380
    ## [17] 3.011841 3.521824 3.351093 4.633351

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

    ## [1] 1.689949 2.011948 2.958791 1.338709 2.319952

``` r
E[[10]]
```

    ##  [1] 2.866425 1.396811 5.020024 2.701937 1.814469 3.827689 2.784728 2.185148
    ##  [9] 2.031921 1.913772

``` r
E[[20]]
```

    ##  [1] -0.4829301  4.0468436  3.0461689  2.6099620  1.5166851  2.4218403
    ##  [7]  4.6233172  0.8834062  3.1885865  2.7486370  4.6068982  1.8252190
    ## [13]  2.3245541  2.6008667  2.2467537  3.0760220  2.4866580  3.9017023
    ## [19]  3.1929782  2.8067127

``` r
E[[50]]
```

    ##  [1] 3.0482558 3.0092611 3.4812892 2.5744753 3.9000371 2.8078498 3.0718179
    ##  [8] 1.1901736 0.9488820 0.6080001 1.9051153 1.7290454 2.6392676 1.8903783
    ## [15] 3.6032881 1.7702794 3.5558105 1.1082847 1.9302871 2.8032129 2.8665783
    ## [22] 2.8050780 0.9506473 1.7567201 2.0540655 3.4356916 1.7146090 3.8863378
    ## [29] 3.5528911 2.5807447 1.8096929 1.6556153 2.6984574 3.7222788 1.0007015
    ## [36] 4.5876349 3.1035509 1.0726483 3.3122388 3.0543158 3.8612304 1.6195883
    ## [43] 3.5718243 2.6007603 3.0963501 1.5667235 1.5169648 1.5211120 2.6253449
    ## [50] 1.6726263
