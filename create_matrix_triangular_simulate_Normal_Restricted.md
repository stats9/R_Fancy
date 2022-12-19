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

    ##  [1] 2.126661 3.715538 2.177037 3.206144 2.500492 3.132915 2.851895 2.703510
    ##  [9] 2.129966 2.840252 2.397677

``` r
all(s > 2) && all(s < 4)
```

    ## [1] TRUE
