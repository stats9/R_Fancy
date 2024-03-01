# In
this
presentation
we
want
to
introduce
8
types
of
Distance
or
Similarity
that
Using
in
Clustering.

#### preparation for using R and python

``` r
library (reticulate)
path <- Sys.which("python")
path <- gsub("\\", "//", path, fixed = TRUE)
use_python(path)
```

<br><br>

#### Eculidean Distance Using R

<br>

``` r
set.seed(123)
Mat <- matrix(rnorm(40, 2, 2) |> round(2), 2, 20)
head(Mat)
```

         [,1] [,2] [,3]  [,4] [,5] [,6] [,7] [,8]  [,9] [,10] [,11] [,12] [,13]
    [1,] 0.88 5.12 2.26  2.92 0.63 4.45 2.80 0.89  3.00  3.40 -0.14 -0.05  0.75
    [2,] 1.54 2.14 5.43 -0.53 1.11 2.72 2.22 5.57 -1.93  1.05  1.56  0.54 -1.37
         [,14] [,15] [,16] [,17] [,18] [,19] [,20]
    [1,]  3.68 -0.28  2.85  3.79  3.64  3.11  1.39
    [2,]  2.31  4.51  1.41  3.76  3.38  1.88  1.24

``` r
euclidean <- function(a, b) sum((a - b)^2)

euclidean(Mat[1, ], Mat[2, ])
```

    [1] 122.7879
