# distance-similarity
index


- [preparation
  for
  using
  R
  and
  python](#preparation-for-using-r-and-python)
- [Eculidean
  Distance
  Using
  R
  (i)](#eculidean-distance-using-r-i)
- [Eculidean
  Distance
  Using
  python
  (i)](#eculidean-distance-using-python-i)
- [Minkovski
  Distance
  Using
  R
  (ii)](#minkovski-distance-using-r-ii)
- [Minkovski
  Distance
  Using
  Python
  (ii)](#minkovski-distance-using-python-ii)
- [Manhattan
  Distance
  Using
  R
  (iii)](#manhattan-distance-using-r-iii)
- [Manhattan
  Distance
  Using
  Python
  (iii)](#manhattan-distance-using-python-iii)
- [hamming
  Distance
  Using
  R
  (iv)](#hamming-distance-using-r-iv)
- [hamming
  Distance
  Using
  Python
  (iv)](#hamming-distance-using-python-iv)
- [Cosine
  Distance
  Using
  R
  (v)](#cosine-distance-using-r-v)
- [Cosine
  Distance
  Using
  Python
  (v)](#cosine-distance-using-python-v)
- [gower
  Index
  Using
  R
  (vi)](#gower-index-using-r-vi)
- [gower
  Index
  Using
  Python
  (vi)](#gower-index-using-python-vi)
- [Czekanowski-Sorensen
  Index
  Using
  R](#czekanowski-sorensen-index-using-r)
- [Czekanowski-Sorensen
  Index
  Using
  python](#czekanowski-sorensen-index-using-python)
- [Jaccard
  Index
  Using
  R](#jaccard-index-using-r)
- [Jaccard
  Index
  Using
  python](#jaccard-index-using-python)

<hr>
<hr>

<br><br>

#### preparation for using R and python

``` r
library (reticulate)
path <- Sys.which("python")
path <- gsub("\\", "//", path, fixed = TRUE)
use_python(path)
```

<hr>
<hr>

<br><br>

#### Eculidean Distance Using R (i)

<hr>

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
dist(Mat, method = "euclidean")
```

             1
    2 11.08097

<br><br>

#### Eculidean Distance Using python (i)

<hr>

``` python
from scipy.spatial import distance

# Calculate Euclidean distance
dist = distance.euclidean(r.Mat[0, :], r.Mat[1, :])
print(round(dist, 5))
```

    11.08097

<br>
<br>
<br>

### Minkovski Distance Using R (ii)

<br>

``` r
k <- 5
dist(Mat, method = "minkowski", p = k)
```

             1
    2 6.161605

<br><br>

### Minkovski Distance Using Python (ii)

<hr>

``` python
distance.minkowski(r.Mat[0, :], r.Mat[1, :], r.k)
```

    6.161604866392561

<br>
<br>
<br>

### Manhattan Distance Using R (iii)

<br>

``` r
dist(Mat, method = "manhattan")
```

          1
    2 38.69

<br><br>

### Manhattan Distance Using Python (iii)

<hr>

``` python
distance.cityblock(r.Mat[0, :], r.Mat[1, :])
```

    38.68999999999999

<br>
<br>
<br>

### hamming Distance Using R (iv)

<br>

``` r
x <- c(1, -1, 1, 1, -1, 1, 1)
y <- c(-1, -1, 1, 1, 1, -1, 1)
Hamming_dist <- sum(x != y)
Hamming_dist
```

    [1] 3

<br><br>

### hamming Distance Using Python (iv)

<hr>

``` python
distance.hamming(r.x, r.y) * len(r.x)
```

    3.0

<br>
<br>
<br>

### Cosine Distance Using R (v)

<br>

``` r
if(!require(stylo)) {
    chooseCRANmirror(graphics = F, ind = 1)
    install.packages("stylo")
}
```

    Loading required package: stylo


    ### stylo version: 0.7.4 ###

    If you plan to cite this software (please do!), use the following reference:
        Eder, M., Rybicki, J. and Kestemont, M. (2016). Stylometry with R:
        a package for computational text analysis. R Journal 8(1): 107-121.
        <https://journal.r-project.org/archive/2016/RJ-2016-007/index.html>

    To get full BibTeX entry, type: citation("stylo")

``` r
x <- c(3, 4, 1, 3, 5, 10)
y <- c(1, 0, 3, 5, 10, 11)
cosine_dist <- stylo :: dist.cosine(rbind(x, y))
cosine_dist
```

              x
    y 0.1056683

<br><br>

### Cosine Distance Using Python (v)

<hr>

``` python
distance.cosine(r.x, r.y) 
```

    0.10566834923363022

<br>
<br>
<br>

### gower Index Using R (vi)

<br>

``` r
if(!require(cluster)) {
    chooseCRANmirror(graphics = F, ind = 1)
    install.packages("cluster")
}
```

    Loading required package: cluster

``` r
df <- data.frame(
  num = c(1, 2, 3, 4),
  cat = factor(c("A", "B", "A", "B"))
)

df
```

      num cat
    1   1   A
    2   2   B
    3   3   A
    4   4   B

``` r
gower_dist <- cluster :: daisy(df, metric = "gower")

print(gower_dist)
```

    Dissimilarities :
              1         2         3
    2 0.6666667                    
    3 0.3333333 0.6666667          
    4 1.0000000 0.3333333 0.6666667

    Metric :  mixed ;  Types = I, N 
    Number of objects : 4

<br><br>

### gower Index Using Python (vi)

<hr>

``` python
import PyDistances as distt
import pandas as pd
distt.Gower_Similarity_Matrix(r.df, p1 = 1, p2 = 0, p3 = 1)
```

    array([[1.        , 0.33333333, 0.66666667, 0.        ],
           [0.33333333, 1.        , 0.33333333, 0.66666667],
           [0.66666667, 0.33333333, 1.        , 0.33333333],
           [0.        , 0.66666667, 0.33333333, 1.        ]])

<br>
<br>
<br>

### Czekanowski-Sorensen Index Using R

``` r
x <- c('a', 'a', 'b', 'c', 'b', 'b', 'a')
y <- c('b', 'd', 'a', 'b', 'a', 'b', 'c')

dist_sorensen <- 2 * (intersect(x, y) |> length()) / 
                    (x |> unique() |> length() + y |> unique()|> length()) 
dist_sorensen
```

    [1] 0.8571429

<br><br><br>

### Czekanowski-Sorensen Index Using python

``` python
2 * len(set(r.x).intersection(set(r.y))) / (len(set(r.x)) + len(set(r.y)))
```

    0.8571428571428571

<br>
<br>
<br>

### Jaccard Index Using R

``` r
x <- c('a', 'a', 'b', 'c', 'b', 'b', 'a')
y <- c('b', 'd', 'a', 'b', 'a', 'b', 'c')

dist_Jaccard <- (intersect(x, y) |> length()) / 
                    (union(x, y) |> length()) 
dist_Jaccard
```

    [1] 0.75

<br><br><br>

### Jaccard Index Using python

``` python
len(set(r.x).intersection(set(r.y))) / (len(set(r.x).union(set(r.y))))
```

    0.75
