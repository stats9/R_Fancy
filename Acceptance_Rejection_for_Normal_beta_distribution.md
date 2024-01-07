# Acceptance-Rejection Algorithm

## Problem i

From a normal distribution with mean 4 and standard deviation 2, run a
simulation of size 10,000. (Consider the candidate density, the density
of an exponential distribution with a mean of 10).

$$
\begin{aligned}
& \text{target density:}~~ Y \sim \mathcal{N}(\mu = 4, \sigma = 2), \\
& \text{Candidate density:} ~~ \sim \mathcal{E}\text{xp}\left(\text{rate} = \frac{1}{10}\right)
\end{aligned}
$$

``` r
x <- 2
print(x)
```

    [1] 2
