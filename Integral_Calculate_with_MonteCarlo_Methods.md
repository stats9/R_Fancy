Using MonteCarlo Method for Integrate
================
habib ezzatabadi (stats9)

# First Method; MonteCarlo Simulation —-

------------------------------------------------------------------------

## Suppose we want to calculate the following integral using the Monte Carlo method.

$$
\int_{0}^{+ \infty}  \exp(-x)dx
$$

## For this purpose, we must first transfer this integral to a limited interval with a suitable transformation.

$$
\begin{aligned}
& u = \frac{1}{x + 1} \implies \\
& x + 1 = \frac{1}{u} \implies x = \frac{1}{u} - 1 \implies \\
& dx = -\frac{1}{u^2}du \implies 
\int_{0}^{+\infty} \exp(-x)dx = \int_1^0 -\frac{1}{u^2}\times \exp\{-(\frac{1}{u} - 1)\}du \\
& =  I = \int_0^1 \frac{1}{u^2}\times \exp\{-(\frac{1}{u} - 1)\}du \implies \\
& \text{if} \quad g(u) = \frac{1}{u^2}\times \exp\{-(\frac{1}{u} - 1)\} \implies \\
& I \approx \frac{1}{n} \sum_{1}^n g(u_i), \quad U_i \overset{iid}{\sim} \text{Uniform}(0, ~1), ~~ n = \text{number of generate} \implies 
\end{aligned}
$$

#### Code Soloution in R:

``` r
set.seed(1)
f <- function(x) exp(-x)
g <- function(u) 1/u**2 * exp(-(1/u - 1)) # define g
n <- 1e+5 # number of generate
U <- runif(n) # generate uniform random variables 
I <- mean(g(U))

Exact_value <- integrate(f, 0, Inf)$value
result <- data.frame(Integral_Method = c("MonteCarlo", "Exact_Value"), 
            Value = c(I, Exact_value))
result |> 
        knitr:: kable(caption = "Results", align = "c")
```

| Integral_Method |   Value   |
|:---------------:|:---------:|
|   MonteCarlo    | 0.9974524 |
|   Exact_Value   | 1.0000000 |

Results

------------------------------------------------------------------------

------------------------------------------------------------------------

# Method II

## Importance Sampling Method

## Suppose we want to take the integral of the following function.

$$
I = \int_0^\infty x^2 \exp(-x^2)dx
$$

## Soloution:

$$
\begin{aligned}
& \text{if} \quad g = x^2 \times \exp(-x^2) = x \times x \times \exp(-x^2) \\
& \text{if}\quad U \sim Weibull(\alpha = 2, \lambda = 1) \implies \\
& f_U(u) = 2x \times \exp(-x^2) \\
& \implies I = \mathcal{E}(\frac{1}{2}U) = \frac{1}{2} \times\mathcal{E}(U)
\end{aligned}
$$

## Code Soloution in R:

``` r
n <- 1e+6
W <- rweibull(n, shape = 2, scale = 1)
res <- 1/2 * mean(W)
g <- function(x) x**2 * exp(-x**2)

Exact_value <- integrate(g, 0, Inf)$value

result <- data.frame(Integral_Method = c("MonteCarlo", "Exact_Value"), 
            Value = c(res, Exact_value))
result |> 
        knitr:: kable(caption = "Results", align = "c")
```

| Integral_Method |   Value   |
|:---------------:|:---------:|
|   MonteCarlo    | 0.4430934 |
|   Exact_Value   | 0.4431135 |

Results
