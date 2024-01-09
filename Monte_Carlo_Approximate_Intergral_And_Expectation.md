# MonteCarlo Excercise

## Problem: 1

Obtain an approximation for the expression below using simulation.

$$
\begin{aligned}
& U \sim \mathcal{U}(0, 1), \\
& Y = \exp(U) \implies \\
& \text{Cov}(U, Y) = ?
\end{aligned}
$$

#### Theoretical solution

$$
\begin{aligned}
& \text{Cov}(U, Y) = \mathcal{E}(U \times Y) - \mathcal{E}(U \times Y) = \\
& \mathcal{E}(U \times \exp(U)) - \mathcal{E}(U)\times \mathcal{E}(\exp(U)) \\
& \mathcal{E}(U \times \exp(U)) = \int_0^1 u \times \exp(u) du = u\exp(u) - \int_0^1 \exp(u) du  = \\
& \left[u\exp(u) - \exp(u)\right]_0^1  = \exp(1) - \exp(1) - (0 - 1) = 1, \\
& \mathcal{E}(U) = \frac{1}{2}, \quad \mathcal{E}(\exp(U)) = \int_0^1 \exp(u)du = [\exp(u)]_0^1 = \exp(1) - \exp(0) = \\
& \exp(1) - 1 \implies \\
& \text{Cov}(U, Y) = 1 - \frac{1}{2} \times (\exp(1) - 1) = \frac{3 - \exp(1)}{2} \approx 0.1409
\end{aligned}
$$

#### Code:

``` r
sim_fun_1 <- function(n){
    U = runif(n)
    Y = exp(U)
    covv = mean(U * Y) - mean(U) * mean(Y)
    return (covv)
}

num = c(1e+4, 1e+5, 1e+6, 1e+7)

result = unlist(lapply(num, sim_fun_1))
```

``` r
print(cbind(num, result))
```

| number_of_Iteration |  result   | Theory_vlaue |
|:-------------------:|:---------:|:------------:|
|        1e+04        | 0.1383793 |   0.140859   |
|        1e+05        | 0.1407337 |   0.140859   |
|        1e+06        | 0.1408430 |   0.140859   |
|        1e+07        | 0.1408926 |   0.140859   |

Table of simulation Results

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

## Problem 2:

$$
\begin{aligned}
& U \sim \mathcal{U}(0, 1), \quad Y = \lfloor 10 \times U \rfloor + 1 \\
& \text{Cov}(U Y) = ?
\end{aligned}
$$

#### Theory Soloution

$$
\begin{aligned}
& Y = \begin{cases} 
y = 1 \iff U < \frac{1}{10}, \\
y = 2 \iff \frac{1}{10} U < \frac{2}{10} , \\
y = 3 \iff \frac{2}{10} U < \frac{3}{10}, \\
y = 4 \iff \frac{3}{10} U < \frac{4}{10}, \\
y = 5 \iff \frac{4}{10} U < \frac{5}{10}, \\
y = 6 \iff \frac{5}{10} U < \frac{6}{10}, \\
y = 7 \iff \frac{6}{10} U < \frac{7}{10}, \\
y = 8 \iff \frac{7}{10} U < \frac{8}{10}, \\
y = 9 \iff \frac{8}{10} U < \frac{9}{10}, \\
y = 10 \iff \frac{9}{10} U < \frac{10}{10}.
\end{cases} \implies \\
& \mathbb{P}(Y = y) = \begin{cases} 
y = 1 & \mathbb{P}(U < \frac{1}{10}) = \frac{1}{10} , \\
y = 2 &  \mathbb{P}(\frac{1}{10} < U < \frac{2}{10}) = \frac{1}{10}, \\
y = 3 &  \mathbb{P}(\frac{2}{10} < U < \frac{3}{10}) = \frac{1}{10}, \\
y = 4 &  \mathbb{P}(\frac{3}{10} < U < \frac{4}{10}) = \frac{1}{10}, \\
y = 5 &  \mathbb{P}(\frac{4}{10} < U < \frac{5}{10}) = \frac{1}{10}, \\
y = 6 &  \mathbb{P}(\frac{5}{10} < U < \frac{6}{10}) = \frac{1}{10}, \\
y = 7 &  \mathbb{P}(\frac{6}{10} < U < \frac{7}{10}) = \frac{1}{10}, \\
y = 8 &  \mathbb{P}(\frac{7}{10} < U < \frac{8}{10}) = \frac{1}{10}, \\
y = 9 &  \mathbb{P}(\frac{8}{10} < U < \frac{9}{10}) = \frac{1}{10}, \\
y = 10 & \mathbb{P}(\frac{9}{10} < U < \frac{10}{10}) = \frac{1}{10},
\end{cases}
\end{aligned}
$$

$$
\begin{aligned}
& \text{Cov}(Y \exp(Y)) = \mathcal{E}(Y \exp(Y)) - \mathcal{E}(Y) \times \mathcal{E}(\exp(Y)), \\
& \mathcal{E}(\exp(Y)) = \sum_{i = 1}^{10} \exp(i) \times \frac{1}{10} = \frac{1}{10}\times \sum_{i = 1}^{10} \exp(i) \approx 3484.377385, \\
& \mathcal{E}{Y} = \frac{1}{10}\sum_{i = 1}^{10} i = \frac{1}{10}\frac{10 \times 11}{2} = \frac{11}{2}, \\
& \mathcal{E}(Y \exp(Y)) = \frac{1}{10}\sum_{i = 1}{10} i \times \exp(i) \approx 3.2817529\times 10^{4}\\
& \text{Cov}(Y, \exp(Y)) = 3.2817529\times 10^{4} - \frac{11}{2} \times 3484.377385 \approx 1.3653454\times 10^{4}
\end{aligned}
$$

#### Simulate With R

``` r
sim_fun_2 <- function(n){
    U = runif(n)
    Y = floor(10 * U) + 1
    Y2= exp(Y)
    covv = mean(Y * Y2) - mean(Y) * mean(Y2)
    return (covv)
}

num = c(1e+4, 1e+5, 1e+6, 1e+7)

result = unlist(lapply(num, sim_fun_2))
```

``` r
print(cbind(num, result))
```

| number_of_Iteration |  result  | Theory_vlaue |
|:-------------------:|:--------:|:------------:|
|        1e+04        | 13907.35 |   13653.45   |
|        1e+05        | 13680.93 |   13653.45   |
|        1e+06        | 13672.89 |   13653.45   |
|        1e+07        | 13646.98 |   13653.45   |

Table of simulation Results

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

## Problem iii

$$
\begin{aligned} 
a: \quad & \int_0^1 \exp(\exp(x)) ~dx, \\
~~ & \\
b: \quad & \int_{0}^{1} (1 - x^2)^\frac{3}{2}~dx, \\
~~ & \\
c: \quad & \int_{-2}^{+2} \exp(x + x^2)~dx, \\
~~ & \\
d: \quad & \int_{-\infty}^{+\infty} \exp(-x^2)~dx
\end{aligned}
$$

> Soloution a:

``` r
f <- function(x) exp(exp(x))

Real_value <- integrate(f, 0, 1)$value

## Simulation Approach 

n <- 1e+5
U <- runif(n)
simulation_value <- mean(f(U))

c(Real_value = Real_value, Simulation_value = simulation_value)
```

          Real_value Simulation_value 
            6.316564         6.310207 

------------------------------------------------------------------------

> soloution b:

``` r
f <- function(x) (1 - x**2)**(3/2)

Real_value <- integrate(f, 0, 1)$value

## Simulation Approach 

n <- 1e+5
U <- runif(n)
simulation_value <- mean(f(U))

c(Real_value = Real_value, Simulation_value = simulation_value)
```

          Real_value Simulation_value 
           0.5890486        0.5885730 

------------------------------------------------------------------------

> soloution c:

``` r
f <- function(x) exp(x + x**2)

Real_value <- integrate(f, -2, 2)$value

## Simulation Approach 

n <- 1e+5
U <- runif(n, min = -2, max = 2)
simulation_value <- 4 * mean(f(U))

c(Real_value = Real_value, Simulation_value = simulation_value)
```

          Real_value Simulation_value 
            93.16275         93.51183 

------------------------------------------------------------------------

> soloution d:

$$
\begin{aligned} 
& \int_{-\infty}^{+\infty} \exp(-x^2)~dx \overset{\text{even function}}{=} 2\int_{0}^{+\infty} \exp(-x^2)~dx, \\
& ~~ \\
& \text{if} ~~ u = \frac{1}{x^2 + 1} \implies x = \sqrt{\frac{1}{u} - 1} \\ 
& ~~\\
& \text{and}~~ du = dx \times \frac{-2x}{(x^2 + 1)^2} \implies du \times \frac{(x^2 + 1)^2}{-2x} = dx\\
&~~\\
& \int_{-\infty}^{+\infty} \exp(-x^2)~dx = 2 \times \int_1^0 \frac{1}{(-2 \times\sqrt{\frac{1}{u} - 1})} \times \frac{1}{u^2} \times \exp(\frac{u - 1}{u})~du \\
& ~~\\
& = \int_0^1 \frac{1}{(2 \times\sqrt{\frac{1}{u} - 1})} \times \frac{1}{u^2} \times \exp(\frac{u - 1}{u})~du \\
& ~~\\
\end{aligned}
$$

#### Exact answer

$$
\begin{aligned}
& \text{if}~~ X \sim \mathcal{N}\left(0,~ \frac{\sqrt{2}}{2}\right) \implies \\
& f_X(x) = \frac{1}{\sqrt{2 \times \pi \times \left(\frac{\sqrt{2}}{2}\right)^2}} \exp\left(-\frac{1}{2 \left(\frac{\sqrt{2}}{2}\right)^2}x^2\right) \\
& = \frac{1}{\sqrt{\pi}}\times \exp(-x^2) \implies \\
& \int_{-\infty}^{+\infty} \exp(-x^2)~dx = \sqrt{\pi} \int_{-\infty}^{+\infty} \frac{1}{\sqrt{\pi}}\times \exp(-x^2)~dx = \sqrt{\pi}
\end{aligned}
$$

> > Simulation Code:

``` r
f <- function(u) {
    temp1 <- 1 / (2 * sqrt(1/u - 1))
    temp2 <- 1/u**2 * exp((u - 1)/u)
    temp3 <- 2 * temp1 * temp2
    return(temp3)
}



R_Integrate_Estimate <- integrate(f, 0, 1)$value
Exact_Result <- sqrt(pi)

## Simulation Approach 

n <- 1e+5
U <- runif(n)
simulation_value <- mean(f(U))

c(R_Integrate_Estimate = R_Integrate_Estimate, Exact_Soloution = Exact_Result, Simulation_value = simulation_value)
```

    R_Integrate_Estimate      Exact_Soloution     Simulation_value 
                1.772454             1.772454             1.766754 
