# Simulation of the probability space of the maximum or minimum value
for simultaneous throwing of several dice

## In this text, we want to simulate the probability space for the maximum value or the minimum value obtained by throwing several dices (one or more).

<br><br>

#### Total Probablity Space $\Omega$ for two dices

$$
\Omega = \left\{
\begin{aligned}
& (1, 1), ~(1, 2), ~(1, 3), ~(1, 4), ~(1, 5),~(1, 6), \\
& (2, 1),~(2, 2)~, (2, 3),~(2, 4),~(2, 5),~(2, 6), \\
& (3, 1),~(3, 2),~(3, 3),~(3, 4),~(3, 5),~(3, 6), \\
& (4, 1), ~(4, 2),~(4, 3),~(4, 4),~(4, 5),~(4, 6), \\
& (5, 1), ~(5, 2), ~(5, 3), ~(5, 4),~(5,5), ~(5, 6), \\
& (6, 1), ~(6, 2), ~(6, 3), ~(6, 4), ~(6, 5), ~(6, 6) \\
\end{aligned}
\right\} \implies 
$$

#### for example:

$$
\text{Pr}\left(\max(X, Y) = z\right) = \begin{cases} 
\frac{1}{36} & \text{if}\quad z = 1,  \\
\frac{3}{36} & \text{if} \quad z = 2, \\
\frac{5}{36} & \text{if} \quad z = 3, \\
\frac{7}{36} & \text{if} \quad z = 4, \\
\frac{9}{36} & \text{if} \quad z = 5, \\
\frac{11}{36} & \text{if} \quad z = 6.
\end{cases} \implies 
$$

$$
\text{Pr}(\max(X, Y) \geq 5) = \text{Pr}(\max(X, Y) = 5) + \text{Pr}(\max(X, Y) = 6) = \frac{9}{36} + \frac{11}{36} = \frac{20}{36} = 0.5555556 
$$

<br><br>

<hr>
<hr>

<br><br>

#### Simulate with R programming:

``` r
sim_fun <- function(nSim = 1e+3, numDice = 2, num = 5, level = "max",
                        operator = ">=") {
    if (!(level %in% c("max", "min"))) stop("level must be max or min")
    if (!(operator %in% c(">", ">=", "<=", "<", "=="))) 
        stop("operator must be in c(>, >=, <=, <, ==)")
    mat_sim <- matrix(sample(1:6, size = nSim * numDice, replace = TRUE), 
                nrow = nSim, ncol = numDice)
    temp1 <- apply(mat_sim, MARGIN = 1, FUN = function(x) get(level)(x))
    temp2 <- get(operator)(temp1, num) 
    return(mean(temp2))
}

result <- sim_fun(nSim = 1e+6, numDice = 2, num = 5, 
            level = "max", operator = ">=")
sprintf("Pr(max(x, y) >= 5) = %.4f", result)
```

    [1] "Pr(max(x, y) >= 5) = 0.5562"
