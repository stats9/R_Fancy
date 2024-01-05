f <- function(n, lamb) {
    U <- runif(n)
    Y <- -lamb * log(U)
    return(Y)
}

n <- 1e+4
lambda = 2
yy <- f(n, lambda)

Mean <- mean(yy)
Var <- var(yy)

cat("Mean Sim data: ", Mean, "\n", "Variance Sim data: ", Var, "\n", sep = "")
n <- 1e+5
yy2 <- f(n, lambda)
Mean <- mean(yy2)
Var <- var(yy2)

cat("Mean Sim data: ", Mean, "\n", "Variance Sim data: ", Var, "\n", sep = "")


library(httpgd); hgd(); hgd_browse()

hist(yy, freq = FALSE, xlim = c(0, 10))
hist(yy, breaks = 16, xlim = c(0, max(yy)), ylim = c(0, 0.5), freq = FALSE)
curve(dexp(x, rate = 1/2), 0, max(yy), lwd = 2, col = "red", add = TRUE)



############################

chi_sim <- function(v) {
    U <- runif(v)
    return(-2 * sum(log(U)))
}

n <- 1e+5

chi_sim_observ <- replicate(n, chi_sim(5))
head(chi_sim_observ)


cat("Mean sim: ", mean(chi_sim_observ), "\n", 
    "Var sim: ", var(chi_sim_observ), "\n", sep = "")


hist(chi_sim_observ, freq = FALSE, breaks = 16)
curve(dchisq(x, df = 10), 0, max(chi_sim_observ), lwd = 2, col = "red", add = TRUE)


######################

gamma_sim <- function(a, b) {
    U <- runif(a)
    return(-b * sum(log(U)))
}


n <- 1e+5
a <- 10; b <- 3
gamma_observ <- replicate(n, gamma_sim(a, b))
head(gamma_observ)


cat("Mean sim: ", mean(gamma_observ), "\n", 
    "Var sim: ", var(gamma_observ), "\n", sep = "")


hist(gamma_observ, freq = FALSE, breaks = 16)
curve(dgamma(x, shape = a, scale = b), 0, max(gamma_observ), lwd = 2, col = "red", add = TRUE)


######################

beta_sim <- function(a, b) {
    U <- runif(a + b)
    temp <- log(U)
    temp2 <- sum(temp[1:a])
    return(temp2 / sum(temp))
}

n <- 1e+5
a <- 10; b <- 3
beta_observ <- replicate(n, beta_sim(a, b))
head(beta_observ)


cat("Mean sim: ", mean(beta_observ), "\n", 
    "Var sim: ", var(beta_observ), "\n", sep = "")


hist(beta_observ, freq = FALSE, breaks = 16)
curve(dbeta(x, shape1 = a, shape2 = b), 0, max(beta_observ), lwd = 2, col = "red", add = TRUE)



######### Box Muller algorithm -------


boxMuller <- function(n, mu = 0, sig = 1) {
    U <- runif(n)
    V <- runif(n)
    X <- sqrt(-2 * log(U)) * cos(2 * pi * V)
    Y <- sqrt(-2 * log(U)) * sin(2 * pi * V)
    return(list(X = sig * X + mu, Y = sig * Y + mu))
}

n <- 1e+4

res1 <- boxMuller(n = n)

cat("Mean X: ", mean(res1$X), "\n", "Variance X: ", var(res1$X), "\n", 
    "Mean Y: ", mean(res1$Y), "\n", "Variance Y: ", var(res1$Y), "\n", 
    "Corr(X, Y): ", cor(res1$X, res1$Y), "\n", sep = "")


hist(res1$X, freq = FALSE, breaks = 16)
curve(dnorm(x), min(res1$X), max(res1$X), lwd = 2, col = "red", add = TRUE)


hist(res1$Y, freq = FALSE, breaks = 16)
curve(dnorm(x), min(res1$Y), max(res1$Y), lwd = 2, col = "red", add = TRUE)




res1 <- boxMuller(n = n, mu = -10, sig = 0.1)

cat("Mean X: ", mean(res1$X), "\n", "Variance X: ", var(res1$X), "\n", 
    "Mean Y: ", mean(res1$Y), "\n", "Variance Y: ", var(res1$Y), "\n", 
    "Corr(X, Y): ", cor(res1$X, res1$Y), "\n", sep = "")


hist(res1$X, freq = FALSE, breaks = 16)
curve(dnorm(x, mean = -10, sd = 0.1), min(res1$X), max(res1$X), lwd = 2, col = "red", add = TRUE)


hist(res1$Y, freq = FALSE, breaks = 16)
curve(dnorm(x, mean = -10, sd = 0.1), min(res1$Y), max(res1$Y), lwd = 2, col = "red", add = TRUE)



######################### Categorical Data

xx <- 0:4
res1 <- pbinom(xx, size = 4, prob = 5/8)
res1

f_binom <- function(n) {
    U <- runif(n)
    fun <- function(x) {
        temp <- sum(x > res1)
        return(temp)
    }
    temp2 <- lapply(U, fun) |> unlist()
    return(temp2)
}

n <- 1e+5
ress <- f_binom(n)
head(ress)

cat("Mean binom sim: ", mean(ress), "\n", 
    "variance Binom sim : ", var(ress), "\n", 
     sep = "")


### Geometric Distribution

geom_sim <- function(n, p) {
    q <- 1 - p
    U <- runif(n)
    res1 <- log(U) / log(q)
    return(ceiling(res1))
} 

temp <- geom_sim(1e+5, p = 1/3)
cat("Mean sim: ", mean(temp), "\n", 
    "Variance sim: ", var(temp), "\n", sep = "")


temp <- geom_sim(1e+6, p = 1/5)
cat("Mean sim: ", mean(temp), "\n", 
    "Variance sim: ", var(temp), "\n", sep = "")
    

###########################################


## Poisson distribution

p_sim <- function(la = 18.69, n = 5e+3) {
    temp1 <- matrix(rpois(n = 13 * n, lambda = la), nrow = 13, ncol = n)
    ress1 <- apply(temp1, MARGIN = 1, FUN = function(x) return(var(x)))
    return(ress1)
}

ress <- p_sim()
ress |> head()

a <- 44.9
prob <- mean(ress > a)
prob



ress <- p_sim(n = 1e+6)
ress |> head()

a <- 44.9
prob <- mean(ress > a)
prob

hist(ress, freq = FALSE, col = "orange")

cat("Mean sim: ", mean(ress), "\n", 
    "Standard Deviation of sim: ", sd(ress), "\n", sep = "")
