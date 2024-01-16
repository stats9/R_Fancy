data {
   int <lower = 0> N; // number of observation
   int <lower = 0> L; // number of test data
   int <lower = 0, upper = 1> y[N]; // binary response variable
   int<lower = 0> k; // number of predictors
   matrix[N, k] X_train; // predictor matrix
   matrix[L, k] X_test; // test data
}

parameters {
    real alpha; // intercept
    vector[k] beta; // coefficients for predictors
    real mu1; 
    real sigma1; 
    real mu2; 
    real sigma2; 
}

model {
    sigma1 ~ gamma(4, 2);
    sigma2 ~ gamma(4, 2);
    mu1 ~ normal(0, 10);
    mu2 ~ normal(0, 10);
    alpha ~ normal(mu1, sigma1);
    beta ~ normal(mu2, sigma2);
    
    for (n in 1:N)
       y[n] ~ bernoulli_logit(alpha + dot_product(beta, X_train[n, ]));
}
generated quantities {
   vector[L] y_test; 
   for(i in 1:L)
    y_test[i] = bernoulli_rng(inv_logit(alpha + dot_product(beta, X_test[i, ])));
}