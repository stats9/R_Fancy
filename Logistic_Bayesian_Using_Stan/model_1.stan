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
}

model {
    alpha ~ normal(0, 10);
    beta ~ normal(0, 10);
    
    for (n in 1:N)
       y[n] ~ bernoulli_logit(alpha + dot_product(beta, X_train[n, ]));
}
generated quantities {
   vector[L] y_test; 
   for(i in 1:L)
    y_test[i] = bernoulli_rng(inv_logit(alpha + dot_product(beta, X_test[i, ])));
}