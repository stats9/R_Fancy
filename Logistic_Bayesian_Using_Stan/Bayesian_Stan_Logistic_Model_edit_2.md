# Using stan \| bayesian Model for Logistic regression

## load libraries —————-

``` r
## for load rstan package
if (!require(rstan)) {
    install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
    library(rstan)
}




## for parallel calculation 
options(mc.cores = parallel :: detectCores())

## for adjust run programming
rstan_options(auto_write = TRUE)




## for load tidyverse packages, (manipulate data and use ggplot objects)
if (!require(tidyverse)) {
    chooseCRANmirror(graphics = FALSE, ind = 1)
    install.packages("tidyverse")
    library(tidyverse)
}


## for feature selection 

if(!require(mlbench)) {
    chooseCRANmirror(graphics = FALSE, ind = 1)
    install.packages("mlbench")
    library(mlbench)
}

if(!require(caret)) {
    chooseCRANmirror(graphics = FALSE, ind = 1)
    install.packages("caret")
    library(caret)
}
```

------------------------------------------------------------------------

------------------------------------------------------------------------

#### load data ——————-

``` r
set.seed(132)

dat <- read.csv(file = "diabetes_binary_5050split_health_indicators_BRFSS2015.csv", header = TRUE)

names(dat)
```

     [1] "Diabetes_binary"      "HighBP"               "HighChol"            
     [4] "CholCheck"            "BMI"                  "Smoker"              
     [7] "Stroke"               "HeartDiseaseorAttack" "PhysActivity"        
    [10] "Fruits"               "Veggies"              "HvyAlcoholConsump"   
    [13] "AnyHealthcare"        "NoDocbcCost"          "GenHlth"             
    [16] "MentHlth"             "PhysHlth"             "DiffWalk"            
    [19] "Sex"                  "Age"                  "Education"           
    [22] "Income"              

``` r
n <- 3e+3
ind <- sample(nrow(dat), size = n, replace = FALSE)
new_dat <- dat |> 
                slice(ind)
y <- new_dat$Diabetes_binary |> unlist()
p <- 0.8
train <- createDataPartition(y, p, 
            list = FALSE, times = 1)
name_cols <- new_dat |> names()
name_cols
```

     [1] "Diabetes_binary"      "HighBP"               "HighChol"            
     [4] "CholCheck"            "BMI"                  "Smoker"              
     [7] "Stroke"               "HeartDiseaseorAttack" "PhysActivity"        
    [10] "Fruits"               "Veggies"              "HvyAlcoholConsump"   
    [13] "AnyHealthcare"        "NoDocbcCost"          "GenHlth"             
    [16] "MentHlth"             "PhysHlth"             "DiffWalk"            
    [19] "Sex"                  "Age"                  "Education"           
    [22] "Income"              

``` r
X <- new_dat |> 
       dplyr :: select(name_cols[-1])

X |> names()
```

     [1] "HighBP"               "HighChol"             "CholCheck"           
     [4] "BMI"                  "Smoker"               "Stroke"              
     [7] "HeartDiseaseorAttack" "PhysActivity"         "Fruits"              
    [10] "Veggies"              "HvyAlcoholConsump"    "AnyHealthcare"       
    [13] "NoDocbcCost"          "GenHlth"              "MentHlth"            
    [16] "PhysHlth"             "DiffWalk"             "Sex"                 
    [19] "Age"                  "Education"            "Income"              

``` r
yTrain <- y[train]
yTest <- y[-train]


xScale <- apply(X, 2, function(x) {
    temp <- (x - mean(x)) / sd(x)
    return(temp)
})
colMeans(xScale)
```

                  HighBP             HighChol            CholCheck 
           -5.218048e-18        -3.608225e-17         1.658673e-16 
                     BMI               Smoker               Stroke 
            2.076626e-16        -1.016964e-16         4.229950e-17 
    HeartDiseaseorAttack         PhysActivity               Fruits 
           -4.773959e-18        -1.328567e-17        -2.457294e-17 
                 Veggies    HvyAlcoholConsump        AnyHealthcare 
            3.293662e-17         1.520080e-17         2.183439e-17 
             NoDocbcCost              GenHlth             MentHlth 
           -2.386980e-17        -6.056267e-17         5.219436e-17 
                PhysHlth             DiffWalk                  Sex 
            4.141132e-17        -4.644433e-17        -8.422892e-17 
                     Age            Education               Income 
           -1.395828e-16        -1.420276e-16         1.521468e-16 

``` r
apply(xScale, 2, sd)
```

                  HighBP             HighChol            CholCheck 
                       1                    1                    1 
                     BMI               Smoker               Stroke 
                       1                    1                    1 
    HeartDiseaseorAttack         PhysActivity               Fruits 
                       1                    1                    1 
                 Veggies    HvyAlcoholConsump        AnyHealthcare 
                       1                    1                    1 
             NoDocbcCost              GenHlth             MentHlth 
                       1                    1                    1 
                PhysHlth             DiffWalk                  Sex 
                       1                    1                    1 
                     Age            Education               Income 
                       1                    1                    1 

``` r
xTrain <- xScale |> _[train, ]
xTest <- xScale |> _[-train, ] 


xTrain <- xTrain  |> as.data.frame()
xTest <- xTest |> as.data.frame()

xTrain_mat <- model.matrix(~ ., data = xTrain)[, -1]
xTest_mat <- model.matrix(~ ., data = xTest)[, -1]


# xTrain_mat |> dim()
```

------------------------------------------------------------------------

------------------------------------------------------------------------

#### fit stan models

``` r
stanFit1 <- stan(file = "model_3_update.stan", 
data = list(N = nrow(xTrain), L = nrow(xTest), X_test = xTest_mat,
        k = ncol(xTrain), y = yTrain,  X_train = xTrain_mat), iter = 500, chains = 3)

# saveRDS(stanFit1, "Model3.RDS")

plot(stanFit1)
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-3-1.png)

``` r
plot(stanFit1, pars = c("alpha", "beta"))
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-3-2.png)

``` r
traceplot(stanFit1, pars = c("alpha", "beta"))
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-3-3.png)

``` r
# ext_fit <- rstan :: extract(stanFit1)


# # Accuracy
# mean(apply(ext_fit$y_test, 2, median) == yTest)
```

------------------------------------------------------------------------

------------------------------------------------------------------------

``` r
# stanFit1 <- readRDS("Model3.RDS")
```

------------------------------------------------------------------------

------------------------------------------------------------------------

``` r
#| warning: false
#| message: false
#| fig-height: 12
#| fig-width: 12


plot(stanFit1, pars = c("alpha", "beta"))
```

    ci_level: 0.8 (80% intervals)

    outer_level: 0.95 (95% intervals)

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-5-1.png)

``` r
traceplot(stanFit1, pars = c("alpha", "beta"))
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-5-2.png)

``` r
ext_fit <- rstan :: extract(stanFit1)


# Accuracy
mean(apply(ext_fit$y_test, 2, median) == yTest)
```

    [1] 0.7516667

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

## Using brms

``` r
set.seed(1234)
if (!require(brms)) {
    chooseCRANmirror(graphics = FALSE, ind = 1)
    install.packages("brms")
    library(brms)
}

if (!require(bayesplot)) {
    chooseCRANmirror(graphics = FALSE, ind = 1)
    install.packages("bayesplot")
    library(bayesplot)
}


new_dat2 <- cbind(yTrain, xTrain) |> 
                setNames(new_dat |> names())
names(new_dat2)
```

     [1] "Diabetes_binary"      "HighBP"               "HighChol"            
     [4] "CholCheck"            "BMI"                  "Smoker"              
     [7] "Stroke"               "HeartDiseaseorAttack" "PhysActivity"        
    [10] "Fruits"               "Veggies"              "HvyAlcoholConsump"   
    [13] "AnyHealthcare"        "NoDocbcCost"          "GenHlth"             
    [16] "MentHlth"             "PhysHlth"             "DiffWalk"            
    [19] "Sex"                  "Age"                  "Education"           
    [22] "Income"              

``` r
temp1 <- names(new_dat2)[-1]


temp2 <- paste(temp1, collapse = " + ")
form <- paste(names(new_dat2)[1], temp2, sep = " ~ ")

## define Priors 


Model4 <- brm(as.formula(form), family = bernoulli, 
            data = new_dat2)

saveRDS(Model4, "brms_Model4")
Model4 <- readRDS("brms_Model4")
prior_summary(Model4)
```

                    prior     class                 coef group resp dpar nlpar lb
                   (flat)         b                                              
                   (flat)         b                  Age                         
                   (flat)         b        AnyHealthcare                         
                   (flat)         b                  BMI                         
                   (flat)         b            CholCheck                         
                   (flat)         b             DiffWalk                         
                   (flat)         b            Education                         
                   (flat)         b               Fruits                         
                   (flat)         b              GenHlth                         
                   (flat)         b HeartDiseaseorAttack                         
                   (flat)         b               HighBP                         
                   (flat)         b             HighChol                         
                   (flat)         b    HvyAlcoholConsump                         
                   (flat)         b               Income                         
                   (flat)         b             MentHlth                         
                   (flat)         b          NoDocbcCost                         
                   (flat)         b         PhysActivity                         
                   (flat)         b             PhysHlth                         
                   (flat)         b                  Sex                         
                   (flat)         b               Smoker                         
                   (flat)         b               Stroke                         
                   (flat)         b              Veggies                         
     student_t(3, 0, 2.5) Intercept                                              
     ub       source
             default
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
        (vectorized)
             default

``` r
prob <- predict(Model4, xTest)[, 1]
conf_mat <- table((prob > 0.5), yTest)
conf_mat
```

           yTest
              0   1
      FALSE 231  57
      TRUE   89 223

``` r
acc <- sum(diag(conf_mat)) / sum(conf_mat)
acc
```

    [1] 0.7566667

``` r
pp_check(Model4)
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-6-1.png)

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

#### Model 5 With Normal Prior

``` r
## define Priors 
priors <- c(
    prior_string("normal(0, 15)", class = "b"), 
    prior_string("normal(0, 20)", class = "Intercept") 
)

# set_prior(priors)

Model5 <- brm(as.formula(form),  data = new_dat2, 
            family = bernoulli, 
            prior = priors)

prior_summary(Model5)
```

             prior     class                 coef group resp dpar nlpar lb ub
     normal(0, 15)         b                                                 
     normal(0, 15)         b                  Age                            
     normal(0, 15)         b        AnyHealthcare                            
     normal(0, 15)         b                  BMI                            
     normal(0, 15)         b            CholCheck                            
     normal(0, 15)         b             DiffWalk                            
     normal(0, 15)         b            Education                            
     normal(0, 15)         b               Fruits                            
     normal(0, 15)         b              GenHlth                            
     normal(0, 15)         b HeartDiseaseorAttack                            
     normal(0, 15)         b               HighBP                            
     normal(0, 15)         b             HighChol                            
     normal(0, 15)         b    HvyAlcoholConsump                            
     normal(0, 15)         b               Income                            
     normal(0, 15)         b             MentHlth                            
     normal(0, 15)         b          NoDocbcCost                            
     normal(0, 15)         b         PhysActivity                            
     normal(0, 15)         b             PhysHlth                            
     normal(0, 15)         b                  Sex                            
     normal(0, 15)         b               Smoker                            
     normal(0, 15)         b               Stroke                            
     normal(0, 15)         b              Veggies                            
     normal(0, 20) Intercept                                                 
           source
             user
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
             user

``` r
prob2 <- predict(Model5, xTest)[, 1]
conf_mat2 <- table((prob2 > 0.5), yTest)
conf_mat2
```

           yTest
              0   1
      FALSE 230  60
      TRUE   90 220

``` r
acc2 <- sum(diag(conf_mat2)) / sum(conf_mat2)
acc2
```

    [1] 0.75

``` r
mcmc_plot(Model5, type = "trace")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-7-1.png)

``` r
mcmc_plot(Model5, type = "dens")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-7-2.png)

``` r
bayesplot :: pp_check(Model5, type = "error_hist")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-7-3.png)

``` r
bayesplot :: pp_check(Model5, type = "loo_pit")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-7-4.png)

``` r
bayesplot :: pp_check(Model5, type = "loo_pit_overlay")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-7-5.png)

``` r
bayesplot :: pp_check(Model5, type = "bars")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-7-6.png)

``` r
bayesplot :: pp_check(Model5, type = "dens_overlay")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-7-7.png)

``` r
waic(Model4)
```


    Computed from 4000 by 2400 log-likelihood matrix

              Estimate   SE
    elpd_waic  -1263.8 24.7
    p_waic        22.5  0.8
    waic        2527.6 49.4

``` r
waic(Model5)
```


    Computed from 4000 by 2400 log-likelihood matrix

              Estimate   SE
    elpd_waic  -1264.1 24.7
    p_waic        22.8  0.8
    waic        2528.2 49.4

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

#### Model 6 With Cauchy Prior

``` r
## define Priors 
priors2 <- c(
    prior_string("cauchy(0, 3)", class = "b"),
    prior_string("normal(0, 5)", class = "Intercept") 
)

# set_prior(priors)

Model6 <- brm(as.formula(form),  data = new_dat2, 
            family = bernoulli, 
            prior = priors2)

prior_summary(Model6)
```

            prior     class                 coef group resp dpar nlpar lb ub
     cauchy(0, 3)         b                                                 
     cauchy(0, 3)         b                  Age                            
     cauchy(0, 3)         b        AnyHealthcare                            
     cauchy(0, 3)         b                  BMI                            
     cauchy(0, 3)         b            CholCheck                            
     cauchy(0, 3)         b             DiffWalk                            
     cauchy(0, 3)         b            Education                            
     cauchy(0, 3)         b               Fruits                            
     cauchy(0, 3)         b              GenHlth                            
     cauchy(0, 3)         b HeartDiseaseorAttack                            
     cauchy(0, 3)         b               HighBP                            
     cauchy(0, 3)         b             HighChol                            
     cauchy(0, 3)         b    HvyAlcoholConsump                            
     cauchy(0, 3)         b               Income                            
     cauchy(0, 3)         b             MentHlth                            
     cauchy(0, 3)         b          NoDocbcCost                            
     cauchy(0, 3)         b         PhysActivity                            
     cauchy(0, 3)         b             PhysHlth                            
     cauchy(0, 3)         b                  Sex                            
     cauchy(0, 3)         b               Smoker                            
     cauchy(0, 3)         b               Stroke                            
     cauchy(0, 3)         b              Veggies                            
     normal(0, 5) Intercept                                                 
           source
             user
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
             user

``` r
prob3 <- predict(Model6, xTest)[, 1]
conf_mat3 <- table((prob3 > 0.5), yTest)
conf_mat3
```

           yTest
              0   1
      FALSE 230  58
      TRUE   90 222

``` r
acc3 <- sum(diag(conf_mat3)) / sum(conf_mat3)
acc3
```

    [1] 0.7533333

``` r
mcmc_plot(Model6, type = "trace")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-8-1.png)

``` r
mcmc_plot(Model6, type = "dens")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-8-2.png)

``` r
bayesplot :: pp_check(Model6, type = "error_hist")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-8-3.png)

``` r
bayesplot :: pp_check(Model6, type = "loo_pit")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-8-4.png)

``` r
bayesplot :: pp_check(Model6, type = "loo_pit_overlay")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-8-5.png)

``` r
bayesplot :: pp_check(Model6, type = "bars")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-8-6.png)

``` r
bayesplot :: pp_check(Model6, type = "dens_overlay")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-8-7.png)

``` r
waic(Model6)
```


    Computed from 4000 by 2400 log-likelihood matrix

              Estimate   SE
    elpd_waic  -1263.8 24.7
    p_waic        22.5  0.8
    waic        2527.6 49.3

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

#### Model 7 With t studnet Prior

``` r
## define Priors 
priors3 <- c(
    prior_string("student_t(10, 0, 4)", class = "b"),
    prior_string("cauchy(0, 5)", class = "Intercept") 
)

# set_prior(priors)

Model7 <- brm(as.formula(form),  data = new_dat2, 
            family = bernoulli, 
            prior = priors3)

prior_summary(Model7)
```

                   prior     class                 coef group resp dpar nlpar lb ub
     student_t(10, 0, 4)         b                                                 
     student_t(10, 0, 4)         b                  Age                            
     student_t(10, 0, 4)         b        AnyHealthcare                            
     student_t(10, 0, 4)         b                  BMI                            
     student_t(10, 0, 4)         b            CholCheck                            
     student_t(10, 0, 4)         b             DiffWalk                            
     student_t(10, 0, 4)         b            Education                            
     student_t(10, 0, 4)         b               Fruits                            
     student_t(10, 0, 4)         b              GenHlth                            
     student_t(10, 0, 4)         b HeartDiseaseorAttack                            
     student_t(10, 0, 4)         b               HighBP                            
     student_t(10, 0, 4)         b             HighChol                            
     student_t(10, 0, 4)         b    HvyAlcoholConsump                            
     student_t(10, 0, 4)         b               Income                            
     student_t(10, 0, 4)         b             MentHlth                            
     student_t(10, 0, 4)         b          NoDocbcCost                            
     student_t(10, 0, 4)         b         PhysActivity                            
     student_t(10, 0, 4)         b             PhysHlth                            
     student_t(10, 0, 4)         b                  Sex                            
     student_t(10, 0, 4)         b               Smoker                            
     student_t(10, 0, 4)         b               Stroke                            
     student_t(10, 0, 4)         b              Veggies                            
            cauchy(0, 5) Intercept                                                 
           source
             user
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
             user

``` r
prob4 <- predict(Model7, xTest)[, 1]
conf_mat4 <- table((prob4 > 0.5), yTest)
conf_mat4
```

           yTest
              0   1
      FALSE 231  56
      TRUE   89 224

``` r
acc4 <- sum(diag(conf_mat4)) / sum(conf_mat4)
acc4
```

    [1] 0.7583333

``` r
mcmc_plot(Model7, type = "trace")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-9-1.png)

``` r
mcmc_plot(Model7, type = "dens")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-9-2.png)

``` r
bayesplot :: pp_check(Model7, type = "error_hist")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-9-3.png)

``` r
bayesplot :: pp_check(Model7, type = "loo_pit")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-9-4.png)

``` r
bayesplot :: pp_check(Model7, type = "loo_pit_overlay")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-9-5.png)

``` r
bayesplot :: pp_check(Model7, type = "bars")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-9-6.png)

``` r
bayesplot :: pp_check(Model7, type = "dens_overlay")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-9-7.png)

``` r
waic(Model7)
```


    Computed from 4000 by 2400 log-likelihood matrix

              Estimate   SE
    elpd_waic  -1264.0 24.7
    p_waic        22.7  0.8
    waic        2527.9 49.4

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

#### Model 8 Using Logistic For Prior

``` r
## define Priors 
priors4 <- c(
    prior_string("logistic(0, 3)", class = "b"),
    prior_string("normal(0, 5)", class = "Intercept") 
)

# set_prior(priors)

Model8 <- brm(as.formula(form),  data = new_dat2, 
            family = bernoulli, 
            prior = priors4)

prior_summary(Model8)
```

              prior     class                 coef group resp dpar nlpar lb ub
     logistic(0, 3)         b                                                 
     logistic(0, 3)         b                  Age                            
     logistic(0, 3)         b        AnyHealthcare                            
     logistic(0, 3)         b                  BMI                            
     logistic(0, 3)         b            CholCheck                            
     logistic(0, 3)         b             DiffWalk                            
     logistic(0, 3)         b            Education                            
     logistic(0, 3)         b               Fruits                            
     logistic(0, 3)         b              GenHlth                            
     logistic(0, 3)         b HeartDiseaseorAttack                            
     logistic(0, 3)         b               HighBP                            
     logistic(0, 3)         b             HighChol                            
     logistic(0, 3)         b    HvyAlcoholConsump                            
     logistic(0, 3)         b               Income                            
     logistic(0, 3)         b             MentHlth                            
     logistic(0, 3)         b          NoDocbcCost                            
     logistic(0, 3)         b         PhysActivity                            
     logistic(0, 3)         b             PhysHlth                            
     logistic(0, 3)         b                  Sex                            
     logistic(0, 3)         b               Smoker                            
     logistic(0, 3)         b               Stroke                            
     logistic(0, 3)         b              Veggies                            
       normal(0, 5) Intercept                                                 
           source
             user
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
             user

``` r
prob5 <- predict(Model8, xTest)[, 1]
conf_mat5 <- table((prob5 > 0.5), yTest)
conf_mat5
```

           yTest
              0   1
      FALSE 231  58
      TRUE   89 222

``` r
acc5 <- sum(diag(conf_mat5)) / sum(conf_mat5)
acc5
```

    [1] 0.755

``` r
mcmc_plot(Model8, type = "trace")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-10-1.png)

``` r
mcmc_plot(Model8, type = "dens")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-10-2.png)

``` r
bayesplot :: pp_check(Model8, type = "error_hist")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-10-3.png)

``` r
bayesplot :: pp_check(Model8, type = "loo_pit")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-10-4.png)

``` r
bayesplot :: pp_check(Model8, type = "loo_pit_overlay")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-10-5.png)

``` r
bayesplot :: pp_check(Model8, type = "bars")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-10-6.png)

``` r
bayesplot :: pp_check(Model8, type = "dens_overlay")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-10-7.png)

``` r
waic(Model8)
```


    Computed from 4000 by 2400 log-likelihood matrix

              Estimate   SE
    elpd_waic  -1264.3 24.7
    p_waic        22.9  0.8
    waic        2528.5 49.4

------------------------------------------------------------------------

------------------------------------------------------------------------

------------------------------------------------------------------------

#### Model

``` r
## define Priors 
priors5 <- c( 
    prior_string("normal(0, 0.5)", class = "b"),
    prior_string("normal(0, 5)", class = "Intercept") 
)

# set_prior(priors)

Model9 <- brm(as.formula(form),  data = new_dat2, 
            family = bernoulli, 
            prior = priors5)

prior_summary(Model9)
```

              prior     class                 coef group resp dpar nlpar lb ub
     normal(0, 0.5)         b                                                 
     normal(0, 0.5)         b                  Age                            
     normal(0, 0.5)         b        AnyHealthcare                            
     normal(0, 0.5)         b                  BMI                            
     normal(0, 0.5)         b            CholCheck                            
     normal(0, 0.5)         b             DiffWalk                            
     normal(0, 0.5)         b            Education                            
     normal(0, 0.5)         b               Fruits                            
     normal(0, 0.5)         b              GenHlth                            
     normal(0, 0.5)         b HeartDiseaseorAttack                            
     normal(0, 0.5)         b               HighBP                            
     normal(0, 0.5)         b             HighChol                            
     normal(0, 0.5)         b    HvyAlcoholConsump                            
     normal(0, 0.5)         b               Income                            
     normal(0, 0.5)         b             MentHlth                            
     normal(0, 0.5)         b          NoDocbcCost                            
     normal(0, 0.5)         b         PhysActivity                            
     normal(0, 0.5)         b             PhysHlth                            
     normal(0, 0.5)         b                  Sex                            
     normal(0, 0.5)         b               Smoker                            
     normal(0, 0.5)         b               Stroke                            
     normal(0, 0.5)         b              Veggies                            
       normal(0, 5) Intercept                                                 
           source
             user
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
     (vectorized)
             user

``` r
prob6 <- predict(Model9, xTest)[, 1]
conf_mat6 <- table((prob6 > 0.5), yTest)
conf_mat6
```

           yTest
              0   1
      FALSE 228  58
      TRUE   92 222

``` r
acc6 <- sum(diag(conf_mat6)) / sum(conf_mat6)
acc6
```

    [1] 0.75

``` r
mcmc_plot(Model9, type = "trace")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-11-1.png)

``` r
mcmc_plot(Model9, type = "dens")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-11-2.png)

``` r
bayesplot :: pp_check(Model9, type = "error_hist")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-11-3.png)

``` r
bayesplot :: pp_check(Model9, type = "loo_pit")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-11-4.png)

``` r
bayesplot :: pp_check(Model9, type = "loo_pit_overlay")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-11-5.png)

``` r
bayesplot :: pp_check(Model9, type = "bars")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-11-6.png)

``` r
bayesplot :: pp_check(Model9, type = "dens_overlay")
```

![](Bayesian_Stan_Logistic_Model_edit_2_files/figure-commonmark/unnamed-chunk-11-7.png)

``` r
waic(Model9)
```


    Computed from 4000 by 2400 log-likelihood matrix

              Estimate   SE
    elpd_waic  -1264.0 24.5
    p_waic        22.7  0.8
    waic        2528.1 49.0

#### Simple Model

``` r
library(MASS)
Model10 <- glm(as.formula(form), data = new_dat2, 
                family = binomial(link = "logit"))
summary(Model10)
```


    Call:
    glm(formula = as.formula(form), family = binomial(link = "logit"), 
        data = new_dat2)

    Coefficients:
                          Estimate Std. Error z value Pr(>|z|)    
    (Intercept)          -0.091084   0.050411  -1.807 0.070789 .  
    HighBP                0.350586   0.053306   6.577 4.80e-11 ***
    HighChol              0.396568   0.051939   7.635 2.25e-14 ***
    CholCheck             0.246171   0.072266   3.406 0.000658 ***
    BMI                   0.555215   0.063199   8.785  < 2e-16 ***
    Smoker                0.066918   0.050342   1.329 0.183757    
    Stroke                0.040802   0.053888   0.757 0.448957    
    HeartDiseaseorAttack  0.015543   0.053003   0.293 0.769338    
    PhysActivity         -0.084090   0.052386  -1.605 0.108450    
    Fruits               -0.004027   0.051517  -0.078 0.937698    
    Veggies              -0.016718   0.051777  -0.323 0.746780    
    HvyAlcoholConsump    -0.147454   0.051257  -2.877 0.004018 ** 
    AnyHealthcare        -0.079702   0.052238  -1.526 0.127076    
    NoDocbcCost           0.057866   0.054918   1.054 0.292028    
    GenHlth               0.495335   0.068030   7.281 3.31e-13 ***
    MentHlth              0.022383   0.057098   0.392 0.695049    
    PhysHlth             -0.057475   0.062969  -0.913 0.361375    
    DiffWalk             -0.005408   0.059997  -0.090 0.928172    
    Sex                   0.155824   0.051797   3.008 0.002627 ** 
    Age                   0.447667   0.060386   7.413 1.23e-13 ***
    Education            -0.024015   0.058056  -0.414 0.679126    
    Income               -0.107201   0.061271  -1.750 0.080184 .  
    ---
    Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    (Dispersion parameter for binomial family taken to be 1)

        Null deviance: 3326.6  on 2399  degrees of freedom
    Residual deviance: 2482.9  on 2378  degrees of freedom
    AIC: 2526.9

    Number of Fisher Scoring iterations: 5

``` r
prob7 <- predict(Model10, xTest, type = "response") 
conf_mat7 <- table((prob7 > 0.5), yTest)
conf_mat7
```

           yTest
              0   1
      FALSE 231  55
      TRUE   89 225

``` r
acc7 <- sum(diag(conf_mat7)) / sum(conf_mat7)
acc7
```

    [1] 0.76

``` r
# install.packages("car")
AIC(Model10)
```

    [1] 2526.888

``` r
car :: vif(Model10)
```

                  HighBP             HighChol            CholCheck 
                1.150299             1.104845             1.019568 
                     BMI               Smoker               Stroke 
                1.129176             1.048307             1.073928 
    HeartDiseaseorAttack         PhysActivity               Fruits 
                1.149534             1.136907             1.099631 
                 Veggies    HvyAlcoholConsump        AnyHealthcare 
                1.125148             1.029073             1.108423 
             NoDocbcCost              GenHlth             MentHlth 
                1.164205             1.560757             1.244828 
                PhysHlth             DiffWalk                  Sex 
                1.593076             1.434319             1.112627 
                     Age            Education               Income 
                1.284108             1.359214             1.520516 
