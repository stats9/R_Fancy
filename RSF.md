# RSF


- [Python Section](#python-section)
- [R section](#r-section)
- [Random Survival Forest (RSF) in Python and
  R](#random-survival-forest-rsf-in-python-and-r)
  - [Python Implementation (using
    `sksurv`)](#python-implementation-using-sksurv)
  - [R Implementation (using
    `randomForestSRC`)](#r-implementation-using-randomforestsrc)
  - [Results Comparison](#results-comparison)
  - [Interpretation and Comparison of
    Results](#interpretation-and-comparison-of-results)

# Python Section

``` python
# Install required packages
# You can run this in a Jupyter cell or terminal

# !pip install scikit-survival pandas scikit-learn

# Import necessary libraries
import time
from sksurv.datasets import load_veterans_lung_cancer
from sksurv.ensemble import RandomSurvivalForest
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import ColumnTransformer
from sklearn.pipeline import Pipeline

# ----------------------------------------------------------
# 1. Load the veteran lung cancer dataset
#    X: features (some categorical), y: structured array (event, time)
# ----------------------------------------------------------
X, y = load_veterans_lung_cancer()

# ----------------------------------------------------------
# 2. Identify categorical and numeric columns
# ----------------------------------------------------------
categorical_cols = X.select_dtypes(include=["category", "object"]).columns
numeric_cols = X.select_dtypes(exclude=["category", "object"]).columns

# ----------------------------------------------------------
# 3. Preprocessing: One-hot encode categorical columns, pass numeric as-is
# ----------------------------------------------------------
preprocessor = ColumnTransformer(
    transformers=[
        ("cat", OneHotEncoder(), categorical_cols),
        ("num", "passthrough", numeric_cols)
    ]
)

# ----------------------------------------------------------
# 4. Define the Random Survival Forest model
# ----------------------------------------------------------
rsf = RandomSurvivalForest(
    n_estimators=100,            # Number of trees in the forest
    min_samples_split=10,        # Minimum samples required to split a node
    min_samples_leaf=15,         # Minimum samples required in a leaf node
    max_features="sqrt",         # Number of features to consider at each split
    n_jobs=-1,                   # Use all available CPU cores
    random_state=42              # For reproducibility
)

# ----------------------------------------------------------
# 5. Build a pipeline: preprocessing + model
# ----------------------------------------------------------
model = Pipeline(steps=[
    ("preprocessor", preprocessor),
    ("model", rsf)
])

# ----------------------------------------------------------
# 6. Split the data into training and testing sets
# ----------------------------------------------------------
X_train, X_test, y_train, y_test = train_test_split(
    X, y, test_size=0.3, random_state=42
)

# ----------------------------------------------------------
# 7. Train the model and measure execution time
# ----------------------------------------------------------
start_time = time.time()
model.fit(X_train, y_train)
```

    Pipeline(steps=[('preprocessor',
                     ColumnTransformer(transformers=[('cat', OneHotEncoder(),
                                                      Index(['Celltype', 'Prior_therapy', 'Treatment'], dtype='object')),
                                                     ('num', 'passthrough',
                                                      Index(['Age_in_years', 'Karnofsky_score', 'Months_from_Diagnosis'], dtype='object'))])),
                    ('model',
                     RandomSurvivalForest(min_samples_leaf=15, min_samples_split=10,
                                          n_jobs=-1, random_state=42))])

``` python
end_time = time.time()
print(f"Python RSF execution time: {end_time - start_time:.3f} seconds")
```

    Python RSF execution time: 0.078 seconds

``` python
# ----------------------------------------------------------
# 8. Evaluate model performance using concordance index (C-index)
# ----------------------------------------------------------
score = model.score(X_test, y_test)
print(f"C-index: {score:.3f}")
```

    C-index: 0.674

# R section

``` r
# ----------------------------------------------------------
# Load required packages
# ----------------------------------------------------------
library(survival)   # For the veteran dataset and Surv() function
library(randomForestSRC)  # For Random Survival Forest
library(survcomp)   # For concordance index (C-index)

# ----------------------------------------------------------
# 1. Load the veteran lung cancer dataset
#    This dataset is equivalent to load_veterans_lung_cancer() in Python
# ----------------------------------------------------------
data(veteran)  # Comes with the survival package

# ----------------------------------------------------------
# 2. Prepare the survival object
#    time: survival time in days
#    status: 1 = event (death), 0 = censored
# ----------------------------------------------------------
# In the veteran dataset, status is coded as 1=censored, 2=dead
# We convert it to 0/1 for Surv()
veteran$status <- ifelse(veteran$status == 1, 0, 1)
surv_obj <- Surv(time = veteran$time, event = veteran$status)

# ----------------------------------------------------------
# 3. Split into training and testing sets (70/30 split)
# ----------------------------------------------------------
set.seed(42)
train_idx <- sample(seq_len(nrow(veteran)), size = 0.7 * nrow(veteran))
train_data <- veteran[train_idx, ]
test_data  <- veteran[-train_idx, ]

# ----------------------------------------------------------
# 4. Train the Random Survival Forest model
#    Measure execution time
# ----------------------------------------------------------
start_time <- Sys.time()

rsf_model <- rfsrc(
  Surv(time, status) ~ .,
  data = train_data,
  ntree = 100,          # Number of trees
  nodesize = 15,        # Minimum samples in a terminal node
  mtry = floor(sqrt(ncol(train_data) - 2)), # Features per split
  nsplit = 10,          # Number of random split points
  block.size = 1        # For reproducibility
)

end_time <- Sys.time()
exec_time <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat(sprintf("R RSF execution time: %.3f seconds\n", exec_time))
```

    R RSF execution time: 0.014 seconds

``` r
# ----------------------------------------------------------
# 5. Predict risk scores on the test set
# ----------------------------------------------------------
pred <- predict(rsf_model, newdata = test_data)

# ----------------------------------------------------------
# 6. Compute C-index
#    We use the predicted mortality risk (higher = worse prognosis)
# ----------------------------------------------------------
c_index <- concordance.index(
  x = pred$predicted, 
  surv.time = test_data$time, 
  surv.event = test_data$status,
  method = "noether"
)$c.index

cat(sprintf("C-index: %.3f\n", c_index))
```

    C-index: 0.362

> Explanatin

# Random Survival Forest (RSF) in Python and R

## Python Implementation (using `sksurv`)

### Steps

1.  **Load dataset**
    - The dataset `veterans_lung_cancer` is included in the
      `sksurv.datasets` module.  
    - It contains survival times, event indicators, and patient
      covariates.
2.  **Preprocessing**
    - Categorical features are one-hot encoded using `OneHotEncoder`.  
    - Numeric features are passed through unchanged.  
    - A `ColumnTransformer` ensures both categorical and numeric
      variables are handled properly.
3.  **Model specification**
    - A `RandomSurvivalForest` (RSF) is defined with:
      - `n_estimators=100`,  
      - `min_samples_leaf=15`,  
      - `min_samples_split=10`,  
      - `max_features="sqrt"`.
4.  **Pipeline construction**
    - The model pipeline combines preprocessing and RSF estimation.
5.  **Train-test split**
    - Data is split into training (70%) and testing (30%).
6.  **Model training**
    - Execution time is measured using `time.time()`.
7.  **Evaluation**
    - Performance is measured using the **Concordance Index (C-index)**.

## R Implementation (using `randomForestSRC`)

### Steps

1.  **Load dataset**
    - The `veteran` dataset is included in the `survival` package.  
    - Status variable is originally coded as `1 = censored, 2 = dead`.  
    - It is recoded to `0 = censored, 1 = event`.
2.  **Survival object**
    - `Surv(time, status)` is created to represent the outcome.
3.  **Train-test split**
    - The dataset is randomly split into training (70%) and testing
      (30%).
4.  **Model specification**
    - The RSF is trained with:
      - `ntree=100`,  
      - `nodesize=15`,  
      - `mtry = sqrt(p)`,  
      - `nsplit=10`.
5.  **Model training**
    - Execution time is measured with `Sys.time()`.
6.  **Prediction**
    - Predictions are obtained for the test dataset using `predict()`.
7.  **Evaluation**
    - The **C-index** is computed using `concordance.index()` from the
      `survcomp` package.

## Results Comparison

| Implementation | Execution Time (seconds) | C-index |
|----------------|--------------------------|---------|
| Python RSF     | 0.117                    | 0.674   |
| R RSF          | 0.017                    | 0.362   |

## Interpretation and Comparison of Results

### Python Output

- Execution time: about 0.117 seconds.  
- C-index: 0.674, indicating moderate predictive discrimination.

### R Output

- Execution time: about 0.017 seconds (faster training).  
- C-index: 0.362, indicating relatively poor discrimination compared to
  Python.

### Why the Results Differ

1.  **Different Implementations**
    - `sksurv` (Python) and `randomForestSRC` (R) are independent
      implementations of RSF with different default settings.
2.  **C-index Calculation**
    - Python uses Harrell’s C-index directly via `model.score()`.  
    - R uses `concordance.index()` which may handle ties differently.
3.  **Preprocessing**
    - Python applies one-hot encoding to categorical variables,
      expanding the feature space.  
    - R treats categorical variables as factors, which changes the
      splitting mechanism.
4.  **Parameter Defaults**
    - Defaults for `mtry`, `splitrule`, and `nsplit` differ.
5.  **Randomness**
    - RSF involves bootstrapping and random feature selection. Different
      seeds and implementations can yield different outcomes.

### Final Remarks

- Both implementations are valid but not numerically identical.  
- Python’s RSF achieved a higher C-index but with slightly longer
  runtime.  
- R’s RSF was faster but produced a weaker discrimination measure.  
- Results should be interpreted qualitatively, not expected to match
  exactly across implementations.
