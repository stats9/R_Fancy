# Check Available CRAN Mirrors
Habib Ezzatabadi Pour
2025-07-24

``` r
# List of selected CRAN mirrors to check
mirrors <- c(
  Iran     = "https://cran.um.ac.ir",
  Turkey   = "https://cran.pau.edu.tr",
  UAE      = "https://cran.nyuad.nyu.edu",
  Global   = "https://cloud.r-project.org"
)

# Function to test connection and measure response time
check_mirror_status <- function(url) {
  start <- Sys.time()
  result <- tryCatch({
    readLines(url, n = 1)
    list(status = "Available", 
    latency = round(difftime(Sys.time(), start, units = "secs"), 3))
  }, error = function(e) {
    list(status = "Unavailable", latency = NA)
  })
  return(result)
}

# Apply to all mirrors and collect results
results <- lapply(mirrors, check_mirror_status)

# Convert to data frame
df_status <- data.frame(
  Mirror      = names(mirrors),
  URL         = unname(mirrors),
  Status      = sapply(results, `[[`, "status"),
  ResponseSec = sapply(results, `[[`, "latency")
)

# Print result table
print(df_status, row.names = FALSE)
```

     Mirror                         URL    Status ResponseSec
       Iran       https://cran.um.ac.ir Available       0.265
     Turkey     https://cran.pau.edu.tr Available       1.113
        UAE  https://cran.nyuad.nyu.edu Available      15.990
     Global https://cloud.r-project.org Available       2.226
