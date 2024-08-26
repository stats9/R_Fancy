# template code for automate parameterized


- [create a dataframe for quarto
  yaml](#create-a-dataframe-for-quarto-yaml)
- [map over qmd template code](#map-over-qmd-template-code)

# create a dataframe for quarto yaml

``` r
data <- expand.grid(
    year = c(2006, 2020),
    color = c("red", 'darkgreen'), 
    stringsAsFactors = FALSE)


data2 <- data|> 
    dplyr :: mutate(
    output_format = "gfm", 
    output_file = paste(
        year, color, "report.md", 
        sep = "-"),
    execute_params = 
    purrr :: map2(color, year, \(color, year) list(color = color, year = year))
     ) |> 
    dplyr :: select(-c(color, year))
```

# map over qmd template code

``` r
purrr :: pwalk(
    .l = data2, ## dataframe for map over
    .f = quarto :: quarto_render, ## function that we want mapping on dataframe
    input = "quarto_Paremerized.qmd", ## qmd template 
    .progress = TRUE
)
```



    processing file: quarto_Paremerized.qmd
    1/13                  
    2/13 [unnamed-chunk-1]
    3/13                  
    4/13 [unnamed-chunk-2]
    5/13                  
    6/13 [unnamed-chunk-3]
    7/13                  
    8/13 [figure chunk]   
    9/13                  
    10/13 [unnamed-chunk-5]
    11/13                  
    12/13 [unnamed-chunk-6]
    13/13                  
    output file: quarto_Paremerized.knit.md

    pandoc --output 2006-red-report.md
      to: >-
        commonmark+autolink_bare_uris+emoji+footnotes+gfm_auto_identifiers+pipe_tables+strikeout+task_lists+tex_math_dollars
      from: markdown+gfm_auto_identifiers
      standalone: true
      default-image-extension: png
      toc: true
      
    metadata
      title: How to create parameterized report
      message: false
      
    Output created: 2006-red-report.md



    processing file: quarto_Paremerized.qmd
    1/13                  
    2/13 [unnamed-chunk-1]
    3/13                  
    4/13 [unnamed-chunk-2]
    5/13                  
    6/13 [unnamed-chunk-3]
    7/13                  
    8/13 [figure chunk]   
    9/13                  
    10/13 [unnamed-chunk-5]
    11/13                  
    12/13 [unnamed-chunk-6]
    13/13                  
    output file: quarto_Paremerized.knit.md

    pandoc --output 2020-red-report.md
      to: >-
        commonmark+autolink_bare_uris+emoji+footnotes+gfm_auto_identifiers+pipe_tables+strikeout+task_lists+tex_math_dollars
      from: markdown+gfm_auto_identifiers
      standalone: true
      default-image-extension: png
      toc: true
      
    metadata
      title: How to create parameterized report
      message: false
      
    Output created: 2020-red-report.md



    processing file: quarto_Paremerized.qmd
    1/13                  
    2/13 [unnamed-chunk-1]
    3/13                  
    4/13 [unnamed-chunk-2]
    5/13                  
    6/13 [unnamed-chunk-3]
    7/13                  
    8/13 [figure chunk]   
    9/13                  
    10/13 [unnamed-chunk-5]
    11/13                  
    12/13 [unnamed-chunk-6]
    13/13                  
    output file: quarto_Paremerized.knit.md

    pandoc --output 2006-darkgreen-report.md
      to: >-
        commonmark+autolink_bare_uris+emoji+footnotes+gfm_auto_identifiers+pipe_tables+strikeout+task_lists+tex_math_dollars
      from: markdown+gfm_auto_identifiers
      standalone: true
      default-image-extension: png
      toc: true
      
    metadata
      title: How to create parameterized report
      message: false
      
    Output created: 2006-darkgreen-report.md



    processing file: quarto_Paremerized.qmd
    1/13                  
    2/13 [unnamed-chunk-1]
    3/13                  
    4/13 [unnamed-chunk-2]
    5/13                  
    6/13 [unnamed-chunk-3]
    7/13                  
    8/13 [figure chunk]   
    9/13                  
    10/13 [unnamed-chunk-5]
    11/13                  
    12/13 [unnamed-chunk-6]
    13/13                  
    output file: quarto_Paremerized.knit.md

    pandoc --output 2020-darkgreen-report.md
      to: >-
        commonmark+autolink_bare_uris+emoji+footnotes+gfm_auto_identifiers+pipe_tables+strikeout+task_lists+tex_math_dollars
      from: markdown+gfm_auto_identifiers
      standalone: true
      default-image-extension: png
      toc: true
      
    metadata
      title: How to create parameterized report
      message: false
      
    Output created: 2020-darkgreen-report.md
