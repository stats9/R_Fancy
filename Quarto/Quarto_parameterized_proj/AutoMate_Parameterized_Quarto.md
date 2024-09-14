# Automate Parameterized


- [create a dataframe for params](#create-a-dataframe-for-params)

# create a dataframe for params

``` r
dat <- expand.grid(color = c("darkblue", "darkgreen"), 
    year = c(2022, 2008), stringsAsFactors = FALSE)
# dat 

dat2 <- dat |> 
        dplyr :: mutate(output_format = "gfm", 
            output_file = paste(year, color, "report.md", sep = "-"), 
            execute_params = purrr :: map2(color, year, 
            \(color, year) list(color = color, year = year))) |> 
            dplyr :: select(-c(color, year))

dat2
```

      output_format              output_file  execute_params
    1           gfm  2022-darkblue-report.md  darkblue, 2022
    2           gfm 2022-darkgreen-report.md darkgreen, 2022
    3           gfm  2008-darkblue-report.md  darkblue, 2008
    4           gfm 2008-darkgreen-report.md darkgreen, 2008

------------------------------------------------------------------------

``` r
purrr :: pwalk(
    .l = dat2, 
    .f = quarto :: quarto_render, 
    input = "quarto_parameterized.qmd", 
    .progress = TRUE
)
```



    processing file: quarto_parameterized.qmd
    1/13                  
    2/13 [unnamed-chunk-1]
    3/13                  
    4/13 [unnamed-chunk-2]
    5/13                  
    6/13 [unnamed-chunk-3]
    7/13                  
    8/13 [unnamed-chunk-4]
    9/13                  
    10/13 [unnamed-chunk-5]
    11/13                  
    12/13 [unnamed-chunk-6]
    13/13                  
    output file: quarto_parameterized.knit.md

    pandoc --output 2022-darkblue-report.md
      to: >-
        commonmark+autolink_bare_uris+emoji+footnotes+gfm_auto_identifiers+pipe_tables+strikeout+task_lists+tex_math_dollars
      from: markdown+gfm_auto_identifiers
      standalone: true
      default-image-extension: png
      toc: true
      
    metadata
      title: template code
      
    Output created: 2022-darkblue-report.md



    processing file: quarto_parameterized.qmd
    1/13                  
    2/13 [unnamed-chunk-1]
    3/13                  
    4/13 [unnamed-chunk-2]
    5/13                  
    6/13 [unnamed-chunk-3]
    7/13                  
    8/13 [unnamed-chunk-4]
    9/13                  
    10/13 [unnamed-chunk-5]
    11/13                  
    12/13 [unnamed-chunk-6]
    13/13                  
    output file: quarto_parameterized.knit.md

    pandoc --output 2022-darkgreen-report.md
      to: >-
        commonmark+autolink_bare_uris+emoji+footnotes+gfm_auto_identifiers+pipe_tables+strikeout+task_lists+tex_math_dollars
      from: markdown+gfm_auto_identifiers
      standalone: true
      default-image-extension: png
      toc: true
      
    metadata
      title: template code
      
    Output created: 2022-darkgreen-report.md



    processing file: quarto_parameterized.qmd
    1/13                  
    2/13 [unnamed-chunk-1]
    3/13                  
    4/13 [unnamed-chunk-2]
    5/13                  
    6/13 [unnamed-chunk-3]
    7/13                  
    8/13 [unnamed-chunk-4]
    9/13                  
    10/13 [unnamed-chunk-5]
    11/13                  
    12/13 [unnamed-chunk-6]
    13/13                  
    output file: quarto_parameterized.knit.md

    pandoc --output 2008-darkblue-report.md
      to: >-
        commonmark+autolink_bare_uris+emoji+footnotes+gfm_auto_identifiers+pipe_tables+strikeout+task_lists+tex_math_dollars
      from: markdown+gfm_auto_identifiers
      standalone: true
      default-image-extension: png
      toc: true
      
    metadata
      title: template code
      
    Output created: 2008-darkblue-report.md



    processing file: quarto_parameterized.qmd
    1/13                  
    2/13 [unnamed-chunk-1]
    3/13                  
    4/13 [unnamed-chunk-2]
    5/13                  
    6/13 [unnamed-chunk-3]
    7/13                  
    8/13 [unnamed-chunk-4]
    9/13                  
    10/13 [unnamed-chunk-5]
    11/13                  
    12/13 [unnamed-chunk-6]
    13/13                  
    output file: quarto_parameterized.knit.md

    pandoc --output 2008-darkgreen-report.md
      to: >-
        commonmark+autolink_bare_uris+emoji+footnotes+gfm_auto_identifiers+pipe_tables+strikeout+task_lists+tex_math_dollars
      from: markdown+gfm_auto_identifiers
      standalone: true
      default-image-extension: png
      toc: true
      
    metadata
      title: template code
      
    Output created: 2008-darkgreen-report.md
