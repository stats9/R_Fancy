# Automate Parameterized


- [create a dataframe for params](#create-a-dataframe-for-params)

# create a dataframe for params

``` r
dat <- expand.grid(color = c("darkblue", "darkgreen"), 
    year = c(2022, 2008), stringsAsFactors = FALSE)
# dat 

dat2 <- dat |> 
        dplyr :: mutate(output_format = "html", 
            output_file = paste(year, color, "report.html", sep = "-"),
            execute_params = purrr :: map2(color, year, 
            \(color, year) list(color = color, year = year))) |> 
            dplyr :: select(-c(color, year))

dat2
```

      output_format                output_file  execute_params
    1          html  2022-darkblue-report.html  darkblue, 2022
    2          html 2022-darkgreen-report.html darkgreen, 2022
    3          html  2008-darkblue-report.html  darkblue, 2008
    4          html 2008-darkgreen-report.html darkgreen, 2008

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

      |                                                          
      |                                                    |   0%
      |                                                          
      |....                                                |   8%                  
      |                                                          
      |........                                            |  15% [unnamed-chunk-1]
      |                                                          
      |............                                        |  23%                  
      |                                                          
      |................                                    |  31% [unnamed-chunk-2]
      |                                                          
      |....................                                |  38%                  
      |                                                          
      |........................                            |  46% [unnamed-chunk-3]
      |                                                          
      |............................                        |  54%                  
      |                                                          
      |................................                    |  62% [unnamed-chunk-4]
      |                                                          
      |....................................                |  69%                  
      |                                                          
      |........................................            |  77% [unnamed-chunk-5]
      |                                                          
      |............................................        |  85%                  
      |                                                          
      |................................................    |  92% [unnamed-chunk-6]
      |                                                          
      |....................................................| 100%                  
                                                                                                                
    output file: quarto_parameterized.knit.md

    pandoc --output 2022-darkblue-report.html
      to: html
      standalone: true
      section-divs: true
      html-math-method: mathjax
      wrap: none
      default-image-extension: png
      toc: true
      
    metadata
      document-css: false
      link-citations: true
      date-format: long
      lang: en
      title: template code
      
    Output created: 2022-darkblue-report.html



    processing file: quarto_parameterized.qmd

      |                                                          
      |                                                    |   0%
      |                                                          
      |....                                                |   8%                  
      |                                                          
      |........                                            |  15% [unnamed-chunk-1]
      |                                                          
      |............                                        |  23%                  
      |                                                          
      |................                                    |  31% [unnamed-chunk-2]
      |                                                          
      |....................                                |  38%                  
      |                                                          
      |........................                            |  46% [unnamed-chunk-3]
      |                                                          
      |............................                        |  54%                  
      |                                                          
      |................................                    |  62% [unnamed-chunk-4]
      |                                                          
      |....................................                |  69%                  
      |                                                          
      |........................................            |  77% [unnamed-chunk-5]
      |                                                          
      |............................................        |  85%                  
      |                                                          
      |................................................    |  92% [unnamed-chunk-6]
      |                                                          
      |....................................................| 100%                  
                                                                                                                
    output file: quarto_parameterized.knit.md

    pandoc --output 2022-darkgreen-report.html
      to: html
      standalone: true
      section-divs: true
      html-math-method: mathjax
      wrap: none
      default-image-extension: png
      toc: true
      
    metadata
      document-css: false
      link-citations: true
      date-format: long
      lang: en
      title: template code
      
    Output created: 2022-darkgreen-report.html



    processing file: quarto_parameterized.qmd

      |                                                          
      |                                                    |   0%
      |                                                          
      |....                                                |   8%                  
      |                                                          
      |........                                            |  15% [unnamed-chunk-1]
      |                                                          
      |............                                        |  23%                  
      |                                                          
      |................                                    |  31% [unnamed-chunk-2]
      |                                                          
      |....................                                |  38%                  
      |                                                          
      |........................                            |  46% [unnamed-chunk-3]
      |                                                          
      |............................                        |  54%                  
      |                                                          
      |................................                    |  62% [unnamed-chunk-4]
      |                                                          
      |....................................                |  69%                  
      |                                                          
      |........................................            |  77% [unnamed-chunk-5]
      |                                                          
      |............................................        |  85%                  
      |                                                          
      |................................................    |  92% [unnamed-chunk-6]
      |                                                          
      |....................................................| 100%                  
                                                                                                                
    output file: quarto_parameterized.knit.md

    pandoc --output 2008-darkblue-report.html
      to: html
      standalone: true
      section-divs: true
      html-math-method: mathjax
      wrap: none
      default-image-extension: png
      toc: true
      
    metadata
      document-css: false
      link-citations: true
      date-format: long
      lang: en
      title: template code
      
    Output created: 2008-darkblue-report.html



    processing file: quarto_parameterized.qmd

      |                                                          
      |                                                    |   0%
      |                                                          
      |....                                                |   8%                  
      |                                                          
      |........                                            |  15% [unnamed-chunk-1]
      |                                                          
      |............                                        |  23%                  
      |                                                          
      |................                                    |  31% [unnamed-chunk-2]
      |                                                          
      |....................                                |  38%                  
      |                                                          
      |........................                            |  46% [unnamed-chunk-3]
      |                                                          
      |............................                        |  54%                  
      |                                                          
      |................................                    |  62% [unnamed-chunk-4]
      |                                                          
      |....................................                |  69%                  
      |                                                          
      |........................................            |  77% [unnamed-chunk-5]
      |                                                          
      |............................................        |  85%                  
      |                                                          
      |................................................    |  92% [unnamed-chunk-6]
      |                                                          
      |....................................................| 100%                  
                                                                                                                
    output file: quarto_parameterized.knit.md

    pandoc --output 2008-darkgreen-report.html
      to: html
      standalone: true
      section-divs: true
      html-math-method: mathjax
      wrap: none
      default-image-extension: png
      toc: true
      
    metadata
      document-css: false
      link-citations: true
      date-format: long
      lang: en
      title: template code
      
    Output created: 2008-darkgreen-report.html
