# survival function

``` r
library(survival)
library(survminer)
```

    Loading required package: ggplot2

    Loading required package: ggpubr


    Attaching package: 'survminer'

    The following object is masked from 'package:survival':

        myeloma

``` r
library(gridtext)
rm(list=ls())#clean all variables
library(foreign)
df <- haven :: read_spss("data_transplant.sav")
names(df) =c("ID","group","time","event","age","sex","CMV","FAB","center")

summary(df)
```

           ID          group            time            event       
     Min.   :  1   Min.   :1.000   Min.   :   1.0   Min.   :0.0000  
     1st Qu.: 35   1st Qu.:1.000   1st Qu.: 183.0   1st Qu.:0.0000  
     Median : 69   Median :2.000   Median : 547.0   Median :1.0000  
     Mean   : 69   Mean   :2.051   Mean   : 839.2   Mean   :0.5839  
     3rd Qu.:103   3rd Qu.:3.000   3rd Qu.:1377.0   3rd Qu.:1.0000  
     Max.   :137   Max.   :3.000   Max.   :2640.0   Max.   :1.0000  
          age             sex              CMV              FAB        
     Min.   : 7.00   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
     1st Qu.:21.00   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
     Median :28.00   Median :1.0000   Median :0.0000   Median :0.0000  
     Mean   :28.36   Mean   :0.5839   Mean   :0.4964   Mean   :0.3285  
     3rd Qu.:35.00   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:1.0000  
     Max.   :52.00   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
         center     
     Min.   :1.000  
     1st Qu.:2.000  
     Median :3.000  
     Mean   :2.905  
     3rd Qu.:4.000  
     Max.   :5.000  

``` r
head(df)
```

    # A tibble: 6 × 9
         ID group  time event       age sex        CMV            FAB center
      <dbl> <dbl> <dbl> <dbl+lbl> <dbl> <dbl+lbl>  <dbl+lbl>    <dbl>  <dbl>
    1     1     1  2081 0 [alive]    26 1 [male]   1 [positive]     0      1
    2    10     1  1199 0 [alive]    24 1 [male]   0 [negative]     0      1
    3    20     1   172 1 [dead]     40 0 [female] 0 [negative]     0      1
    4    27     1  1279 1 [dead]     17 0 [female] 0 [negative]     0      1
    5    35     1     1 1 [dead]     42 1 [male]   0 [negative]     0      1
    6    36     1   107 1 [dead]     30 1 [male]   1 [positive]     0      1

``` r
s1 <- survdiff(Surv(time, event) ~ group, data = df)
s1
```

    Call:
    survdiff(formula = Surv(time, event) ~ group, data = df)

             N Observed Expected (O-E)^2/E (O-E)^2/V
    group=1 38       23     21.1     0.179     0.246
    group=2 54       23     38.3     6.084    11.910
    group=3 45       34     20.7     8.570    11.687

     Chisq= 15.1  on 2 degrees of freedom, p= 5e-04 

``` r
library(survival)
library(survminer)
library(gridtext)
rm(list=ls())#clean all variables
library(haven)
df <- read_spss("data_transplant.sav")
names(df) <- c("ID","group","time","event","age","sex","CMV","FAB","center")



df |> with(table(group))
```

    group
     1  2  3 
    38 54 45 

``` r
df |> with(table(event))
```

    event
     0  1 
    57 80 

``` r
df |> with(summary(time))
```

       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
        1.0   183.0   547.0   839.2  1377.0  2640.0 

``` r
df |> with(table(sex))
```

    sex
     0  1 
    57 80 

``` r
#--------non parametric analysis
skm <- survfit(Surv(time,event)~1,data=df)
summary(skm, time = c(365, 730))
```

    Call: survfit(formula = Surv(time, event) ~ 1, data = df)

     time n.risk n.event survival std.err lower 95% CI upper 95% CI
      365     86      49    0.641   0.041        0.566        0.727
      730     62      23    0.469   0.043        0.392        0.561

\`\`\`
