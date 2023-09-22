library(tidyverse)
library(ggdist)
install.packages("tidyquant")
library(httpgd); hgd(); hgd_browse()
mpg %>%
    filter(cyl %in% c(4, 6, 8)) %>%
ggplot2 :: ggplot(aes(x = factor(cyl), y = hwy, fill = factor(cyl))) + 
ggdist :: stat_halfeye(# use for half-density
    ## custom bandwidth
    adjust = 0.5, 
    ## move geom to the right
    justification = -.2,
    ## remove slab interval
    .width = 0, 
    point_colour = NA
) + 
ggplot2 :: geom_boxplot(
    width = 0.12,
    ## remove outliers
    outlier.color = NA, 
    alpha = 0.5
) + 
## add dot plots from {ggdist} package
ggdist :: stat_dots(
    ## orientation to the left
    side = "left", 
    ## move to the left
    justification = 1.1, 
    ## adjust grouping {binning} of observations
    binwidth = 0.25
) + 
## adjust theme
tidyquant :: scale_fill_tq() + 
tidyquant :: theme_tq() + 
ggplot2 :: labs(title = "Raincloud Plot", 
subtitle = "showing the Bi-Modal Distribution of 6 Cylinder Vehicles", 
x = "Engine Size (No. of Cylinders)", 
y = "Highway Fuel Economy (MPG)", 
fill = "Cylinders") + 
coord_flip()


