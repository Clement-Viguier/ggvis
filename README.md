ggvistools
----------

This package is an extension of ggplot2 to enable faster design
exploration for data visualisation.

For now it contains the custom geom\_dash that allows to plot dashes
instead of points. Those can be rotated (aes\_angle in radiant) or
widden (aes\_width) to encode additional information in comparison of
geom\_points.

It also allows distribution visualisation like geom\_rug.

    iris %>% ggplot(aes(Species , Petal.Length)) +
      geom_dash(width = 0.8) +
      coord_fixed()

![](README_files/figure-markdown_strict/unnamed-chunk-1-1.png)

An example of the use of `geom_dash` to plot gradient is as follow:

    library(data.table)
    library(metR)  # devtools::install_github("eliocamp/metR")

    volcano <- as.data.table(melt(volcano, varnames = c("x", "y"),
                                  value.name = "h"))
    volcano[, c("dx", "dy") := metR::Derivate(h ~ x + y)]
    volcano[, "angle" := atan(dy/dx)]
    volcano[, "slope" := sqrt(dy^2 + dx^2)]

    volcano %>% as.data.frame() %>%  ggplot(aes(x, y)) +
      geom_dash(aes(width = slope/max(slope, na.rm = T), angle = angle), relative = T) +
      coord_fixed()+ theme_void()

    ## Warning: Removed 586 rows containing missing values (geom_dash).

![](README_files/figure-markdown_strict/unnamed-chunk-2-1.png) ![Volcano
slope field](./images/volcano_dash.png)

The geom\_arc function allows rapic arc visualisation

    iris %>% ggplot(aes(Sepal.Length , Petal.Length, colour = Species)) +
      geom_arc(width = 0.8, aes(angle = Petal.Width ,radius = 0.1*Sepal.Width)) +
      coord_fixed()

![](README_files/figure-markdown_strict/unnamed-chunk-3-1.png)

Because these geoms do not use symbols, but explicit representation of
geometric objects, they work better with fixed coordinates. This can be
ensured by addig `+ coord_fixed()` to your ggplot call.

Further work is need to ensure all objects fit the frame of the plot
that is currently computed from the x,y of the aesthetic. For now this
problem is fixed by increasin the visible area with the function
`coord_cartesian()`.
