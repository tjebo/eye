<!-- README.md is generated from README.Rmd. Please edit that file -->

# eye

See your data with a new eye.

# Features

## Only about the eye

  - [Easy count of patients and eyes](#count-patients-and-eyes)
  - [Conversion of visual acuity notations](#va-conversion)

## Beyond the eye

### Stats functions

  - [Show common summary statistics](#basic-statistics)
  - [Calculate 2d probability contours for the use in
    ggplot2](#probability-contours)

### Convenience functions:

  - [Easy save of your data frame as csv](#save-to-csv)
  - [Calculate age](#calculate-age)

### ggplot2 extensions

  - [geom\_trail: A base plot type = “b” equivalent for
    ggplot2](#geom_trail)

## AMD data

Real life anonymised human subject data from a large cohort of patients
with treatment-naive neovascular age-related macular degeneration (AMD)
that received intravitreal anti-VEGF therapy. The data is from
Moorfields Eye Hospital, London, UK and was made available [on a public
repository](https://datadryad.org/stash/dataset/doi:10.5061/dryad.97r9289).

To reference this data in your publication, please kindly cite the
corresponding article by Fasler and colleagues.(Fasler et al.
[2019](#ref-fasler))

# Get eye

``` r
# for the development version 
devtools::install_github("tjebo/eye")
```

# Examples

## Eye stuff

### Count patients and eyes

``` r
eyes(amd)
#> Warning: Eyes are coded 0/1 or 1/2. Interpreting r = 0 or 1, respectively
#> patients     eyes  right.0   left.1 
#>     3357     3357     1681     1676
```

### VA conversion

## Beyond eye stuff

### Common statistics

``` r
x = y = z = c(rnorm(20), NA)
# named or unnamed list
mylist <- list(x = x, y = y, z = z)
# with a data frame
mydf <- data.frame(x, y, z)

show_stats(mylist)
#>   mean  sd  n median  min max
#> x -0.2 1.1 21    0.1 -2.2 1.4
#> y -0.2 1.1 21    0.1 -2.2 1.4
#> z -0.2 1.1 21    0.1 -2.2 1.4
show_stats(mydf)
#>   mean  sd  n median  min max
#> x -0.2 1.1 21    0.1 -2.2 1.4
#> y -0.2 1.1 21    0.1 -2.2 1.4
#> z -0.2 1.1 21    0.1 -2.2 1.4

# For an aggregation by group, split the data frame first
mydf2 <- data.frame(group = rep(letters[1:2], each = 42), x, y, z)
lapply(split(mydf2, mydf2$group), show_stats, rownames = FALSE)
#> $a
#>   var mean  sd  n median  min max
#> 1   x -0.2 1.1 42    0.1 -2.2 1.4
#> 2   y -0.2 1.1 42    0.1 -2.2 1.4
#> 3   z -0.2 1.1 42    0.1 -2.2 1.4
#> 
#> $b
#>   var mean  sd  n median  min max
#> 1   x -0.2 1.1 42    0.1 -2.2 1.4
#> 2   y -0.2 1.1 42    0.1 -2.2 1.4
#> 3   z -0.2 1.1 42    0.1 -2.2 1.4
```

### Probability contours

``` r
library(ggplot2)

set.seed(1)
n=100
foo <- data.frame(x=rnorm(n, 0, 1), y=rnorm(n, 0, 1))

df_contours <- dplyr::bind_rows(
  purrr::map(seq(0.2, 0.8, 0.2), function(p) prob_contour(foo, prob = p))
)

ggplot() +
  geom_point(data = foo, aes(x = x, y = y)) +
  geom_polygon(data = df_contours, aes(x = x, y = y, color = prob), fill = NA) +
  scale_color_brewer(name = "Probs", palette = "Set1")
```

![](README-prob-1.png)<!-- -->

### Calculate age

Age calculated in years. Periods or duration possible. If only one
argument, the age today.

``` r
age("1984-10-16")
#> [1] 35.6

dob <-  c("1984-10-16", "2000-01-01")
test_date <-  as.Date(dob) + c(15000, 20000)

age(dob, test_date)
#> [1] 41.1 54.8
```

### Save to csv

This is a convenience wrapper around write.csv

``` r
csv(amd)
```

## ggplot2 extensions

### geom\_trail

A base plot type = “b” equivalent for ggplot. Works also with text\!

``` r
library(ggplot2)

ggplot(pressure, aes(temperature, pressure)) +
  geom_ribbon(aes(ymin = pressure - 50, ymax = pressure + 50), alpha = 0.2) +
  geom_trail()
```

![](README-trail-1.png)<!-- -->

# References

<div id="refs" class="references">

<div id="ref-fasler">

Fasler, Katrin, Gabriella Moraes, Siegfried Wagner, Karsten U Kortuem,
Reena Chopra, Livia Faes, Gabriella Preston, et al. 2019. “One- and
Two-Year Visual Outcomes from the Moorfields Age-Related Macular
Degeneration Database: A Retrospective Cohort Study and an Open Science
Resource.” *BMJ Open* 9 (6). British Medical Journal Publishing Group.
<https://doi.org/10.1136/bmjopen-2018-027441>.

</div>

</div>
