eye
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

See more with eye.

eye is a package dedicated to facilitate ophthalmic research. Its two
core functions [`eyes()`](#eyes) and [`va()`](#va) help with very common
tasks (counting patients and eyes, visual acuity notation conversions).
It contains a well curated [real life data set](#amd-data) and some
functions beyond ophtalmology, which could make your data analysis a
tiny bit more convenient. Finally, there are also a few ggplot2
extensions to make some nice graphs.

# Features

## Pure ophthalmology

  - [Easy count of patients and eyes](#eyes)
  - [Conversion of visual acuity notations](#va)
  - [Make your eye data long](#myop)

### AMD data

  - Anonymised [real life data from a large
    cohort](https://datadryad.org/stash/dataset/doi:10.5061/dryad.97r9289)
    of patients with treatment-naive neovascular age-related macular
    degeneration (AMD) who received intravitreal anti-VEGF therapy in
    Moorfields Eye Hospital, London, UK.
  - To reference this data in your publication, please kindly cite the
    corresponding article by Fasler and colleagues.(Fasler et al.
    [2019](#ref-fasler))

## Beyond the eye

### Convenience functions:

  - [See common summary statistics](#see)
  - [Calculate age](#age)
  - [Conveniently save a data frame to csv](#csv)

### Catch eyes - ggplot2 extensions

  - [geom\_trail: A base plot type = “b” equivalent for
    ggplot2](#geom_trail)

# Install eye

Currently only on github.

``` r
# for the development version 
devtools::install_github("tjebo/eye")
```

# Examples

## Pure eye stuff - core eye functions

### eyes

Count patient and eyes.
[source](https://github.com/tjebo/eye/blob/master/R/eyes.R)

``` r
eyes(amd)
#> Eyes coded 0:1. Interpreting as r = 0
#> patients     eyes    right     left 
#>     3357     3357     1681     1676
```

### va

Visual acuity notation conversion

``` r
# TBC
```

### myop

Make your data long (“myopic”). Convenience wrapper around
`tidyr::pivot_longer`

``` r
set.seed(42)
iop <- data.frame(id = letters[1:3], r = sample(11:13), l = sample(14:16))

myop(iop, values_to = "iop")
#> # A tibble: 6 x 3
#>   id    eye     iop
#>   <chr> <chr> <int>
#> 1 a     r        11
#> 2 a     l        14
#> 3 b     r        13
#> 4 b     l        15
#> 5 c     r        12
#> 6 c     l        16
```

Often enough, there are right eye / left eye columns for more than one
variable, e.g., for both IOP and VA. The following example shows one way
to clean up this mess.

``` r
library(dplyr)
iop_va <- data.frame(id = letters[1:3], iop_r = sample(11:13), iop_l = sample(11:13), va_r = sample(41:43), va_l = sample(41:43))
iop_va
#>   id iop_r iop_l va_r va_l
#> 1  a    12    13   41   42
#> 2  b    13    12   43   43
#> 3  c    11    11   42   41

# use myope twice on both iop and va columns and join the results
iop_long <- myop(iop_va, cols = c("iop_r", "iop_l"), values_to = "iop") 
va_long  <- myop(iop_va, cols = c("va_r", "va_l"), values_to = "va") 

full_join(iop_long, va_long, by = c("id", "eye")) %>%
  select(id, eye, va, iop)
#> # A tibble: 6 x 4
#>   id    eye      va   iop
#>   <chr> <chr> <int> <int>
#> 1 a     r        41    12
#> 2 a     l        42    13
#> 3 b     r        43    13
#> 4 b     l        43    12
#> 5 c     r        42    11
#> 6 c     l        41    11
```

## Beyond the eye

### see

Show common statistics

``` r
amd_unq <- amd[!duplicated(amd$Id),]

see(amd_unq[c("BaselineAge", "VA_ETDRS_Letters", "FollowupDays")])
#>                  mean   sd    n median min max
#> BaselineAge      78.3  9.1 3357     79  60  99
#> VA_ETDRS_Letters 56.3 14.7 3357     58   0  92
#> FollowupDays      0.1  3.1 3357      0   0 168
```

### age

  - Calculate age in years, as [periods or
    durations](https://lubridate.tidyverse.org/articles/lubridate.html#time-intervals)
  - If only the start date given, calculating the age today.

<!-- end list -->

``` r
age("1984-10-16")
#> [1] 35.6

dob <-  c("1984-10-16", "2000-01-01")
test_date <-  as.Date(dob) + c(15000, 20000)

age(dob, test_date)
#> [1] 41.1 54.8
```

### csv

  - A convenience wrapper around `write.csv`. Saves a .csv file with the
    name of the data frame, or with a different name.

<!-- end list -->

``` r
csv(amd)
```

## ggplot2 extensions

### geom\_trail

A base plot type = “b” equivalent for ggplot. Works also with text\!

``` r
library(ggplot2)
library(dplyr)
# data preparation
amd_aggr <-
  amd %>%
  group_by(
    age_cut10 = cut_width(BaselineAge, 10),
    days_cut90 = cut_width(FollowupDays, 90, labels = seq(0, 810, 90))
  ) %>%
  summarise(mean_va = mean(VA_ETDRS_Letters)) 

# plot
p <-
  ggplot(amd_aggr, aes(days_cut90, mean_va, color = age_cut10)) +
    theme_classic() +
    labs(x = "Follow up time [Days]", y = "Mean VA [ETDRS letters]", color = "Age strata")

p1 <- p + geom_trail(aes(group = age_cut10))

p2 <- p + geom_trail(aes(group = age_cut10), size = 0) +
          geom_text(aes(label = round(mean_va, 0)), show.legend = FALSE)
```

``` r
p1 

p2
```

<img src="README-unnamed-chunk-4-1.png" width="45%" /><img src="README-unnamed-chunk-4-2.png" width="45%" />

# Acknowledgements

  - Thanks to Siegfried Wagner and Abraham Olvera, for their help with
    VA conversion
  - Thanks to Tim Yap for helping to find typos. All remaining typos are
    entirely his responsability.
  - Thanks to Hadley Wickham and his development tools, (in particular
    roxygen2, usethis, testthis, and devtools) without I would have
    never been able to make this package.

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
