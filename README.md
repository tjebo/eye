eye
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

See more with *eye*.

*eye* is dedicated to facilitate ophthalmic research.</br> Its core
functions [`blink()`](#blink), [`eyes()`](#eyes), [`myop()`](#myop), and
[`va()`](#va) help with very common tasks:

  - Counting patients and eyes
  - Visual acuity and intraocular pressure: Shape data and analyze\!
  - Conversion of visual acuity notations.

*eye* contains a well curated [real life data set](#amd-data) and some
functions beyond ophtalmology, which could make your data analysis a
tiny bit more convenient.

Finally, eye comes with [`geom_trail()`](#geom_trail) for some nice
trail graphs.

## Features

### Pure ophthalmology

  - [Perceive your data in a blink of an eye](#blink)
  - [Easy count of patients and eyes](#eyes)
  - [Conversion of visual acuity notations](#va)
  - Conversion chart and conversion functions
  - [Make your eye data long](#myop)

#### AMD data

  - Anonymised [real life data from a large
    cohort](https://datadryad.org/stash/dataset/doi:10.5061/dryad.97r9289)
    of patients with treatment-naive neovascular age-related macular
    degeneration (AMD) who received intravitreal anti-VEGF therapy in
    Moorfields Eye Hospital, London, UK.
  - To reference this data in your publication, please kindly cite the
    corresponding publication.(Fasler et al. [2019](#ref-fasler))

### Beyond the eye

#### Convenience functions:

  - [Insight: get common summary statistics](#insight)
  - [Calculate age](#age)
  - [Conveniently save a data frame to csv](#csv)

#### Catch eyes - ggplot2 extensions

  - [geom\_trail: A base plot type = “b” equivalent for
    ggplot2](#geom_trail)

## Install eye

Currently only on github.

``` r
## for the development version 
devtools::install_github("tjebo/eye")
```

## Examples

Required packages

``` r
library(tidyverse)
```

### Pure eye stuff - core eye functions

#### blink

#### eyes

Count patient and eyes

``` r
eyes(amd)
#> Eyes coded 0:1. Interpreting as r = 0
#> patients     eyes    right     left 
#>     3357     3357     1681     1676
```

#### va

  - *eye* includes a [visual acuity conversion chart:
    `va_chart`](#va-conversion-chart)
  - This chart and VA conversion formulas are based on (Holladay
    [2004](#ref-holladay)), (Beck et al. [2003](#ref-beck)) and
    (Gregori, Feuer, and Rosenfeld [2010](#ref-gregori))
  - Categories **counting fingers** and **hand movements** are converted
    following (Schulze-Bonsel et al. [2006](#ref-bach))
  - Categories **no light perception** and **light perception** are
    converted following the suggestions by Michael Bach

<!-- end list -->

``` r
## will automatically detect VA class and convert to logMAR by default
## ETDRS letters
va(c(23, 56, 74, 58)) 
#> [1] 1.24 0.58 0.22 0.54
#> attr(,"class")
#> [1] "logmar"  "va"      "numeric"

## ... or convert to snellen
va(c(23, 56, 74, 58), to = "snellen") 
#> [1] "20/320" "20/80"  "20/32"  "20/63" 
#> attr(,"class")
#> [1] "snellen"   "va"        "character"

## snellen, mixed with categories
va(c("NLP", "LP", "HM", "CF", "6/60", "20/200", "6/9", "20/40"))
#> [1] 3.00 2.70 2.30 1.90 1.00 1.00 0.18 0.30
#> attr(,"class")
#> [1] "logmar"  "va"      "numeric"

## on the inbuilt data set:
amd$logmar <- va(amd$VA_ETDRS_Letters)
#> Warning: Guess ETDRS? Values out of range. Check your data.

## (indeed, there are unplausible ETDRS values in this data set!!)
## let's see where they are

amd %>% 
  select(-(1:5)) %>% 
  arrange(VA_ETDRS_Letters) %>%
  slice(c(head(row_number(),3), tail(row_number(), 3)))
#> # A tibble: 6 x 3
#>   VA_ETDRS_Letters InjectionNumber logmar
#>              <dbl>           <dbl>  <dbl>
#> 1                0              20   1.7 
#> 2                0               9   1.7 
#> 3                0               3   1.7 
#> 4              100              13  -0.3 
#> 5              102               7  -0.34
#> 6              105              15  -0.4

## Unplausible values! Need to contact the data curator:)
## The logMAR conversion worked nevertheless
```

#### myop

Make your data long (“myopic”)

Simple data frame with one column for right eye and left eye.

``` r
iop_wide
#>   id  r  l
#> 1  a 11 14
#> 2  b 13 15
#> 3  c 12 16

myop(iop_wide)
#> Picked "r" and "l" for right and left eyes
#> Neither VA nor IOP column(s) found. Gathering eye columns
#> # A tibble: 6 x 3
#>   id    name  value
#>   <chr> <chr> <int>
#> 1 a     r        11
#> 2 a     l        14
#> 3 b     r        13
#> 4 b     l        15
#> 5 c     r        12
#> 6 c     l        16
```

Often enough, there are right eye / left eye columns for more than one
variable, e.g., for both IOP and VA. `myop` helps you clean this mess
and will detect IOP and VA columns automatically.

``` r
messy_df
#>   id iop_r iop_l va_r va_l
#> 1  a    12    13   41   42
#> 2  b    13    12   43   43
#> 3  c    11    11   42   41

clean_df <- myop(messy_df)
#> Picked "iop_r,va_r" and "iop_l,va_l" for right and left eyes
#> Gathering both VA and IOP columns

clean_df
#> # A tibble: 6 x 4
#>   id    eye     IOP    VA
#>   <chr> <chr> <int> <int>
#> 1 a     r        12    41
#> 2 a     l        13    42
#> 3 b     r        13    43
#> 4 b     l        12    43
#> 5 c     r        11    42
#> 6 c     l        11    41
```

### Beyond the eye

#### insight

Show common statistics

``` r
amd_unq <- amd[!duplicated(amd$Id),]

insight(amd_unq[c("BaselineAge", "VA_ETDRS_Letters", "FollowupDays")])
#>                  mean   sd    n median min max
#> BaselineAge      78.3  9.1 3357     79  60  99
#> VA_ETDRS_Letters 56.3 14.7 3357     58   0  92
#> FollowupDays      0.1  3.1 3357      0   0 168
```

#### age

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

#### csv

  - A convenience wrapper around `write.csv`. Saves a .csv file with the
    name of the data frame, or with a different name.

<!-- end list -->

``` r
csv(amd)
```

### ggplot2 extensions

#### geom\_trail

A base plot type = “b” equivalent for ggplot. Works also with text\!

<details>

<summary>Prepare AMD data for plot (click to unfold) </summary>

``` r
amd_aggr <-
  amd %>%
  group_by(
    age_cut10 = cut_width(BaselineAge, 10),
    days_cut90 = cut_width(FollowupDays, 90, labels = seq(0, 810, 90))
  ) %>%
  summarise(mean_va = mean(VA_ETDRS_Letters)) 
```

</details>

``` r
p <-
  ggplot(amd_aggr, aes(days_cut90, mean_va, color = age_cut10)) +
    theme_classic() +
    labs(x = "Follow up time [Days]", y = "Mean VA [ETDRS letters]", 
         color = "Age strata")
```

    p + geom_trail(aes(group = age_cut10))
    
    p + geom_trail(aes(group = age_cut10), size = 0) +
              geom_text(aes(label = round(mean_va, 0)), show.legend = FALSE)

<img src="README-unnamed-chunk-5-1.png" width="45%" /><img src="README-unnamed-chunk-5-2.png" width="45%" />

## Acknowledgements

  - Thanks to Siegfried Wagner and Abraham Olvera, for their help with
    VA conversion
  - Thanks to Tim Yap for helping find typos. All remaining typos are
    entirely his fault.
  - Thanks to Hadley Wickham\! Many of the functions in my package rely
    on the `tidyverse` package, but I would never have been able to make
    this package without his development tools `roxygen2`, `usethis`,
    `testthis` and `devtools`.

## VA conversion chart

<div style="font-size:8 pt;">

| snellen\_ft | snellen\_m | snellen\_dec | logMAR | ETDRS | quali |
| ----------- | ---------- | ------------ | ------ | ----- | ----- |
| 20/20000    | 6/6000     | 0.001        | 3      | 0     | NLP   |
| 20/10000    | 6/3000     | 0.002        | 2.7    | 0     | LP    |
| 20/4000     | 6/1200     | 0.005        | 2.3    | 0     | HM    |
| 20/2000     | 6/600      | 0.01         | 1.9    | 2     | CF    |
| 20/800      | 6/240      | 0.025        | 1.6    | 5     | NA    |
| 20/630      | 6/190      | 0.032        | 1.5    | 10    | NA    |
| 20/500      | 6/150      | 0.04         | 1.4    | 15    | NA    |
| 20/400      | 6/120      | 0.05         | 1.3    | 20    | NA    |
| 20/320      | 6/96       | 0.062        | 1.2    | 25    | NA    |
| 20/300      | 6/90       | 0.067        | 1.18   | 26    | NA    |
| 20/250      | 6/75       | 0.08         | 1.1    | 30    | NA    |
| 20/200      | 6/60       | 0.1          | 1.0    | 35    | NA    |
| 20/160      | 6/48       | 0.125        | 0.9    | 40    | NA    |
| 20/125      | 6/38       | 0.16         | 0.8    | 45    | NA    |
| 20/100      | 6/30       | 0.2          | 0.7    | 50    | NA    |
| 20/80       | 6/24       | 0.25         | 0.6    | 55    | NA    |
| 20/70       | 6/21       | 0.29         | 0.54   | 58    | NA    |
| 20/63       | 6/19       | 0.32         | 0.5    | 60    | NA    |
| 20/60       | 6/18       | 0.33         | 0.48   | 61    | NA    |
| 20/50       | 6/15       | 0.4          | 0.4    | 65    | NA    |
| 20/40       | 6/12       | 0.5          | 0.3    | 70    | NA    |
| 20/32       | 6/9.6      | 0.625        | 0.2    | 75    | NA    |
| 20/30       | 6/9        | 0.66         | 0.18   | 76    | NA    |
| 20/25       | 6/7.5      | 0.8          | 0.1    | 80    | NA    |
| 20/20       | 6/6        | 1.0          | 0.0    | 85    | NA    |
| 20/16       | 6/5        | 1.25         | \-0.1  | 90    | NA    |
| 20/15       | 6/4.5      | 1.33         | \-0.12 | 91    | NA    |
| 20/13       | 6/4        | 1.5          | \-0.2  | 95    | NA    |
| 20/10       | 6/3        | 2.0          | \-0.3  | 100   | NA    |

</div>

## Resources

[Michael Bach’s homepage](https://michaelbach.de/sci/acuity.html)
[Michael Bach on NLP and
LP](https://michaelbach.de/sci/pubs/Bach2007IOVS%20eLetter%20FrACT.pdf)

## References

<div id="refs" class="references">

<div id="ref-beck">

Beck, Roy W, Pamela S Moke, Andrew H Turpin, Frederick L Ferris, John
Paul SanGiovanni, Chris A Johnson, Eileen E Birch, et al. 2003. “A
Computerized Method of Visual Acuity Testing.” *American Journal of
Ophthalmology* 135 (2). Elsevier BV: 194–205.
<https://doi.org/10.1016/s0002-9394(02)01825-1>.

</div>

<div id="ref-fasler">

Fasler, Katrin, Gabriella Moraes, Siegfried Wagner, Karsten U Kortuem,
Reena Chopra, Livia Faes, Gabriella Preston, et al. 2019. “One- and
Two-Year Visual Outcomes from the Moorfields Age-Related Macular
Degeneration Database: A Retrospective Cohort Study and an Open Science
Resource.” *BMJ Open* 9 (6). British Medical Journal Publishing Group.
<https://doi.org/10.1136/bmjopen-2018-027441>.

</div>

<div id="ref-gregori">

Gregori, Ninel Z, William Feuer, and Philip J Rosenfeld. 2010. “Novel
Method for Analyzing Snellen Visual Acuity Measurements.” *Retina* 30
(7). Ovid Technologies (Wolters Kluwer Health): 1046–50.
<https://doi.org/10.1097/iae.0b013e3181d87e04>.

</div>

<div id="ref-holladay">

Holladay, Jack T. 2004. “Visual Acuity Measurements.” *Journal of
Cataract and Refractive Surgery* 30 (2): 287–90.
<https://doi.org/10.1016/j.jcrs.2004.01.014>.

</div>

<div id="ref-bach">

Schulze-Bonsel, Kilian, Nicolas Feltgen, Hermann Burau, Lutz Hansen, and
Michael Bach. 2006. “Visual Acuities ‘Hand Motion’ and ‘Counting
Fingers’ Can Be Quantified with the Freiburg Visual Acuity Test.”
*Investigative Ophthalmology & Visual Science* 47 (3): 1236–40.
<https://doi.org/10.1167/iovs.05-0981>.

</div>

</div>
