eye
================

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/tjebo/eye.svg?branch=master)](https://travis-ci.com/tjebo/eye)
<!-- badges: end -->

WARNING - eye is currenty under heavy development - would recommend
currently to install eye from CRAN

See more with *eye*

## Purpose

*eye* is dedicated to facilitate very common tasks in ophthalmic
research.

  - Visual acuity conversion for snellen, logMAR and ETDRS
  - Counting patients and eyes
  - Recode eye strings
  - Reshape eye specific variables  
  - Summarizing data with common statistics (mean, sd, n, range)
  - Calculating age of patients

## Installation

You can install eye from
[CRAN](https://cran.r-project.org/web/packages/eye/index.html) using
`install.packages("eye")`

Or you can install the development version from github:

    # install.packages("devtools")
    devtools::install_github("tjebo/eye")

Installing eye will also install
[eyedata](https://github.com/tjebo/eyedata/), a package collating open
source ophthalmic data sets.

## eye Features

  - va: [Conversion of visual acuity notations](#va)
  - eyes: [Easy count of patients and eyes](#eyes)
  - eyestr: [return eye count as text for your report](#eyestr)
  - recodeye: [recode eye variable](#recodeye)
  - myop: [Make your eye data long](#myop)
  - hyperop: [Make your eye data wide](#hyperop)
  - blink: [Perceive your data in a blink of an eye](#blink)
  - Visual acuity [conversion chart](#va-conversion)
  - reveal: [Get common summary statistics](#reveal)
  - age: [Calculate age](#getage)

## Details and examples

### va

Easy conversion from visual acuity notations in a single call to `va()`:
Automatic detection of VA notation and convert to logMAR by default (but
you can convert to snellen or ETDRS as well). For some more details see
[VA conversion](#va-conversion)

``` r
## automatic detection of VA notation and converting to logMAR by default
x <- c(23, 56, 74, 58) ## ETDRS letters
va(x, to = "logmar")
#> From etdrs
#> [1] 1.24 0.58 0.22 0.54

va(x, to = "snellen") ## ... or convert to snellen
#> From etdrs
#> [1] "20/320" "20/80"  "20/32"  "20/70"

# or with the wrapper 
to_logmar(x)
#> From etdrs
#> [1] 1.24 0.58 0.22 0.54
to_snellen(x)
#> From etdrs
#> [1] "20/320" "20/80"  "20/32"  "20/70"

## A mix of notations, and weird NA entries
x <- c("NLP", "0.8", "34", "3/60", "2/200", "20/50", "  ", ".", "-", "NULL")
va(x, to = "snellen", type = "m")
#> From snellen. Could be snellen, logmar, snellendec, etdrs
#> 6x NA introduced for: 0.8, 34, , ., -, null
#>  [1] "6/6000" NA       NA       "6/120"  "6/600"  "6/15"   NA       NA      
#>  [9] NA       NA

## "plus/minus" entries are converted to the most probable threshold (any spaces allowed) (currently not stable, will be fixed)
x <- c("20/200", "20/200 - 1", "6/6", "6/6-2", "20/50 + 3", "20/50 -2")
va(x, to = "snellen")
#> From snellen
#> [1] "20/200" "20/200" "20/20"  "20/20"  "20/50"  "20/50"

## or evaluating them as logmar values 
to_snellen(x, logmarstep = TRUE)
#> From snellen
#> ignoring +/- entries when converting to snellen with logmarstep TRUE
#> [1] "20/200" "20/200" "20/20"  "20/20"  "20/50"  "20/50"
```

### eyes

Count patient and eyes (**eyes** or **eyestr**)

``` r
library(eyedata)
eyes(amd2)
#> patients     eyes    right     left 
#>     3357     3357     1681     1676
```

#### eyestr

Same as `eyes`, but as text for reports

``` r
eyestr(amd2)
#> [1] "3357 eyes of 3357 patients"

 ## Numbers smaller than or equal to 12 will be real English

eyestr(head(amd2, 100))
#> [1] "Eleven eyes of eleven patients"
```

### recodeye

Makes recoding eye variables very easy. It deals with weird missing
entries like `"."` and `""`, or `"N/A"`

``` r
x <- c("r", "re", "od", "right", "l", "le", "os", "left")
recodeye(x)
#> [1] "r" "r" "r" "r" "l" "l" "l" "l"

## or with "both eyes"
x <- c(x, "both", "ou")
recodeye(x)
#>  [1] "r" "r" "r" "r" "l" "l" "l" "l" "b" "b"

## chose the resulting codes
recodeye(x, to = c("od", "os", "ou"))
#>  [1] "od" "od" "od" "od" "os" "os" "os" "os" "ou" "ou"

## Numeric codes 0:1/ 1:2 are recognized 
x <- 1:2
recodeye(x)
#> Eyes coded 1:2. Interpreting r = 1
#> [1] "r" "l"

## with weird missing values
x <- c(1:2, ".", NA, "", "    ")
recodeye(x)
#> Missing values and/or meaningless strings contained
#> Eyes coded 1:2. Interpreting r = 1
#> [1] "r" "l" NA  NA  NA  NA

## Or if you have weird codes for eyes
x <- c("alright", "righton", "lefty","leftover")

recodeye(x, eyecodes = list(r = c("alright","righton"), l = c("lefty","leftover")))
#> [1] "r" "r" "l" "l"
```

### reveal

Show common statistics for all numeric columns, for the entire cohort or
aggregated by group(s):

``` r
reveal(iris)
#>            var mean  sd   n min max
#> 1 Sepal.Length  5.8 0.8 150 4.3 7.9
#> 2  Sepal.Width  3.1 0.4 150 2.0 4.4
#> 3 Petal.Length  3.8 1.8 150 1.0 6.9
#> 4  Petal.Width  1.2 0.8 150 0.1 2.5
#> 5      Species  2.0 0.8 150 1.0 3.0

reveal(iris, by = "Species") #can be several groups
#>       Species          var mean  sd  n min max
#> 1      setosa Sepal.Length  5.0 0.4 50 4.3 5.8
#> 2      setosa  Sepal.Width  3.4 0.4 50 2.3 4.4
#> 3      setosa Petal.Length  1.5 0.2 50 1.0 1.9
#> 4      setosa  Petal.Width  0.2 0.1 50 0.1 0.6
#> 5  versicolor Sepal.Length  5.9 0.5 50 4.9 7.0
#> 6  versicolor  Sepal.Width  2.8 0.3 50 2.0 3.4
#> 7  versicolor Petal.Length  4.3 0.5 50 3.0 5.1
#> 8  versicolor  Petal.Width  1.3 0.2 50 1.0 1.8
#> 9   virginica Sepal.Length  6.6 0.6 50 4.9 7.9
#> 10  virginica  Sepal.Width  3.0 0.3 50 2.2 3.8
#> 11  virginica Petal.Length  5.6 0.6 50 4.5 6.9
#> 12  virginica  Petal.Width  2.0 0.3 50 1.4 2.5
```

### getage

  - Calculate age in years, as [periods or
    durations](https://lubridate.tidyverse.org/articles/lubridate.html#time-intervals)

<!-- end list -->

``` r
dob <- c("1984-10-16", "2000-01-01")

## If no second date given, the age today
getage(dob)
#> [1] 36.2 21.0
getage(dob, "2000-01-01")                                                    
#> [1] 15.2  0.0
```

### myop - Make your data long

Often enough, there are right eye / left eye columns for more than one
variable, e.g., for both IOP and VA. This may be a necessary data formal
for specific questions. However, “eye” is also variable (a dimension of
your observation), and it can also be stored in a separate column. The
data would be “longer”.

Indeed, R requires exactly this data shape for many tasks: “eye\[r/l\]”
as a separate column, and each eye-related variable (e.g., IOP or VA) in
their own dedicated column.

`myop` provides an easy to use API for an automatic reshape of your data
to a “myop” format.

``` r
## Simple data frame with one column for right eye and left eye.
iop_wide
#>   id iop_r iop_l
#> 1  a    11    14
#> 2  b    12    15
#> 3  c    13    16

myop(iop_wide)
#> # A tibble: 6 x 3
#>   id    eye   iop  
#>   <chr> <chr> <chr>
#> 1 a     r     11   
#> 2 a     l     14   
#> 3 b     r     12   
#> 4 b     l     15   
#> 5 c     r     13   
#> 6 c     l     16
```

Or another example with many more variables:

<details>

<summary>Click to unfold code to create `wide_df` </summary>

``` r
wide_df <- data.frame(
  id = letters[1:4], 
  surgery_right = c("TE", "TE", "SLT", "SLT"),
  surgery_left = c("TE", "TE", "TE", "SLT"),
  iop_r_preop = 21:24, iop_r_postop = 11:14,
  iop_l_preop = 31:34, iop_l_postop = 11:14, 
  va_r_preop = 41:44, va_r_postop = 45:48,
  va_l_preop = 41:44, va_l_postop = 45:48
)
```

</details>

<img src="man/figures/messy.png"/>

``` r
myop_df <- myop(wide_df)

myop_df
#> # A tibble: 8 x 7
#>   id    eye   surgery iop_preop iop_postop va_preop va_postop
#>   <chr> <chr> <chr>   <chr>     <chr>      <chr>    <chr>    
#> 1 a     r     TE      21        11         41       45       
#> 2 a     l     TE      31        11         41       45       
#> 3 b     r     TE      22        12         42       46       
#> 4 b     l     TE      32        12         42       46       
#> 5 c     r     SLT     23        13         43       47       
#> 6 c     l     TE      33        13         43       47       
#> 7 d     r     SLT     24        14         44       48       
#> 8 d     l     SLT     34        14         44       48
```

### hyperop

If you actually need certain eye-related variables spread over two
columns, `hyperop()` is your friend:

``` r
hyperop(myop(iop_wide), iop)
#> # A tibble: 3 x 3
#>   id    r_iop l_iop
#>   <chr> <chr> <chr>
#> 1 a     11    14   
#> 2 b     12    15   
#> 3 c     13    16

hyperop(myop_df, cols = matches("va|iop"))
#> # A tibble: 5 x 10
#>   id    surgery r_iop_preop r_iop_postop r_va_preop r_va_postop l_iop_preop
#>   <chr> <chr>   <chr>       <chr>        <chr>      <chr>       <chr>      
#> 1 a     TE      21          11           41         45          31         
#> 2 b     TE      22          12           42         46          32         
#> 3 c     SLT     23          13           43         47          <NA>       
#> 4 c     TE      <NA>        <NA>         <NA>       <NA>        33         
#> 5 d     SLT     24          14           44         48          34         
#> # … with 3 more variables: l_iop_postop <chr>, l_va_preop <chr>,
#> #   l_va_postop <chr>
```

### blink

See your data in a blink of an eye - wrapper around [`myop`](#myop),
[`eyes`](#eyes), [`va`](#va) and [`reveal`](#reveal). It will look for
VA and for IOP columns and provide the summary stats for the entire
cohort and for right and left eyes for each variable.

[**This requires a certain format of your names and
codes**](#names-and-codes)

``` r
blink(wide_df)
#> The lifecycle of blink() has expired. It will no longer be
#>   maintained, but will be kept in the package.
#> From etdrs
#> From etdrs
#> 
#> ── blink ───────────────────────────────────────────────────────────────────────
#> ══ Data ════════════════════════════════
#> # A tibble: 8 x 7
#>   id    eye   surgery iop_preop iop_postop va_preop va_postop
#>   <chr> <chr> <chr>   <chr>     <chr>      <logmar> <logmar> 
#> 1 a     r     TE      21        11         0.88     0.80     
#> 2 a     l     TE      31        11         0.88     0.80     
#> 3 b     r     TE      22        12         0.86     0.78     
#> 4 b     l     TE      32        12         0.86     0.78     
#> 5 c     r     SLT     23        13         0.84     0.76     
#> 6 c     l     TE      33        13         0.84     0.76     
#> 7 d     r     SLT     24        14         0.82     0.74     
#> 8 d     l     SLT     34        14         0.82     0.74     
#> 
#> ══ Count of patient and eyes ═══════════
#> patients     eyes    right     left 
#>        4        8        4        4 
#> 
#> ══ Visual acuity ═══════════════════════
#> 
#> ── $VA_total (all eyes)
#>         var mean sd n min max
#> 1  va_preop  0.8  0 8 0.8 0.9
#> 2 va_postop  0.8  0 8 0.7 0.8
#> 
#> ── $VA_eyes (right and left eyes)
#>   eye       var mean sd n min max
#> 1   l  va_preop  0.8  0 4 0.8 0.9
#> 2   l va_postop  0.8  0 4 0.7 0.8
#> 3   r  va_preop  0.8  0 4 0.8 0.9
#> 4   r va_postop  0.8  0 4 0.7 0.8
#> 
#> ══ Intraocular pressure ════════════════
#> 
#> ── $IOP_total (all eyes)
#>          var mean  sd n min max
#> 1  iop_preop 27.5 5.5 8  21  34
#> 2 iop_postop 12.5 1.2 8  11  14
#> 
#> ── $IOP_eyes (right and left eyes)
#>   eye        var mean  sd n min max
#> 1   l  iop_preop 32.5 1.3 4  31  34
#> 2   l iop_postop 12.5 1.3 4  11  14
#> 3   r  iop_preop 22.5 1.3 4  21  24
#> 4   r iop_postop 12.5 1.3 4  11  14
```

## Names and codes

**eye works smoother with tidy data** (any package does, really\!)

An important part of tidy data are good names. [Learn more about tidy
data.](https://tidyr.tidyverse.org/articles/tidy-data.html)

### Tips and rules for naming:

1)  Don’t be too creative with your names\!
2)  Use common coding:

<!-- end list -->

  - **eyes**: “r”, “re”, “od”, “right” - or numeric coding r:l = 0:1 or
    1:2
  - **Visual acuity**: “VA”, “BCVA”, “Acuity”
  - **Intraocular pressure**: “IOP”, “GAT”, “NCT”, “pressure”
  - **Patient identifier**: “pat”, “patient”, “ID” (ideally both:
    “patientID” or “patID”)

<!-- end list -->

3)  Column names:

<!-- end list -->

  - No spaces\!
  - Do not use numeric coding for eyes in column names
  - Separate eye and VA and IOP codes with underscores
    (“bcva\_l\_preop”, “VA\_r”, “left\_va”, “IOP\_re”)
  - Keep names short
  - Don’t use underscores when you don’t need to: Consider each section
    divided by an underscore as a relevant characteristic of your
    variable. E.g., “preop” instead of “pre\_op”, or simply “VA” instead
    of “VA\_ETDRS\_Letters”

### Name examples

Good names (`eye` will work nicely)

``` r
## right and left eyes have common codes
## information on the tested dimension is included ("iop")
## VA and eye strings are separated by underscores
## No unnecessary underscores.
names(wide_df)
#>  [1] "id"            "surgery_right" "surgery_left"  "iop_r_preop"  
#>  [5] "iop_r_postop"  "iop_l_preop"   "iop_l_postop"  "va_r_preop"   
#>  [9] "va_r_postop"   "va_l_preop"    "va_l_postop"

names(iop_wide) 
#> [1] "id"    "iop_r" "iop_l"
```

OK names (`eye` will work)

``` r
## Id and Eye are common names, there are no spaces
## VA is separated from the rest with an underscore
## BUT: 
## The names are quite long 
## There is an unnecessary underscore (etdrs are always letters). Better just "VA"
c("Id", "Eye", "FollowupDays", "BaselineAge", "Gender", "VA_ETDRS_Letters", 
"InjectionNumber")
#> [1] "Id"               "Eye"              "FollowupDays"     "BaselineAge"     
#> [5] "Gender"           "VA_ETDRS_Letters" "InjectionNumber"

## All names are commonly used (good!)
## But which dimension of "r"/"l" are we exactly looking at? 
c("id", "r",  "l")
#> [1] "id" "r"  "l"
```

Bad names (`eye` will fail)

``` r
## VA/IOP not separated with underscore
## `eye` won't be able to recognize IOP and VA columns
c("id", "iopr", "iopl", "VAr", "VAl")
#> [1] "id"   "iopr" "iopl" "VAr"  "VAl"

## A human may think this is clear
## But `eye` will fail to understand those variable names
c("person", "goldmann", "vision")
#> [1] "person"   "goldmann" "vision"

## Not even clear to humans
c("var1", "var2", "var3")
#> [1] "var1" "var2" "var3"
```

### How do I rename columns in R?

When I started with R, I found it challenging to rename columns and I
found the following threads on stackoverflow very helpful:

  - [Rename single column](https://stackoverflow.com/q/7531868/7941188)
  - [Rename columns with named
    vector](https://stackoverflow.com/q/20987295/7941188)

I find the two following methods straight forward:

``` r
# I've got a data frame with unfortunate names:
name_mess <- data.frame(name = "a", oculus = "r", eyepressure = 14, vision = 0.2)
names(name_mess)
#> [1] "name"        "oculus"      "eyepressure" "vision"

## rename all names
names(name_mess) <- c("patID", "eye", "IOP", "VA")
names(name_mess)
#> [1] "patID" "eye"   "IOP"   "VA"
```

``` r
## To rename only specific columns, even if you are not sure about their exact position:
names(name_mess)[names(name_mess) %in% c("name", "vision")] <- c("patID", "VA")
names(name_mess)
#> [1] "patID"       "oculus"      "eyepressure" "VA"
```

## Important notes

**I do not assume responsability for your data or analysis**. Please
always keep a critical mind when working with data - if you do get
results that seem implausible, there may be a chance that the data is in
an unfortunate shape for which `eye` may not be suitable.

## VA conversion

  - VA conversion between Snellen, ETDRS and logMAR is based on charts
    and formulas in (Holladay [2004](#ref-holladay)), (Beck et al.
    [2003](#ref-beck)) and (Gregori, Feuer, and Rosenfeld
    [2010](#ref-gregori))
  - Categories **counting fingers** and **hand movements** are converted
    following (Schulze-Bonsel et al. [2006](#ref-bach))
  - Categories **(no) light perception** are converted following the
    suggestions by Michael Bach

### VA conversion chart

This chart is included in the package (`va_chart`)

<div style="font-size:8 pt;">

| Snellen feet | Snellen meter | Snellen decimal | logMAR | ETDRS | Categories |
| ------------ | ------------- | --------------- | ------ | ----- | ---------- |
| 20/20000     | 6/6000        | 0.001           | 3      | 0     | NLP        |
| 20/10000     | 6/3000        | 0.002           | 2.7    | 0     | LP         |
| 20/4000      | 6/1200        | 0.005           | 2.3    | 0     | HM         |
| 20/2000      | 6/600         | 0.01            | 1.9    | 2     | CF         |
| 20/800       | 6/240         | 0.025           | 1.6    | 5     | NA         |
| 20/630       | 6/190         | 0.032           | 1.5    | 10    | NA         |
| 20/500       | 6/150         | 0.04            | 1.4    | 15    | NA         |
| 20/400       | 6/120         | 0.05            | 1.3    | 20    | NA         |
| 20/320       | 6/96          | 0.062           | 1.2    | 25    | NA         |
| 20/300       | 6/90          | 0.067           | 1.18   | 26    | NA         |
| 20/250       | 6/75          | 0.08            | 1.1    | 30    | NA         |
| 20/200       | 6/60          | 0.1             | 1.0    | 35    | NA         |
| 20/160       | 6/48          | 0.125           | 0.9    | 40    | NA         |
| 20/125       | 6/38          | 0.16            | 0.8    | 45    | NA         |
| 20/120       | 6/36          | 0.167           | 0.78   | 46    | NA         |
| 20/100       | 6/30          | 0.2             | 0.7    | 50    | NA         |
| 20/80        | 6/24          | 0.25            | 0.6    | 55    | NA         |
| 20/70        | 6/21          | 0.29            | 0.54   | 58    | NA         |
| 20/63        | 6/19          | 0.32            | 0.5    | 60    | NA         |
| 20/60        | 6/18          | 0.33            | 0.48   | 61    | NA         |
| 20/50        | 6/15          | 0.4             | 0.4    | 65    | NA         |
| 20/40        | 6/12          | 0.5             | 0.3    | 70    | NA         |
| 20/32        | 6/9.6         | 0.625           | 0.2    | 75    | NA         |
| 20/30        | 6/9           | 0.66            | 0.18   | 76    | NA         |
| 20/25        | 6/7.5         | 0.8             | 0.1    | 80    | NA         |
| 20/20        | 6/6           | 1.0             | 0.0    | 85    | NA         |
| 20/16        | 6/5           | 1.25            | \-0.1  | 90    | NA         |
| 20/15        | 6/4.5         | 1.33            | \-0.12 | 91    | NA         |
| 20/13        | 6/4           | 1.5             | \-0.2  | 95    | NA         |
| 20/10        | 6/3           | 2.0             | \-0.3  | 100   | NA         |

</div>

## Acknowledgements

  - Thanks to **Alasdair Warwick**, **Aaron Lee**, **Tim Yap**,
    **Siegfried Wagner** and **Abraham Olvera** for great suggestions,
    testing and code review.  
  - **Pearse Keane**, **Dun Jack Fu**, **Katrin Fasler** and **Christoph
    Kern** for their contribution of open source data
  - Thanks to [Antoine Fabri](https://github.com/moodymudskipper) for
    his contribution to `getage()`
  - Thanks to Hadley Wickham and all developers of the `tidyverse`
    packages and the packages `roxygen2`, `usethis`, `testthis` and
    `devtools`, all on which `eye` heavily relies.

## Resources

  - [Michael Bach’s homepage](https://michaelbach.de/sci/acuity.html)
  - [Michael Bach on NLP and
    LP](https://michaelbach.de/sci/pubs/Bach2007IOVS%20eLetter%20FrACT.pdf)

## References

<div id="refs" class="references">

<div id="ref-beck">

Beck, Roy W, Pamela S Moke, Andrew H Turpin, Frederick L Ferris, John
Paul SanGiovanni, Chris A Johnson, Eileen E Birch, et al. 2003. “A
Computerized Method of Visual Acuity Testing.” *American Journal of
Ophthalmology* 135 (2): 194–205.
<https://doi.org/10.1016/s0002-9394(02)01825-1>.

</div>

<div id="ref-gregori">

Gregori, Ninel Z, William Feuer, and Philip J Rosenfeld. 2010. “Novel
Method for Analyzing Snellen Visual Acuity Measurements.” *Retina* 30
(7): 1046–50. <https://doi.org/10.1097/iae.0b013e3181d87e04>.

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
