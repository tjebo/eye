## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(eye)

## ----va-----------------------------------------------------------------------
## automatic detection of VA notation and converting to logMAR by default
x <- c(23, 56, 74, 58) ## ETDRS letters
va(x)

va(x, to = "snellen") ## ... or convert to snellen

## A mix of notations
x <- c("NLP", "0.8", "34", "3/60", "2/200", "20/50")
va(x)

## "plus/minus" entries are converted to the most probable threshold (any spaces allowed)
x <- c("20/200", "20/200 - 1", "6/6", "6/6-2", "20/50 + 3", "20/50 -2")
va(x)

## or evaluating them as logmar values 
va(x, logmarstep = TRUE)

## on the inbuilt data set:
head(va(amd$VA_ETDRS_Letters), 10) 

## and indeed, there are unplausible ETDRS values in this data set:
range(amd$VA_ETDRS_Letters)

## Any fraction is possible, and empty values
x <- c("CF", "3/60", "2/200", "", "20/40+3", ".", "      ")
va(x)

## but not when converting from one class to the other
x <- c("3/60", "2/200", "6/60", "20/200", "6/9")
va(x, to="snellen", type = "m")

## -----------------------------------------------------------------------------
x <- c("r", "re", "od", "right", "l", "le", "os", "left")
recodeye(x)

## chose the resulting codes
recodeye(x, to = c("right", "left"))

## Or if you have weird codes for eyes
x <- c("alright", "righton", "lefty","leftover")
recodeye(x, eyecodes = list(c("alright","righton"), c("lefty","leftover")))

## Numeric codes 0:1/ 1:2 are recognized 
x <- 1:2
recodeye(x)

## chose the resulting codes
recodeye(x, to = c("right", "left"))

## or, if right is coded with 2)
recodeye(x, numcode = 2:1)

## ----eyestr-------------------------------------------------------------------
eyes(amd)

## or as text for a report
eyestr(amd)

## Complying with journal standards when at beginning of paragraph
eyestr(amd, para = TRUE)
 
## Numbers smaller than or equal to 12 will be real English
eyestr(head(amd, 100))

## But you can turn this off
eyestr(head(amd, 100), small_num = FALSE)

## -----------------------------------------------------------------------------
## the variable has not been exactly named, (but it is probably IOP data), 
## you can specify the dimension with the var argument
wide1 <- data.frame(id = letters[1:3],  r = 11:13 , l = 14:16)
myop(wide1, var = "iop")

## If the dimension is already part of the column names, this is not necessary. 
iop_wide <- data.frame(id = letters[1:3], iop_r = 11:13, iop_l = 14:16)
myop(iop_wide)

## Mildly messy data frame with several variables spread over two columns:
wide_df <- data.frame(
  id = letters[1:4], 
  surgery_right = c("TE", "TE", "SLT", "SLT"),
  surgery_left = c("TE", "TE", "TE", "SLT"),
  iop_r_preop = 21:24, iop_r_postop = 11:14,
  iop_l_preop = 31:34, iop_l_postop = 11:14, 
  va_r_preop = 41:44, va_r_postop = 45:48,
  va_l_preop = 41:44, va_l_postop = 45:48
)

## myop deals with this in a breeze:
myop(wide_df)

## -----------------------------------------------------------------------------
myop_df <- myop(wide_df)
hyperop(myop_df, cols = matches("va|iop"))

## -----------------------------------------------------------------------------
blink(wide_df)

blink(amd)

## -----------------------------------------------------------------------------
name_mess <- data.frame(name = "a", oculus = "r", eyepressure = 14, vision = 0.2)
names(name_mess)

## -----------------------------------------------------------------------------
names(name_mess) <- c("patID", "eye", "IOP", "VA")
names(name_mess)

## ---- include=FALSE-----------------------------------------------------------
names(name_mess) <- c("name", "oculus", "eyepressure", "vision")

## -----------------------------------------------------------------------------
## if you only want to rename one or a few columns: 
names(name_mess)[names(name_mess) %in% c("name", "vision")] <- c("patID", "VA")
names(name_mess)

## -----------------------------------------------------------------------------
## right and left eyes have common codes
## information on the tested dimension is included ("iop")
## VA and eye strings are separated by underscores
## No unnecessary underscores.
names(wide_df)

names(iop_wide) 

## -----------------------------------------------------------------------------
## Id and Eye are common names, there are no spaces
## VA is separated from the rest with an underscore
## BUT: 
## The names are quite long 
## There is an unnecessary underscore (etdrs are always letters). Better just "VA"
names(amd) 

## All names are commonly used (good!)
## But which dimension of "r"/"l" are we exactly looking at? 
c("id", "r",  "l")

## -----------------------------------------------------------------------------
## VA/IOP not separated with underscore
## `eye` won't be able to recognize IOP and VA columns
c("id", "iopr", "iopl", "VAr", "VAl")

## A human may think this is clear
## But `eye` will fail to understand those variable names
c("person", "goldmann", "vision")

## Not even clear to humans
c("var1", "var2", "var3")

## ----stats, warning=FALSE, message=FALSE--------------------------------------
clean_df <- myop(wide_df)
reveal(clean_df)

reveal(clean_df, by = "eye")

reveal(clean_df, by = c("eye", "surgery"))

## ----age, warning=FALSE, message=FALSE----------------------------------------
dob <- c("1984-10-16", "2000-01-01")

## If no second date given, the age today
age(dob)

## If the second argument is specified, the age until then
age(dob, "2000-01-01")                                                    

