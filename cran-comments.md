## Submission version 0.2.0

## Changes:

### Documentation
* Add CITATION file (add inst folder)
* Add URLs to the description file, including bug report field
* Add url to original article for amd data to its documentation
* Update documentation eye.Rd and in the vignette about new data sets.
* Update description field in DESCRIPTION 

### Data sets
* Update the inbuilt data set `amd` and renamed to `amd2`, because it was in the meanwhile updated by the data set curator (replaced erroneous entries with missing values). 
* some variables of `amd2` are renamed to make it more accessible for immediate usage.
* Add one more eye-related open source data set: "amd" (replacing the previous amd data frame)

## Test environments
* local OS X install, R 4.0.0
* ubuntu Ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel and release), R 4.0.0
* R-hub

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 

## Downstream dependencies
There are currently no downstream dependencies for this package.
