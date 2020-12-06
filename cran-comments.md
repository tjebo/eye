## Submission version 1.0.0

In the opinion of the package maintainer the changes were substantial enough to justify a major version change. 

## Changes:
### Functions
* rename `age()` to `getage()` in order to avoid potential and even likely clashes with the users environment, especially when using `age()` within the local environment of a data frame (e.g., using `with()`)
* add message to `blink()` that this function will no longer be 
  maintained, but kept in the package. 

### Documentation
* Add CITATION file (add inst folder)
* Add URLs to the description file, including bug report field
* Update documentation eye.Rd and in the vignette about removed data sets.
* Update description field in DESCRIPTION 

### Data sets
* Removed data set amd - this will be now part of the dedicated ophthalmic data
set package "eyedata" (currently in submission to CRAN)

## Test environments
* local OS X install, R 4.0.0
* ubuntu Ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel and release), R 4.0.0
* R-hub

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 

## Downstream dependencies
There are currently no downstream dependencies for this package.
