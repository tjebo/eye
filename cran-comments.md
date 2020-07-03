## Submission version 0.1.1
This is a first patch for version 0.1

Changes:

* Add CITATION file (add inst folder)
* Add URLs to the description file, including bug report field
* Update the inbuilt data set `amd`, because it was in the meanwhile updated by the data set curator (replaced erroneous entries with missing values). 
* This also led to renaming and simplifying of given data
* added url to original article for amd data to its documentation
* Second data set: "dme". The data set curator of the first data set "amd" was quite excited about this package and asked if I wanted to include this second data set as well. The .rda file is 85kB in size, so should not have added much to the size of the package.
* Update information about data sets in eye.Rd and in the vignette.

## Test environments
* local OS X install, R 4.0.0
* ubuntu Ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel and release), R 4.0.0
* R-hub

## R CMD check results
There were no ERRORs or WARNINGs or NOTEs. 

## Downstream dependencies
There are currently no downstream dependencies for this package.
