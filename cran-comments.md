## Submission version 1.0.0

In the opinion of the package maintainer the changes were substantial enough to justify a major version change. 

## Changes:
### Functions
* issue #6: introduce new verbs to convert to specific VA class (simple wrapper for `va(x, to = ...)`)
* `recodeye()`:
    * recognises "both/ou/b" for both eyes. 
    * dropunknown unclear codings are converted to NA by default (with warning)
    * removed numcode argument (other numeric codes can be passed via "to")
* `eyes()`:
    * fix issue #16 (eyes: accepting both quoted and unquoted arguments)
    * add stop() when main argument not a data frame
    * fix issue #17 `eyes()` now also deals with "both eye cases"
    * fix issue #18 (now also finding eye columns with funny names, e.g. EyeName)
    * fix issue #19 (now returns meaningful result on empty data frames)
    * removed "report" argument 
* `eyestr()`:
    * moved eyes_to_str step from eyes() to eyestr()
* `age()` 
    rename to getage() in order to avoid potential and even likely clashes with the         users environment, especially when using age() within the local environment of a        data frame (e.g., using with())
* `blink()` 
    message that this function will no longer be 
    maintained, but kept in the package. 
* `reveal():`
    fix issue #15 (reveal: unused group levels are dropped)
* `isNAstring` now also converts "-" string to NA
* new exported function: `tidyNA()` cleans NA entries

### Documentation
* Add CITATION file (add inst folder)
* Add URLs to the description file, including bug report field
* Update documentation eye.Rd and in the vignette about removed data sets.
* add more details to recodeye() documentation
* Update description field in DESCRIPTION 
* Update import field description: add package "eyedata"

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
