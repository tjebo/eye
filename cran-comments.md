## Submission version 1.0.0

In the opinion of the package maintainer the changes were substantial enough to justify a major version change (for details see below). 

## CHECKS
### Test environments
* local OS X install, R 4.0.3
* ubuntu Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2 (2020-06-22)
* win-builder (devel and release), R 4.0.3 
* R-hub (Windows Server 2008 R2 SP1, R-devel, 32/64 bit)
`devtools::check_rhub(platform="windows-x86_64-devel", env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`

## CHECK RESULTS
### R CMD check (local)
There were no ERRORs or WARNINGs or NOTEs. 
### Travis
There were no ERRORs or WARNINGs or NOTEs. 
### win-builder
There were no ERRORs or WARNINGs or NOTEs. 
### R-hub
There were two NOTEs: Found (possibly) invalid url / DOI

- https://doi.org/10.1167/iovs.05-0981
- DOI: 10.1001/jamaophthalmol.2020.5044
- DOI: 10.1136/bmjopen-2018-027441
- DOI: 10.1167/iovs.05-0981

I checked those urls and dois, all seem valid. 

## Downstream dependencies
There are currently no downstream dependencies for this package.

## CHANGES
### DESCRIPTION and CITATION file
* Add CITATION file (add inst folder)
* Add URLs to the description file, including bug report field
* Update description field in DESCRIPTION 
* Update import field description: add package "eyedata"

### Functions and objects (details)
* `va`
    * add arguments "from", "noplus" and "smallstep"
    * removed "from_logmar" and "logmarstep"
    * VA notation guessing is not done by single element any more, but will be 
      forced choice either by most likely or with argument "from". Any implausible  
      values will be forced to NA.
    * issue #6: introduce new verbs to convert to specific VA class (simple wrapper       for `va(x, to = ...)`)
    * issue #21 weird snellen values (e.g., 20/41) can now also converted to other
      snellen values. 
    * New S3 methods introduced to check plausibility of entries by VA class. 
    * Adding plus/minus entries is now done by actual logmar values rather than via 
      lookup in the VA chart (rewrite of `snellen_steps` function)
* `recodeye()`:
    * recognises "both/ou/b" for both eyes. 
    * dropunknown unclear codings are converted to NA by default (with warning)
    * removed numcode argument (other numeric codes can be passed via "to")
* `reveal():`
    fix issue #13 (reveal: unused group levels are dropped)
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
    rename to getage() in order to avoid potential and even likely clashes with the
    users environment, especially when using age() within the local environment 
    of a data frame (e.g., using with())
* `blink()` 
    message that this function will no longer be 
    maintained, but kept in the package. 
* `isNAstring` now also converts "-" string to NA
* new exported function: `tidyNA()` cleans NA entries
* `clean_va` returns numeric vector if all entries are numeric
* removed functions va_dissect() and which_va_dissect()

### Data sets
* Removed data set amd - this will be now part of the dedicated ophthalmic data
set package "eyedata" (currently in submission to CRAN)
* removed data set 'va_quali'


