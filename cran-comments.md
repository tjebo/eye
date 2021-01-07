## Submission eye 1.0.0
This is a major version upgrade to eye 0.1.0 (for details see below). 

## CHECK ENVIRONMENTS
* local OS X install, R 4.0.3
* ubuntu Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2 (2020-06-22)
* win-builder (devel and release), R 4.0.3 
* R-hub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit with
`devtools::check_rhub(platform = "windows-x86_64-devel", env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`

## CHECK RESULTS
### R CMD check (local)
There were no ERRORs or WARNINGs or NOTEs. 
### Travis
There were no ERRORs or WARNINGs or NOTEs. 
### win-builder
There were no ERRORs or WARNINGs or NOTEs.
### R-hub
There was one NOTE: 
#> Found the following (possibly) invalid DOIs: 
#> From: DESCRIPTION
#> 
#>     Status: Forbidden
#>   DOI: 10.1167/iovs.05-0981

#### Comment: 
I am sorry, but I cannot help it :( This is the doi to this article! 

## Downstream dependencies
There are currently no downstream dependencies for this package.

## CHANGES - details
### DESCRIPTION and CITATION file
* Add CITATION file (add inst folder)
* Add URLs to the description file, including bug report field
* Update description field in DESCRIPTION 
* Update import field description: add package "eyedata"

## Functions and objects (details)
### Visual acuity handling
* introduced new verbs `as_...` and `to_...` for class conversion for nice integration into known R grammar (#6) - simple wrapper for `va(x, to = ...)`
* add arguments "from", "noplus" and "smallstep"
* removed "from_logmar" and "logmarstep"
* VA notation guessing is not done by single element any more, but will be forced choice either by most likely or with argument "from". Any implausible values will be forced to NA.
* weird snellen values (e.g., 20/41) are now also converted to other snellen values (#21). 
* New S3 methods introduced to check plausibility of entries by VA class. 
* Adding plus/minus entries is now done by actual logmar values rather than via lookup in the VA chart (rewrite of `snellen_steps` function)
* new function `va_mixed` for vectors of mixed VA notations (replacing previous `va_dissect`)
* new function `cleanVA`

### Smaller fixes
* `recodeye()` recognises "both/ou/b" for both eyes. 
* dropunknown unclear codings are converted to NA by default (with warning)
* removed numcode argument (other numeric codes can be passed via "to")
* `eyes()` accepting both quoted and unquoted arguments (#16)
* add stop() when main argument not a data frame
* `eyes()` now also deals with "both eye cases" (#17)
* now also finding eye columns with funny names, e.g. EyeName (#18) 
* eyes now returns meaningful result on empty data frames) (#19)
* removed "report" argument 
* `reveal()` does not drop unused group levels (#13)
* `eyestr()` moved eyes_to_str step from eyes() to eyestr()
* `age()` renamed to getage() in order to avoid potential and even likely clashes with the users environment, especially when using age() within the local environment of a data frame (e.g., using with())
* `blink()` is deprecated. message that this function will no longer be maintained, but kept in the package. 
* `isNAstring` now also converts "-" string to NA
* new exported function: `tidyNA()` cleans NA entries
* `clean_va` returns numeric vector if all entries are numeric
* removed functions va_dissect() and which_va_dissect()

### Data sets
* Removed data set amd - this will be now part of the dedicated ophthalmic data set package "eyedata"
* removed data set 'va_quali' which was anyways a part of "va_chart"


