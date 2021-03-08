## Submission eye version 1.1.0  
summary of changes: 
- new features for function `eyestr`
- important bug fixes

Details: 
- eyestr: new "english" argument for more flexibility (#26)
- eyestr removed "para" argument 
- eyestr prints one eye correctly in singular (#25)
- eyestr added "caps" argument for more flexibility of spelling
- eyestr removed "UK" argument (there is no difference in spelling for numbers :)
- eyes fixed missing reference to column (#27)
- added internal function `tidyNA_low` to avoid double tolower call
- recodeye fixed fail when trailing white space (#28)
- which_va fixed conversion error when mixed ETDRS and logmar (#29)
- update citation file

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

### R-hub
There was one NOTE: 
#> Found the following (possibly) invalid DOIs: 
#> From: DESCRIPTION
#> 
#>     Status: Forbidden
#>   DOI: 10.1167/iovs.05-0981

COMMENT: I am truly sorry, but this is the DOI to this article! 

## Downstream dependencies
There are currently no downstream dependencies for this package.
