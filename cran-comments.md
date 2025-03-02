## Submission eye 1.2.2

### Documentation fixes 
- update documentation to reflect updated R documentation structure:
1) fixed hyperlink in description (added "//")
2) fixed eye package help file: added "@ alias eye-package" to eye.Rd
3) fixed link inconsistency in va.Rd (removed redundant mention to authors of referenced article). 

### Function updates
- `recodeye` and `eyes` now also recognize "BE" as coding string for "both eyes"



## CHECK ENVIRONMENTS
* local OS X install, R 4.1.1 ("Kick Things")
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
#>   DOI: 10.1167/iovs.05-0981
#>     From: DESCRIPTION
#>     Status: Forbidden
#>     Message: 403

COMMENT: I am truly sorry, but this is the DOI to this article! 

### CRAN package checks
Version: 1.2.0 
Check: LazyData 
Result: NOTE 
     'LazyData' is specified without a 'data' directory 
     
COMMENT: Removed line `LazyData: true` from description file

## Downstream dependencies
There are currently no downstream dependencies for this package.
