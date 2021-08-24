## Submission eye v1.2.1

Bug fixes: 
- if no eye column is found, `eyes` also returns a list #31
- `set_eye_strings` also updates eye column #32 
- partial eye strings recognized in column names #33
- `to_etdrs` now correctly always returns integer class vector #34
- conversion of pure "NA" saved as character now correctly returns `NA` #35
- logMAR will return rounded values to the first digit when `noplus = TRUE` #37
- `to_etdrs` from pure qualitative values correctly returns 0 for NPL and PL #38

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

## Downstream dependencies
There are currently no downstream dependencies for this package.
