## Submission eye 1.0.1
1) This is an important bug fix for v1.0.0 - fixing a dangerous bug of an erroneous visual acuity notation conversion (more details see "changes"). This was noticed unfortunately (or: luckily?) only shortly after the recent package upgrade.  

2) fixed NOTE from CRAN package checks (eye 1.0.0): 
#> Check: dependencies in R code 
#>     Namespace in Imports field not imported from: ‘eyedata’
#>      All declared Imports should be used. 

COMMENT: in DESCRIPTION, moved eyedata (>= 0.1.0) to the "Suggests" section

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
There was one NOTE. 
#> checking CRAN incoming feasibility ... NOTE
#> ...
#> Days since last update: 2

COMMENT: See above. This is a horrendous bug that I had to immediately fix. 

### R-hub
There was one NOTE: 
#> Found the following (possibly) invalid DOIs: 
#> From: DESCRIPTION
#> 
#>     Status: Forbidden
#>   DOI: 10.1167/iovs.05-0981

COMMENT: I am sorry :( This is truly the DOI to this article! 

## Downstream dependencies
There are currently no downstream dependencies for this package.

## Changes - details
- bug fix: erroneous conversion of quality visual acuity entries when VA in ETDRS notation (#24)
- removed deprecated function age()
- VA chart now as internal data, not exported
- internally used S3 methods not exported to namespace any longer:
  - convertVA 
  - checkVA
