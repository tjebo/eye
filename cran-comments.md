## Submission eye 1.0.1
This is a minor version upgrade to eye 1.0.0 - this resolves a bug from an erroneous conversion of visual acuity entries (more details see "changes").

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
- bug fix: conversion of quality visual acuity entries when ETDRS (#24)
- removed deprecated function age()
- VA chart now as internal data, not exported
- internally used methods not exported to namespace any longer:
  - convertVA 
  - checkVA
  



