## Submission eye version 1.2.0 
Summary of changes: 
- `eyes()` - now returns lists (of class "eyes") for easier access of count data
- `set_eye_strings()` - set string codes globally! This makes it easier for people using different languages to use eye.
- recodeye: renamed "eyecodes" argument to "eyestrings"
- Change of terminology to "subjects/id" rather than "patients" 
- simplified code

Details: 
- `eyes()`: "details" argument creates object of class "eyes_details", allowing access to in depth count statistics and subject ids 
- This includes new print methods for class "eyes" and "eyes_details"
- change id and eye arguments to "id_col" and "eye_col" - previous code should not break because of partial argument name matching. However, the new names make it clearer as to what those arguments are for.
- calling `set_eye_strings` will super-assign the new codes to the internal
list object eye_codes
- getElem_eyecol now gives precedence to columns that are called "eye" or "eyes" (#30)
- fixed errors in documentation 
- update citation file
- update description file
Simplifications:
- simplified pivoting in myop using the pivot_longer regex feature
- removed ..._chr arguments from getElem_... functions (redundant)
- getElem... now only for vectors (because they were anyways only used for vectors)

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
