## Submission eye 1.2.2
CRAN request for updates - final deadline given 2025-10-17

### Documentation fixes 
update documentation to reflect updated R documentation structure:
- fixed hyperlink in description 
- fixed eye package help file: added "@ alias eye-package" to eye.Rd
- fixed link inconsistency in va.Rd (removed redundant mention to authors of referenced article). 
- fixed use of citEntry in CITATION file (CRAN NOTE)

## CHECK ENVIRONMENTS
* local OS X install, R 4.1.1 ("Kick Things")
* ubuntu Ubuntu 16.04.6 LTS (on travis-ci), R 4.0.2 (2020-06-22)
* win-builder (devel and release), R 4.0.3 
* R-hub: Windows Server 2008 R2 SP1, R-devel, 32/64 bit with
`devtools::check_rhub(platform = "windows-x86_64-devel", env_vars=c(R_COMPILE_AND_INSTALL_PACKAGES = "always"))`

## CHECK RESULTS
### R CMD check (local)
0 errors | 0 warnings | 0 notes

### win-builder

### macOS builder 
Build system: r-devel-macosx-arm64|4.5.1|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 14.2.0

Status: OK

### R-hub

### CRAN package checks

## Rverse dependencies
There are currently no reverse dependencies for this package.
