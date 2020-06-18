## Resubmission #2

Changes in this second resubmission:

### Submission comment fixes
* Extended on the description field in DESCRIPTION
* Included references in the description field in DESCRIPTION for   
  methods and theoretical background. 
* Added contributor Antoine Fabri to Authors@R field (he contributed to age())
* Added proper copyright holder to MIT license
* Added \value to Rd files for exported functions
* Decision to remove geom_trail (a ggplot2 extension) from eye, because it does   not fully fit to the scope of this package. This resulted in:
    - Removal of one package dependency (ggplot2) 
    - Removal of code contributed by Teun van den Brand.

### Additional issues fixed:
    * Visual acuity conversion issues
      - va_methods fixed conversion to and from snellen decimal
      - clean_va removes non-snellen character strings
      - va() removed "from" argument and simplified to logical argument 
      "from_logmar"
      - va() added argument "mixed" for more flexibility in cases of mixed      
        notation.
      - va_dissect() now also returns vector of class `va`
    * eyestr fix error when called on patient only data
  
## Resubmission
This is a resubmission. In this version I have:

* Changed the link to ggplot2 in eye.Rd to a web-anchored link

## Test environments
* local OS X install, R 4.0.0
* ubuntu Ubuntu 16.04 (on travis-ci), R 4.0.0
* win-builder (devel and release), R 4.0.0
* R-hub


## R CMD check results
There were no ERRORs or WARNINGs. 

There was one NOTE:
* This is the first submission for this package. 

## Downstream dependencies
There are currently no downstream dependencies for this package.
