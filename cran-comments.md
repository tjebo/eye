## Resubmission #2
Changes in this second resubmission:
* Extended on the description field in DESCRIPTION
* Included references in the description field in DESCRIPTION for   
  methods and theoretical background. 
* Added proper copyright holder to MIT license
* Decision to remove geom_trail (a ggplot2 extension) from eye, because it does   not fully fit to the scope of this package. This resulted in:
    - Removal of one package dependency (ggplot2) 
    - Removal of code that based on code by third person Teun van den Brand.
* Added \value to Rd files 
* Conversion issue fixes:
    - va_methods fixed conversion to and from snellen decimal
    - clean_va removes non-snellen character strings
    - va() removed "from" argument and simplified to logical argument 
    "from_logmar"
    - va() added argument "mixed" for more flexibility in cases of mixed      
      notation.
    - va_dissect() now also returns vector of class `va`
  
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
