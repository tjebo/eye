# eye 1.0.0
Major version upgrade of eye 0.1.0!

## Functions
* rename `age()` to `getage()` in order to avoid potential and even likely clashes with the users environment, especially when using `age()` within the local environment of a data frame (e.g., using `with()`)
* The lifecycle of `blink()` has expired. `blink` will no longer be 
  maintained, but kept in the package. 

## Data sets
* `amd` was removed - it will be now part of the dedicated ophthalmic data
set package "eyedata" (currently in submission to CRAN)

# eye 0.1.0
the eye package is online!
eye is a tool to facilitate common tasks in ophthalmic research.

Its visual acuity conversion function `va()` provides eye researchRs with a long coveted tool to handle different visual acuity notations. It works with Snellen, logMAR and ETDRS.

`recodeye` allows an easy recoding of the eye variable.

It also has some other functions which are mainly intelligent wrapper around already existing functionalities, such as `myop` and `hyperop` for reshaping eye specific data, `reveal` or `blink`.

See your data with a new `eye`.
