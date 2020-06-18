# eye 0.1.1
bug fixes
* Issue #7 and #8 ETDRS converts to all ETDRS integers and conversion to Snellen   without specifying type
* Issue #10 logmar with quali conversion 
* clean_va now removes non-VA entries, thus making it less likely to get "mixed VA"


# eye 0.1.0
the eye package is online!
eye is a tool to facilitate common tasks in ophthalmic research.

Its visual acuity conversion function `va()` provides eye researchRs with a long coveted tool to handle different visual acuity notations. It works with Snellen, logMAR and ETDRS.

`recodeye` allows an easy recoding of the eye variable.

It also has some other functions which are mainly intelligent wrapper around already existing functionalities, such as `myop` and `hyperop` for reshaping eye specific data, `reveal` or `blink`.

See your data with a new `eye`.
