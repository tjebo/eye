
# eye 1.0.0
Major version upgrade of eye 0.1.0!

### Functions and objects (details)
* introduced new verbs `as_...` and `to_...` for class conversion for nice integration into known R grammar (issue #6) - simple wrapper for `va(x, to = ...)`
* `va`
    * add arguments "from", "noplus" and "smallstep"
    * removed "from_logmar" and "logmarstep"
    * VA notation guessing is not done by single element any more, but will be forced choice either by most likely or with argument "from". Any implausible values will be forced to NA.
    * issue #6: introduce new verbs to convert to specific VA class
    * issue #21 weird snellen values (e.g., 20/41) can now also converted to other snellen values. 
    * New S3 methods introduced to check plausibility of entries by VA class. 
    * Adding plus/minus entries is now done by actual logmar values rather than via 
      lookup in the VA chart (rewrite of `snellen_steps` function)
* new function `va_mixed` for vectors of mixed VA notations
* `recodeye()`:
    * recognises "both/ou/b" for both eyes. 
    * dropunknown unclear codings are converted to NA by default (with warning)
    * removed numcode argument (other numeric codes can be passed via "to")
* `reveal():`
    fix issue #13 (reveal: unused group levels are dropped)
* `eyes()`:
    * fix issue #16 (eyes: accepting both quoted and unquoted arguments)
    * add stop() when main argument not a data frame
    * fix issue #17 `eyes()` now also deals with "both eye cases"
    * fix issue #18 (now also finding eye columns with funny names, e.g. EyeName)
    * fix issue #19 (now returns meaningful result on empty data frames)
    * removed "report" argument 
* `eyestr()`:
    * moved eyes_to_str step from eyes() to eyestr()
* `age()` 
    rename to getage() in order to avoid potential and even likely clashes with the
    users environment, especially when using age() within the local environment 
    of a data frame (e.g., using with())
* `blink()` 
    message that this function will no longer be 
    maintained, but kept in the package. 
* `isNAstring` now also converts "-" string to NA
* new exported function: `tidyNA()` cleans NA entries
* `clean_va` returns numeric vector if all entries are numeric
* removed functions va_dissect() and which_va_dissect()

### Data sets
* Removed data set amd - this will be now part of the dedicated ophthalmic data set package "eyedata"
* removed data set 'va_quali' which was anyways a part of "va_chart"

# eye 0.1.0
the eye package is online!
eye is a tool to facilitate common tasks in ophthalmic research.

Its visual acuity conversion function `va()` provides eye researchRs with a long coveted tool to handle different visual acuity notations. It works with Snellen, logMAR and ETDRS.

`recodeye` allows an easy recoding of the eye variable.

It also has some other functions which are mainly intelligent wrapper around already existing functionalities, such as `myop` and `hyperop` for reshaping eye specific data, `reveal` or `blink`.

See your data with a new `eye`.
