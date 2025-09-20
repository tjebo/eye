## eye 1.3.0
- new functionality for qualitative visual acuity entries: 
1) More values recognised as qualitative entries, and one can define values that are being recognized using `set_eye_strings`.(#36) 
2) You can now set custom values for qualitative entries (hand movements, count fingers etc) using the "quali_values" argument in `va`
- `recodeye` and `eyes` now also recognize "BE" as coding string for "both eyes"
- updated package documentation file  
- updated hyperlinks 
- fixed issues #43, #48, #50
- removed clean_va and alias cleanVA from exported namespace to keep it simpler
- Improved warning when removing character column in `reveal()`
- moved `revealEye` generic to internal functions
- `va_mixed`: added default to `possible` argument, thus removing snellen decimals from recognised notations by default. 

## eye 1.2.1
Bug fixes: 
- if no eye column is found, `eyes` also returns a list #31
- `set_eye_strings` also updates eye column #32 
- partial eye strings recognized in column names #33
- `to_etdrs` now correctly always returns integer class vector #34
- conversion of pure "NA" saved as character now correctly returns `NA` #35
- logMAR will return rounded values to the first digit when `noplus = TRUE` #37
- `to_etdrs` from pure qualitative values correctly returns 0 for NPL and PL #38

## eye 1.2.0
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
Simplifications:
- simplified pivoting in myop using the pivot_longer regex feature
- removed ..._chr arguments from getElem_... functions (redundant)
- getElem... now only for vectors (because they were anyways only used for vectors)

## eye 1.1.0  
summary of changes: 
- new features for function `eyestr`
- important bug fixes

Details: 
- eyestr: new "english" argument for more flexibility (#26)
- eyestr removed "para" argument 
- eyestr prints one eye correctly in singular (#25)
- eyestr added "caps" argument for more flexibility of spelling
- eyestr removed "UK" argument (there is no difference in spelling for numbers :)
- eyes fixed missing reference to column (#27)
- added internal function `tidyNA_low` to avoid double tolower call
- recodeye fixed fail when trailing white space (#28)
- which_va fixed conversion error when mixed ETDRS and logmar (#29)

## eye 1.0.1
- Nasty bug fix! Erroneous conversion of quality visual acuity entries when ETDRS (#24)
- removed deprecated function `age()`
- VA chart now as internal data, not exported - ideally the VA chart should not
be necessary anyways. However, if you really need to see it, you can still access it with `eye:::va_chart`. Note some of the ETDRS values were chosen for easier conversion. 
- internally used S3 methods not exported to namespace any longer:
  - convertVA 
  - checkVA
  
## eye 1.0.0
Major version upgrade of eye 0.1.0!

### Functions and objects (details)
#### Visual acuity handling
* introduced new verbs `as_...` and `to_...` for class conversion for nice integration into known R grammar (#6) - simple wrapper for `va(x, to = ...)`
* add arguments "from", "noplus" and "smallstep"
* removed "from_logmar" and "logmarstep"
* VA notation guessing is not done by single element any more, but will be forced choice either by most likely or with argument "from". Any implausible values will be forced to NA.
* weird snellen values (e.g., 20/41) are now also converted to other snellen values (#21). 
* New S3 methods introduced to check plausibility of entries by VA class. 
* Adding plus/minus entries is now done by actual logmar values rather than via lookup in the VA chart (rewrite of `snellen_steps` function)
* new function `va_mixed` for vectors of mixed VA notations (replacing previous `va_dissect`)
* new function `cleanVA`

#### Smaller fixes
* `recodeye()` recognises "both/ou/b" for both eyes. 
* dropunknown unclear codings are converted to NA by default (with warning)
* removed numcode argument (other numeric codes can be passed via "to")
* `eyes()` accepting both quoted and unquoted arguments (#16)
* add stop() when main argument not a data frame
* `eyes()` now also deals with "both eye cases" (#17)
* now also finding eye columns with funny names, e.g. EyeName (#18) 
* eyes now returns meaningful result on empty data frames) (#19)
* removed "report" argument 
* `reveal()` does not drop unused group levels (#13)
* `eyestr()` moved eyes_to_str step from eyes() to eyestr()
* `age()` renamed to getage() in order to avoid potential and even likely clashes with the users environment, especially when using age() within the local environment of a data frame (e.g., using with())
* `blink()` is deprecated. message that this function will no longer be maintained, but kept in the package. 
* `isNAstring` now also converts "-" string to NA
* new exported function: `tidyNA()` cleans NA entries
* `clean_va` returns numeric vector if all entries are numeric
* removed functions va_dissect() and which_va_dissect()

#### Data sets
* Removed data set amd - this will be now part of the dedicated ophthalmic data set package "eyedata"
* removed data set 'va_quali' which was anyways a part of "va_chart"

## eye 0.1.0
the eye package is online!
eye is a tool to facilitate common tasks in ophthalmic research.

Its visual acuity conversion function `va()` provides eye researchRs with a long coveted tool to handle different visual acuity notations. It works with Snellen, logMAR and ETDRS.

`recodeye` allows an easy recoding of the eye variable.

It also has some other functions which are mainly intelligent wrapper around already existing functionalities, such as `myop` and `hyperop` for reshaping eye specific data, `reveal` or `blink`.

See your data with a new `eye`.
