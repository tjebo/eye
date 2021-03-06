% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/va.R
\name{va}
\alias{va}
\title{Visual acuity notation conversion}
\usage{
va(x, from = NULL, to = NULL, type = "ft", smallstep = FALSE, noplus = FALSE)
}
\arguments{
\item{x}{Vector with visual acuity entries. Must be atomic.
Snellen fractions need to be entered with "/"}

\item{from}{will force to evaluate from which notation to convert - Must be
"etdrs", "logmar", "snellen" or "snellendec".
Ignored if the value should not be plausible.}

\item{to}{To which class to convert. "etdrs", "logmar" or "snellen" -
any case allowed. If NULL (default), will simply "clean up" VA entries.
This may then result in a vector of "mixed" VA notations.}

\item{type}{To which Snellen notation to convert: "m", "dec" or "ft"}

\item{smallstep}{how +/- entries are evaluated. FALSE:
increase/decrease Snellen fractions by lines. TRUE: plus/minus
entries equivalent to 0.02 logmar}

\item{noplus}{ignoring plus/minus entries and just returning the
snellen fraction. This overrides the smallstep argument.}
}
\value{
vector of \code{va} class. See also "VA classes"
}
\description{
Cleans and converts visual acuity notations (classes)
between Snellen (decimal, meter and feet), ETDRS, and logMAR.
}
\section{VA conversion}{

\itemize{
\item \strong{logMAR to ETDRS}: logMAR rounded to the first digit and converted with
the visual acuity chart (see section VA chart)
\item \strong{Snellen to logMAR}: logMAR = -1 * log10(snellen_frac)
\item \strong{Snellen to ETDRS}: ETDRS = 85 + 50 * log10(snellen_frac)
\doi{10.1097/iae.0b013e3181d87e04}
\item \strong{ETDRS to logMAR}: logMAR = -0.02 * etdrs + 1.7
Beck et al. \doi{10.1016/s0002-9394(02)01825-1}
\item \strong{Hand movements and counting fingers} are converted following
Schulze-Bonsel et al. - https://doi.org/10.1167/iovs.05-0981
\item \strong{(No) light perception} are converted following the suggestions by
\href{https://michaelbach.de/sci/acuity.html}{Michael Bach}
}
}

\section{Qualitative visual acuity entries}{

In order to calculate with qualitative entries counting fingers,
hand movement and (no) perception of light, \strong{use logMAR} !
Qualitative visual acuity lower than counting fingers is assigned 0
ETDRS letter, in order to keep it as a measurement (not: NA). It is very
difficult to justify a "negative" letter score in a test which only has
a specific range (0-100).
\itemize{
\item \strong{To Snellen}:
Although there seems to be no good statistical reason to convert
back to Snellen, it is a very natural thing to eye specialists to think
in Snellen. A conversion to snellen gives a good gauge of how the visual
acuity for the patients are. However, back-conversion should not be
considered an exact science and any attempt to use formulas will result
in very weird Snellen values that have no correspondence to common charts.
Therefore, Snellen matching the nearest ETDRS and logMAR value in
the VA chart are used.
}
}

\section{VA chart}{

You can find with eye:::va_chart.
This chart and VA conversion formulas are based on charts in
Holladay et al.\doi{10.1016/j.jcrs.2004.01.014}, Beck et al.
\doi{10.1016/s0002-9394(02)01825-1}{Beck et al.}, and
Gregori et al.\doi{10.1097/iae.0b013e3181d87e04}.
The etdrs values for NLP and PL are deliberately set at those values because
they are unlikely to happen by chance as a wrong entry (and as integers),
and it has internal reasons that make conversion easier.
}

\section{Accepted VA formats / Plausibility checks}{

\itemize{
\item Snellen fractions (meter/ feet) need to be entered as fraction with
"/". Any fractions allowed. You can get creative with your snellens.
see \strong{"Examples"}
\item ETDRS must be integer-equivalent between 0 and 100 (integer equivalent
means, it can also be a character vector)
\item logMAR must be -0.3 <= x <= 3.0
\item Snellen decimal must be 0 < x <= 2
\item Qualitative must be either of PL, LP, NLP, NPL, HM, CF (any case allowed)
\item Plausibility checks are performed for the automatically or manually defined
notation.
\item Any element which is implausible/ not recognized will be converted to NA
}
}

\section{Entries with mixed VA notations}{

Use \link{va_mixed} instead.
}

\section{Snellen "+/-" entries}{

By default, plus/minus entries are evaluated as intended by the
test design: Snellen fractions increase/decrease only by lines.\preformatted{- if entry -2 to +2 : take same Snellen value
- if < -2 : take Snellen value one line below
- if > +2 : take Snellen value one line above
}

If smallstep = TRUE, each snellen optotype will be considered
equivalent to 0.02 logmar (assuming 5 letters in a row in a chart)
}

\section{VA cleaning}{

For more details see \code{\link[=clean_va]{clean_va()}}
\enumerate{
\item \code{NA} is assigned to strings such as "." or "", "n/a" or "   "
\item notation for qualitative entries is simplified.
}
}

\section{VA classes}{

convertVA returns a vector of three classes:
\enumerate{
\item \code{va}
\item One of snellen, snellendec, logmar, etdrs or quali.
\item Either of \code{character} (for Snellen, snellendec, and qualitative),
\code{numeric} (for logMAR), or \code{integer} (for ETDRS).
}
}

\examples{
## will automatically detect VA class and convert to logMAR by default
## ETDRS letters
x <- c(23, 56, 74, 58)
va(x)

## ... or convert to snellen
va(x, to = "snellen")

## snellen, mixed with categories. Also dealing with those "plus/minus" entries
va(c("NLP", "NPL", "PL", "LP", "HM", "CF", "6/60", "20/200", "6/9",
 "20/40", "20/40+3", "20/50-2"))

## A mix of notations is also possible
x <- c("NLP", "0.8", "34", "3/60", "2/200", "20/40+3", "20/50-2")
va(x)

## Any fraction is possible, and empty values
x <- c("CF", "3/60", "2/200", "", "20/40+3", ".", "      ")
va(x)

## but this not any fraction when converting from one class to the other
x <- c("3/60", "2/200", "6/60", "20/200", "6/9")
va(x, to="snellen", type = "m")
}
\seealso{
Other Ophthalmic functions: 
\code{\link{va_mixed}()}

Other VA converter: 
\code{\link{VAwrapper}},
\code{\link{plausibility_methods}},
\code{\link{snellen_steps}},
\code{\link{va_methods}},
\code{\link{va_mixed}()},
\code{\link{which_va}()}

Other VA cleaner: 
\code{\link{clean_va}()}
}
\concept{Ophthalmic functions}
\concept{VA cleaner}
\concept{VA converter}
