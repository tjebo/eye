% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/which_va.R
\name{which_va}
\alias{which_va}
\title{Guessing the VA class}
\usage{
which_va(x, quali = c("nlp", "npl", "pl", "lp", "hm", "cf"))
}
\arguments{
\item{x}{Vector with VA entries}

\item{quali}{strings for qualitative visual acuity entries}
}
\value{
character vector indicating guessed VA notation
}
\description{
Guessing the VA notation (VA class). Requires x that was
prepared with \link{clean_va}
\itemize{
\item \code{which_va}: guessing VA class for entire vector
}
}
\seealso{
Other VA converter: 
\code{\link{VAwrapper}},
\code{\link{plausibility_methods}},
\code{\link{snellen_steps}},
\code{\link{va_methods}},
\code{\link{va_mixed}()},
\code{\link{va}()}
}
\concept{VA converter}
\concept{VA helper}
\keyword{internal}
