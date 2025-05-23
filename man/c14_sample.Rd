% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/c14_sample.R
\docType{methods}
\name{c14_sample}
\alias{c14_sample}
\alias{c14_sample-method}
\alias{c14_sample,CalibratedAges-method}
\title{Sample Calibrated Ages}
\usage{
c14_sample(object, ...)

\S4method{c14_sample}{CalibratedAges}(object, n = 100, calendar = get_calendar())
}
\arguments{
\item{object}{A \code{\linkS4class{CalibratedAges}} object.}

\item{...}{Currently not used.}

\item{n}{An \code{\link{integer}} specifying the number of random samples.}

\item{calendar}{An \code{\link[aion:TimeScale-class]{aion::TimeScale}} object specifying the target
calendar (see \code{\link[aion:calendar]{aion::calendar()}}). Defaults to \code{\link[aion:gregorian]{aion::CE()}}. If \code{NULL},
\emph{rata die} are returned.}
}
\value{
An \code{numeric} matrix.
}
\description{
Sample Calibrated Ages
}
\examples{
## Calibrate multiple dates
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## Sample
spl <- c14_sample(cal, n = 100)
}
\seealso{
Other radiocarbon tools: 
\code{\link{F14C}},
\code{\link{c14_calibrate}()},
\code{\link{c14_combine}()},
\code{\link{c14_curve}()},
\code{\link{c14_ensemble}()},
\code{\link{c14_plot}},
\code{\link{c14_spd}()},
\code{\link{c14_uncalibrate}()},
\code{\link{rec_plot}}
}
\author{
N. Frerebeau
}
\concept{radiocarbon tools}
