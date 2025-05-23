% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/statistics.R
\docType{methods}
\name{median}
\alias{median}
\alias{median,CalibratedAges-method}
\alias{median,CalibratedAges,missing-method}
\title{Median}
\usage{
\S4method{median}{CalibratedAges}(x, na.rm = FALSE, ..., calendar = get_calendar())
}
\arguments{
\item{x}{A \code{\linkS4class{CalibratedAges}} object.}

\item{na.rm}{A \code{\link{logical}} scalar: should \code{NA} values be stripped before the
computation proceeds?}

\item{...}{Currently not used.}

\item{calendar}{An \code{\link[aion:TimeScale-class]{aion::TimeScale}} object specifying the target
calendar (see \code{\link[aion:calendar]{aion::calendar()}}). If \code{NULL}, \emph{rata die} are returned.}
}
\value{
A \code{\link{numeric}} vector.
}
\description{
Median
}
\examples{
## Calibrate multiple dates
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## Statistics
quantile(cal)
median(cal)
mean(cal)

## Plot
plot(cal, calendar = CE())

## Need to set 'calendar'
abline(v = median(cal, calendar = CE()), lty = 2, col = "blue")
abline(v = mean(cal, calendar = CE()), lty = 2, col = "red")
}
\seealso{
Other statistics: 
\code{\link{interval_credible}()},
\code{\link{interval_hdr}()},
\code{\link{mean}()},
\code{\link{quantile}()}
}
\author{
N. Frerebeau
}
\concept{statistics}
