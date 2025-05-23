% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/interval.R
\docType{methods}
\name{interval_credible}
\alias{interval_credible}
\alias{interval_credible,CalibratedAges-method}
\alias{interval_credible,CalibratedAges,missing-method}
\title{Bayesian Credible Interval}
\usage{
\S4method{interval_credible}{CalibratedAges}(x, level = 0.954, n = 100, ...)
}
\arguments{
\item{x}{A \code{\linkS4class{CalibratedAges}} object.}

\item{level}{A length-one \code{\link{numeric}} vector giving the confidence level.}

\item{n}{An \code{\link{integer}} specifying the number of random samples.}

\item{...}{Currently not used.}
}
\value{
A \code{\linkS4class{CalibratedIntervals}} object.
}
\description{
Bayesian Credible Interval
}
\examples{
## Calibrate multiple dates
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## Credible intervals
crd68 <- interval_credible(cal, level = 0.683)
crd95 <- interval_credible(cal, level = 0.954)
crd99 <- interval_credible(cal, level = 0.997)

## Coerce to data.frame
as.data.frame(crd95, calendar = BC())

## Plot
plot(cal, interval = "credible")
}
\seealso{
\code{\link[arkhe:interval_credible]{arkhe::interval_credible()}}

Other statistics: 
\code{\link{interval_hdr}()},
\code{\link{mean}()},
\code{\link{median}()},
\code{\link{quantile}()}
}
\author{
N. Frerebeau
}
\concept{statistics}
