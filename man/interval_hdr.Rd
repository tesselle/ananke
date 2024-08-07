% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/interval_hdr.R
\docType{methods}
\name{hdr}
\alias{hdr}
\alias{interval_hdr,CalibratedAges,missing-method}
\title{Highest Density Regions}
\usage{
\S4method{interval_hdr}{CalibratedAges,missing}(x, level = 0.954, calendar = getOption("ananke.calendar"), ...)
}
\arguments{
\item{x}{A \code{\linkS4class{CalibratedAges}} object.}

\item{level}{A length-one \code{\link{numeric}} vector giving the confidence level.}

\item{calendar}{An \code{\link[aion:TimeScale-class]{aion::TimeScale}} object specifying the target
calendar (see \code{\link[aion:calendar]{aion::calendar()}}). If \code{NULL}, \emph{rata die} are returned.}

\item{...}{Currently not used.}
}
\value{
Returns a \code{\link{list}} of \code{numeric} \code{\link{matrix}}.
}
\description{
Highest Density Regions
}
\examples{
## Calibrate multiple dates
cal <- c14_calibrate(
  values = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)

## HDR
interval_hdr(cal, level = 0.683)
interval_hdr(cal, level = 0.954)
interval_hdr(cal, level = 0.997)
}
\references{
Hyndman, R. J. (1996). Computing and graphing highest density regions.
\emph{American Statistician}, 50: 120-126. \doi{10.2307/2684423}.
}
\seealso{
\code{\link[stats:density]{stats::density()}}, \code{\link[arkhe:interval_hdr]{arkhe::interval_hdr()}}

Other statistics: 
\code{\link{mean}()},
\code{\link{median}()},
\code{\link{quantile}()}
}
\author{
N. Frerebeau
}
\concept{statistics}
