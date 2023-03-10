% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/radiocarbon.R
\docType{methods}
\name{c14_calibrate}
\alias{c14_calibrate}
\alias{c14_calibrate-method}
\alias{c14_calibrate,numeric,numeric-method}
\title{14C Calibration}
\usage{
c14_calibrate(ages, errors, ...)

\S4method{c14_calibrate}{numeric,numeric}(
  ages,
  errors,
  names = NULL,
  curves = "intcal20",
  reservoir_offsets = 0,
  reservoir_errors = 0,
  from = 55000,
  to = 0,
  resolution = 1,
  normalize = TRUE,
  F14C = FALSE,
  drop = TRUE,
  eps = 1e-05
)
}
\arguments{
\item{ages}{A \code{\link{numeric}} vector giving the BP ages to be calibrated.}

\item{errors}{A \code{\link{numeric}} vector giving the standard deviation of the ages
to be calibrated.}

\item{...}{Currently not used.}

\item{names}{A \code{\link{character}} vector specifying the names of the ages (e.g.
laboratory codes).}

\item{curves}{A \code{\link{character}} vector specifying the calibration curve to be
used. Different curves can be specified per dated sample.}

\item{reservoir_offsets}{A \code{\link{numeric}} vector giving the offset values for
any marine reservoir effect (defaults to 0; i.e. no offset).}

\item{reservoir_errors}{A \code{\link{numeric}} vector giving the offset value errors
for any marine reservoir effect (defaults to 0; i.e. no offset).}

\item{from}{length-one \code{\link{numeric}} vector specifying the earliest data to
calibrate for, in cal. BP years.}

\item{to}{A length-one \code{\link{numeric}} vector specifying the latest data to
calibrate for, in cal. BP years.}

\item{resolution}{A length-one \code{\link{numeric}} vector specifying the temporal
resolution (in years) of the calibration.}

\item{normalize}{A \code{\link{logical}} scalar: should the calibration be normalized?}

\item{F14C}{A \code{\link{logical}} scalar: should the calibration be carried out in
F14C space?}

\item{drop}{A \code{\link{logical}} scalar: should years with zero probability be
discarded? If \code{TRUE} (the default), results in a narrower time range.}

\item{eps}{A length-one \code{\link{numeric}} value giving the cutoff below which
calibration values will be removed.}
}
\value{
A \code{\linkS4class{CalibratedAges}} object.
}
\description{
Calibrates radiocarbon dates.
}
\note{
Adapted from \code{\link[rcarbon:calibrate]{rcarbon::calibrate()}}.
}
\examples{
## Calibrate a single date
cal <- c14_calibrate(130, 20)
plot(cal)

## Calibrate multiple dates
BP <- c14_calibrate(
  ages = c(5000, 4500),
  errors = c(45, 35),
  names = c("X", "Y")
)
plot(BP, panel.first = graphics::grid())

## Convert BP scale to CE
CE <- BP_to_CE(BP)
plot(CE)

## HPDI
hpdi(BP, level = 0.954)
hpdi(CE, level = 0.954)
}
\references{
Bronk Ramsey, C. (2008). RADIOCARBON DATING: REVOLUTIONS IN UNDERSTANDING.
\emph{Archaeometry}, 50:249-275. \doi{10.1111/j.1475-4754.2008.00394.x}.
}
\seealso{
Other radiocarbon tools: 
\code{\link{F14C}},
\code{\link{c14_combine}()},
\code{\link{c14_curve}()},
\code{\link{c14_plot}}
}
\author{
N. Frerebeau
}
\concept{radiocarbon tools}
