% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/c14_f14c.R
\docType{methods}
\name{F14C}
\alias{F14C}
\alias{BP14C_to_F14C}
\alias{BP14C_to_F14C-method}
\alias{F14C_to_BP14C}
\alias{F14C_to_BP14C-method}
\alias{BP14C_to_F14C,numeric,numeric-method}
\alias{F14C_to_BP14C,numeric,numeric-method}
\title{F14C}
\usage{
BP14C_to_F14C(ages, errors, ...)

F14C_to_BP14C(values, errors, ...)

\S4method{BP14C_to_F14C}{numeric,numeric}(ages, errors, lambda = 8033)

\S4method{F14C_to_BP14C}{numeric,numeric}(values, errors, lambda = 8033, asymmetric = FALSE)
}
\arguments{
\item{ages}{A \code{\link{numeric}} vector giving the radiocarbon ages.}

\item{errors}{A \code{\link{numeric}} vector giving the standard deviations.}

\item{...}{Currently not used.}

\item{values}{A \code{\link{numeric}} vector giving the F14C values.}

\item{lambda}{A length-one \code{\link{numeric}} vector specifying the mean-life of
radiocarbon (defaults to 14C half-life value as introduced by Libby 1952).}

\item{asymmetric}{A \code{\link{logical}} scalar: should asymmetric 14C errors be
returned?}
}
\value{
A \code{\link{data.frame}}.
}
\description{
Converts F14C values to 14C ages.
}
\examples{
## Asymmetric 14C errors (van der Plicht and Hogg 2006)
F14C_to_BP14C(0.0052, 0.0006, asym = TRUE)

## Symmetric 14C errors (Bronk Ramsey 2008)
F14C_to_BP14C(0.0052, 0.0006, asym = FALSE)
}
\references{
Bronk Ramsey, C. (2008). Radiocarbon Dating: Revolutions in Understanding.
\emph{Archaeometry}, 50:249-275. \doi{10.1111/j.1475-4754.2008.00394.x}.

van der Plicht, J., Hogg, A. (2006). A Note on Reporting Radiocarbon.
\emph{Quaternary Geochronology}, 1(4): 237-240.
\doi{10.1016/j.quageo.2006.07.001}.
}
\seealso{
Other radiocarbon tools: 
\code{\link{c14_calibrate}()},
\code{\link{c14_combine}()},
\code{\link{c14_curve}()},
\code{\link{c14_ensemble}()},
\code{\link{c14_plot}},
\code{\link{c14_spd}()},
\code{\link{c14_uncalibrate}()},
\code{\link{c14_validate}()},
\code{\link{rec_plot}}
}
\author{
N. Frerebeau
}
\concept{radiocarbon tools}
