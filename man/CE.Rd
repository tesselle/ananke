% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/calendar.R
\docType{methods}
\name{CE}
\alias{CE}
\alias{CE_to_BP}
\alias{CE_to_BP-method}
\alias{CE_to_b2k}
\alias{CE_to_b2k-method}
\alias{is_CE}
\alias{is_CE-method}
\alias{is_CE,CalibratedAges-method}
\alias{CE_to_BP,numeric-method}
\alias{CE_to_BP,matrix-method}
\alias{CE_to_BP,array-method}
\alias{CE_to_b2k,numeric-method}
\alias{CE_to_b2k,matrix-method}
\alias{CE_to_b2k,array-method}
\title{Common Era}
\usage{
CE_to_BP(object)

CE_to_b2k(object)

is_CE(object)

\S4method{is_CE}{CalibratedAges}(object)

\S4method{CE_to_BP}{numeric}(object)

\S4method{CE_to_BP}{matrix}(object)

\S4method{CE_to_BP}{array}(object)

\S4method{CE_to_b2k}{numeric}(object)

\S4method{CE_to_b2k}{matrix}(object)

\S4method{CE_to_b2k}{array}(object)
}
\arguments{
\item{object}{An object.}
}
\value{
\itemize{
\item \code{CE_to_BP()} and \code{CE_to_b2k()} return an object of the same sort as
\code{object} with a new time scale.
\item \code{is_CE()} returns a \code{\link{logical}} scalar.
}
}
\description{
Converts between CE (Common Era) and BP (Before Present) or b2k (before 2000)
time scales.
}
\note{
There is no year \eqn{0} in BCE/CE scale.
}
\examples{
## BP to CE
BP_to_CE(0)
BP_to_CE(1950)

## BP to b2k
BP_to_b2k(0)
BP_to_b2k(1950)

## CE to BP
CE_to_BP(1950)
CE_to_BP(-1)

## CE to b2k
CE_to_b2k(2000)
CE_to_b2k(-1)

## b2k to BP
b2k_to_BP(0)
b2k_to_BP(50)

## b2k to CE
b2k_to_CE(0)
b2k_to_CE(2000)
}
\seealso{
Other time scales: 
\code{\link{BP}},
\code{\link{b2k}}
}
\author{
N. Frerebeau
}
\concept{time scales}
