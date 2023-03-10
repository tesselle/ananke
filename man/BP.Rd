% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/calendar.R
\docType{methods}
\name{BP}
\alias{BP}
\alias{BP_to_CE}
\alias{BP_to_CE-method}
\alias{BP_to_b2k}
\alias{BP_to_b2k-method}
\alias{is_BP}
\alias{is_BP-method}
\alias{is_BP,CalibratedAges-method}
\alias{BP_to_CE,numeric-method}
\alias{BP_to_CE,matrix-method}
\alias{BP_to_CE,array-method}
\alias{BP_to_CE,CalibratedAges-method}
\alias{BP_to_b2k,numeric-method}
\alias{BP_to_b2k,matrix-method}
\alias{BP_to_b2k,array-method}
\title{Before Present}
\usage{
BP_to_CE(object)

BP_to_b2k(object)

is_BP(object)

\S4method{is_BP}{CalibratedAges}(object)

\S4method{BP_to_CE}{numeric}(object)

\S4method{BP_to_CE}{matrix}(object)

\S4method{BP_to_CE}{array}(object)

\S4method{BP_to_CE}{CalibratedAges}(object)

\S4method{BP_to_b2k}{numeric}(object)

\S4method{BP_to_b2k}{matrix}(object)

\S4method{BP_to_b2k}{array}(object)
}
\arguments{
\item{object}{An object.}
}
\value{
\itemize{
\item \code{BP_to_CE()} and \code{BP_to_b2k()} return an object of the same sort as
\code{object} with a new time scale.
\item \code{is_BP()} returns a \code{\link{logical}} scalar.
}
}
\description{
Converts between BP (Before Present) and CE (Common Era) or b2k (before 2000)
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
\code{\link{CE}},
\code{\link{b2k}}
}
\author{
N. Frerebeau
}
\concept{time scales}
