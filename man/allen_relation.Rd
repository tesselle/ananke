% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AllGenerics.R, R/allen.R
\docType{methods}
\name{allen_relation}
\alias{allen_relation}
\alias{allen_relation-method}
\alias{allen_relation,numeric,numeric-method}
\alias{allen_relation,ANY,missing-method}
\title{Allen Relation Between Definite Intervals}
\usage{
allen_relation(x, y, ...)

\S4method{allen_relation}{numeric,numeric}(x, y)

\S4method{allen_relation}{ANY,missing}(x)
}
\arguments{
\item{x, y}{A \code{\link{numeric}} vector giving the lower and upper boundaries of the
time intervals, respectively. If \code{y} is missing, an attempt is made to
interpret \code{x} in a suitable way (see \code{\link[grDevices:xy.coords]{grDevices::xy.coords()}}).}

\item{...}{Currently not used.}
}
\value{
A \code{\link{character}} matrix specifying the Allen relations.
}
\description{
Allen Relation Between Definite Intervals
}
\details{
\tabular{lrlr}{
\strong{Relation} \tab     \tab     \tab  \strong{Converse} \cr
precedes     \tab (p) \tab (P) \tab   preceded by \cr
meets        \tab (m) \tab (M) \tab        met by \cr
overlaps     \tab (o) \tab (O) \tab overlapped by \cr
finished by  \tab (F) \tab (f) \tab      finishes \cr
contains     \tab (D) \tab (d) \tab        during \cr
starts       \tab (s) \tab (S) \tab    started by \cr
equals       \tab (e) \tab     \tab               \cr
}
}
\examples{
## Data from Husi 2022
data("loire", package = "folio")
loire <- subset(loire, area == "Anjou")

## Basic relations
allen_relation(loire$lower, loire$upper)

## Complement
(comp <- allen_complement("F")) # "pmoDseSdfOMP"

## Converse
(conv <- allen_converse(comp)) # "pmoFDseSdOMP"

## Composition
allen_composition("oFD", "oFDseS") # "pmoFD"

## Intersection
allen_intersect("pFsSf", "pmoFD") # "pF"

# Union
allen_union("pFsSf", "pmoFD") # "pmoFDsSf"
}
\references{
Allen, J. F. (1983). Maintaining Knowledge about Temporal Intervals.
\emph{Communications of the ACM}, 26(11): 832-843. \doi{10.1145/182.358434}.

Alspaugh, T. (2019). Allen's Interval Algebra.
URL: \url{https://thomasalspaugh.org/pub/fnd/allen.html}.
}
\seealso{
Other Allen's intervals: 
\code{\link{allen_complement}()},
\code{\link{allen_composition}()},
\code{\link{allen_converse}()},
\code{\link{allen_intersect}()},
\code{\link{allen_union}()}
}
\author{
N. Frerebeau
}
\concept{Allen's intervals}
