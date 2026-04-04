\name{executeMWCAProgram}
\alias{executeMWCAProgram}

\title{Execute an MWCAProgram (Experimental)}

\description{
Compiles and runs an \code{\link{MWCAProgram}}, including optional
one-step factor refinements. For programs without refinements, this is
equivalent to \code{\link{compileMWCAProgram}} followed by
\code{\link{CoupledMWCA}} (or \code{\link{MWCA}}).

Refinements are applied after the main decomposition completes,
using \code{\link{refineFactor}}.
}

\usage{
executeMWCAProgram(program, Xs, ...)
}

\arguments{
  \item{program}{An \code{MWCAProgram} object.}
  \item{Xs}{Named list of input arrays.}
  \item{...}{Additional parameters passed to \code{compileMWCAProgram}.}
}

\value{
A list with components:
\describe{
  \item{fit}{The \code{MWCAResult} or \code{CoupledMWCAResult}.}
  \item{refinements}{Named list of \code{RefinedFactor} objects
    (empty list if no refinements).}
}
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link{MWCAProgram}}, \code{\link{compileMWCAProgram}},
\code{\link{refineFactor}}.
}

\keyword{methods}
