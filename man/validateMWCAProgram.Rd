\name{validateMWCAProgram}
\alias{validateMWCAProgram}

\title{Validate an MWCAProgram}

\description{
Checks structural consistency of a factorization program without
running any computation. Validates factor references, mode mappings,
and refinement depth constraints.
}

\usage{
validateMWCAProgram(program)
}

\arguments{
  \item{program}{An \code{MWCAProgram} object.}
}

\value{
A list with components:
\describe{
  \item{ok}{Logical. \code{TRUE} if no errors.}
  \item{errors}{Character vector of error messages.}
  \item{warnings}{Character vector of warning messages.}
  \item{summary}{Human-readable summary string.}
}
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link{MWCAProgram}}, \code{\link{compileMWCAProgram}}.
}

\keyword{methods}
