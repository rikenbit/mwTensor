\name{compileMWCAProgram}
\alias{compileMWCAProgram}

\title{Compile an MWCAProgram to Solver Parameters}

\description{
Converts a validated, non-recursive \code{MWCAProgram} into
\code{MWCAParams} (single block) or \code{CoupledMWCAParams}
(multiple blocks). Programs with refinements cannot currently be
compiled.
}

\usage{
compileMWCAProgram(program, Xs, ...)
}

\arguments{
  \item{program}{An \code{MWCAProgram} object.}
  \item{Xs}{Named list of input arrays, with names matching the program's block names.}
  \item{...}{Additional parameters: \code{thr}, \code{verbose},
    \code{common_coretype}, \code{specific_coretype}, etc.}
}

\value{
An \code{MWCAParams} or \code{CoupledMWCAParams} object.
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link{MWCAProgram}}, \code{\link{validateMWCAProgram}},
\code{\link{MWCA}}, \code{\link{CoupledMWCA}}.
}

\keyword{methods}
