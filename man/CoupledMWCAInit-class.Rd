\name{CoupledMWCAInit-class}
\docType{class}
\alias{CoupledMWCAInit-class}

\title{Class \code{"CoupledMWCAInit"}}

\description{
Container for CoupledMWCA initialization results produced by
\code{\link{initCoupledMWCA}}.
}

\section{Slots}{
\describe{
  \item{params}{The \code{CoupledMWCAParams} object used for initialization.}
  \item{common_factors}{Named list of common factor matrices.}
  \item{common_cores}{List of common core tensors.}
  \item{specific_factors}{Named list of specific factor matrices.}
  \item{specific_cores}{List of specific core tensors.}
  \item{init_policy}{Character string indicating the initialization policy used.}
  \item{seed}{The random seed used, or \code{NULL}.}
}
}

\seealso{
\code{\link{initCoupledMWCA}}, \code{\link{CoupledMWCAParams-class}}.
}

\keyword{classes}
