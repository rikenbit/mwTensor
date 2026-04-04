\name{checkCoupledMWCA}
\docType{methods}
\alias{checkCoupledMWCA}
\alias{checkCoupledMWCA,CoupledMWCAParams-method}

\title{
Static Validation of CoupledMWCA Parameters
}

\description{
Validates a \code{CoupledMWCAParams} object without running optimization.
Collects all errors and warnings instead of stopping on the first failure.
This is useful for pre-validating candidate decomposition problems
before committing to a potentially expensive \code{CoupledMWCA} run.
}

\usage{
checkCoupledMWCA(params)
}

\arguments{
\item{params}{A \code{CoupledMWCAParams} object.}
}

\value{
A list with the following components:
\describe{
  \item{ok}{Logical. \code{TRUE} if no errors were found.}
  \item{errors}{Character vector of error messages (empty if \code{ok} is \code{TRUE}).}
  \item{warnings}{Character vector of warning messages.}
  \item{normalized_params}{The input \code{params} if valid, or \code{NULL} if errors exist.}
  \item{summary}{A single human-readable summary string.}
}
}

\references{
Andrzej Cichocki et al., (2016). Tensor Networks for Dimensionality Reduction and Large-scale Optimization: Part 1 Low-Rank Tensor Decompositions

Andrzej Cichocki et al., (2015). Tensor Decompositions for Signal Processing Applications, \emph{IEEE SIGNAL PROCESSING MAGAZINE}
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link{CoupledMWCA}}, \code{\link{defaultCoupledMWCAParams}}, \code{\link{CoupledMWCAParams-class}}.
}

\examples{
  if(interactive()){
    # Test data
    Xs <- toyModel("coupled_CP_Easy")
    common_model <- list(
        X1=list(I1="A1", I2="A2"),
        X2=list(I2="A2", I3="A3", I4="A4"),
        X3=list(I4="A4", I5="A5"))
    params <- defaultCoupledMWCAParams(Xs, common_model)
    # Validate without running optimization
    result <- checkCoupledMWCA(params)
    result$ok       # TRUE
    result$errors   # character(0)
    result$summary  # "Validation passed"
  }
}
\keyword{methods}
