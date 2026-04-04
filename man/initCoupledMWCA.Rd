\name{initCoupledMWCA}
\docType{methods}
\alias{initCoupledMWCA}
\alias{initCoupledMWCA,CoupledMWCAParams-method}

\title{
Initialize CoupledMWCA Factors and Cores
}

\description{
Generates normalized, reproducible initial values for a CoupledMWCA
problem without running iterative optimization. This is a companion to
\code{\link{checkCoupledMWCA}} and can be used to inspect or store
initial states before calling \code{\link{CoupledMWCA}}.
}

\usage{
initCoupledMWCA(params, seed = NULL, init_policy = "random")
}

\arguments{
\item{params}{A \code{CoupledMWCAParams} object.}
\item{seed}{An optional integer seed for reproducibility. When \code{NULL}
  (default), no seed management is performed.}
\item{init_policy}{Character string specifying the initialization strategy.
  One of \code{"random"} (default), \code{"svd"}, or \code{"nonneg_random"}.}
}

\value{
A \code{\linkS4class{CoupledMWCAInit}} object with the following slots:
\describe{
  \item{params}{The input \code{CoupledMWCAParams}.}
  \item{common_factors}{Named list of initialized common factor matrices.}
  \item{common_cores}{List of initialized common core tensors.}
  \item{specific_factors}{Named list of initialized specific factor matrices
    (or \code{list(NULL)} when \code{specific=FALSE}).}
  \item{specific_cores}{List of initialized specific core tensors
    (or \code{list(NULL)} when \code{specific=FALSE}).}
  \item{init_policy}{The policy used.}
  \item{seed}{The seed used (or \code{NULL}).}
}
}

\references{
Andrzej Cichocki et al., (2016). Tensor Networks for Dimensionality Reduction and Large-scale Optimization: Part 1 Low-Rank Tensor Decompositions

Andrzej Cichocki et al., (2015). Tensor Decompositions for Signal Processing Applications, \emph{IEEE SIGNAL PROCESSING MAGAZINE}
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link{checkCoupledMWCA}}, \code{\link{CoupledMWCA}},
\code{\link{defaultCoupledMWCAParams}}.
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
    # Initialize with seed for reproducibility
    init <- initCoupledMWCA(params, seed=42L)
    # Inspect shapes
    lapply(init@common_factors, dim)
    # SVD-based initialization
    init_svd <- initCoupledMWCA(params, seed=42L, init_policy="svd")
  }
}
\keyword{methods}
