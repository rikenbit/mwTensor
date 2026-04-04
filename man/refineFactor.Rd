\name{refineFactor}
\alias{refineFactor}

\title{One-Step Factor Refinement (Experimental)}

\description{
Takes a factor matrix from a completed \code{\link{MWCA}} or
\code{\link{CoupledMWCA}} fit and applies a single additional matrix
factorization to decompose it further.

This is \strong{not} a full recursive decomposition engine.
Only one level of refinement is supported.
}

\details{
\strong{Mathematical interpretation:}

In MWCA/CoupledMWCA, each factor matrix \code{A} has shape k x n
where k is the lower dimension and n is the original observation
dimension. Each row of \code{A} is a basis vector in the
n-dimensional observation space.

\code{refineFactor} treats \code{t(A)} (n x k) as a new observed
matrix and decomposes it: \code{t(A) = U * V} where \code{U} is
n x dim and \code{V} is dim x k.

Equivalently, \code{A = t(V) * t(U)}, so the original k basis
vectors are approximated by dim sub-basis vectors.

The returned \code{sub_factors} slot holds \code{t(U)} (dim x n):
the new lower-rank basis vectors. The \code{coef} slot holds
\code{t(V)} (k x dim): the coefficients expressing the original
factor rows in terms of the sub-basis.

If the original decomposition was \code{X = S x_m A_m x ...},
then after refinement of A_m:
\code{X ~ (S x_m coef) x_m sub_factors x ...}.
Computing this updated core is left to the caller.
}

\usage{
refineFactor(fit, factor_name, algorithm = "mySVD", dim = 2L)
}

\arguments{
  \item{fit}{An \code{MWCAResult} or \code{CoupledMWCAResult} object.}
  \item{factor_name}{For \code{MWCAResult}: an integer index.
    For \code{CoupledMWCAResult}: a character factor name (e.g. \code{"A2"}).}
  \item{algorithm}{Character. Decomposition algorithm name.}
  \item{dim}{Integer. Target lower dimension for the refinement.}
}

\value{
A \code{\linkS4class{RefinedFactor}} object with slots:
\describe{
  \item{source_object}{The original fit object.}
  \item{source_factor_name}{Character label identifying the source factor.}
  \item{source_factor}{The original factor matrix A (k x n).}
  \item{algorithm}{Algorithm used.}
  \item{dim}{Target dimension used.}
  \item{sub_factors}{t(U): the refined sub-basis (dim x n).}
  \item{coef}{t(V): coefficient matrix (k x dim) such that
    A is approximately \code{coef \%*\% sub_factors}.}
}
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link{MWCA}}, \code{\link{CoupledMWCA}},
\code{\link{RefinedFactor-class}}.
}

\examples{
  if(interactive()){
    X <- matrix(runif(20*30), nrow=20, ncol=30)
    params <- defaultMWCAParams(X)
    params@dims <- c(5L, 5L)
    fit <- MWCA(params)
    ref <- refineFactor(fit, 1L, algorithm="mySVD", dim=2L)
    dim(ref@sub_factors) # 2 x 20
    dim(ref@coef)        # 5 x 2
  }
}

\keyword{methods}
