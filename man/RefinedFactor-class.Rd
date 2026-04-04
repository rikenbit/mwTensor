\name{RefinedFactor-class}
\docType{class}
\alias{RefinedFactor-class}

\title{Class \code{"RefinedFactor"}}

\description{
Container for one-step factor refinement results produced by
\code{\link{refineFactor}}.

Given a factor matrix \code{A} (k x n), the refinement decomposes
\code{t(A)} (n x k) into \code{U * V} where \code{U} is n x dim
and \code{V} is dim x k. The slots store:
\itemize{
  \item \code{sub_factors = t(U)}: the refined sub-basis (dim x n)
  \item \code{coef = t(V)}: coefficients (k x dim) such that
    \code{A} is approximately \code{coef \%*\% sub_factors}
}
}

\section{Slots}{
\describe{
  \item{source_object}{The original \code{MWCAResult} or \code{CoupledMWCAResult}.}
  \item{source_factor_name}{Character. Name or index of the source factor.}
  \item{source_factor}{Matrix. The original factor (k x n).}
  \item{algorithm}{Character. Algorithm used for refinement.}
  \item{dim}{Integer. Target dimension.}
  \item{sub_factors}{Matrix. Refined sub-basis (dim x n).}
  \item{coef}{Matrix. Coefficient matrix (k x dim).}
}
}

\seealso{
\code{\link{refineFactor}}.
}

\keyword{classes}
