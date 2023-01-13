\name{defaultMWCAParams}
\docType{methods}
\alias{defaultMWCAParams}

\title{
Default parameters for MWCA
}

\description{
The input is assumed to be an array object.
}

\usage{
defaultMWCAParams(X)
}

\arguments{
\item{X}{An array object}
}

\value{
MWCAParams object.
}

\references{
Andrzej Cichocki et al., (2016). Tensor Networks for Dimensionality Reduction and Large-scale Optimization: Part 1 Low-Rank Tensor Decompositions

Andrzej Cichocki et al., (2015). Tensor Decompositions for Signal Processing Applications, \emph{IEEE SIGNAL PROCESSING MAGAZINE}
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link{MWCAParams-class}} and \code{\link{MWCAResult-class}}.
}

\examples{
  if(interactive()){
    # Test data (single array)
    X <- nnTensor::toyModel("Tucker")@data
    # Default Parameters
    params <- defaultMWCAParams(X)
    # Perform MWCA
    out <- MWCA(params)
  }
}
\keyword{methods}
