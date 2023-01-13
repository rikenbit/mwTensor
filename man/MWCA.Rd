\name{MWCA}
\docType{methods}
\alias{MWCA,MWCAParams-method}
\alias{MWCA}

\title{
Multi-way Component Analysis (MWCA)
}

\description{
The input is assumed to be a MWCAParams object.
}

\usage{
MWCA(params)
}

\arguments{
\item{params}{MWCAParams object}
}

\value{
MWCAResult object.
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
