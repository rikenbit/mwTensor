\name{myNMF}
\docType{methods}
\alias{myNMF}

\title{
Independent Component Analysis (ICA) as an example of user-defined matrix decomposition.
}

\description{
The input data is assumed to be a matrix.
When algorithms of MWCAParams and CoupledMWCAParams are specified as "myNMF",
This function is called in MWCA and CoupledMWCA.
}

\usage{
myNMF(Xn, k, L1=1e-10, L2=1e-10)
}

\arguments{
\item{Xn}{
The input matrix which has N-rows and M-columns.
}

\item{k}{
The rank parameter (k <= min(N,M))
}

\item{L1}{
The regularization parameter to control the sparseness (Default: 1e-10)
}

\item{L2}{
The regularization parameter to control the overfit (Default: 1e-10)
}
}

\value{
  The output matrix which has N-rows and k-columns.
}

\author{Koki Tsuyuzaki}

\references{
Andrzej CICHOCK, et. al., (2009). Nonnegative Matrix and Tensor Factorizations.
}

\examples{
  if(interactive()){
    # Test data
    matdata <- matrix(runif(10*20), nrow=10, ncol=20)
    myNMF(matdata, k=3, L1=1e-1, L2=1e-2)
  }
}
\keyword{methods}
