\name{mySVD}
\docType{methods}
\alias{mySVD}

\title{
Singular Value Decomposition (SVD) as an example of user-defined matrix decomposition.
}

\description{
The input data is assumed to be a matrix.
When algorithms of MWCAParams and CoupledMWCAParams are specified as "mySVD",
This function is called in MWCA and CoupledMWCA.
}

\usage{
mySVD(Xn, k)
}

\arguments{
\item{Xn}{
The input matrix which has N-rows and M-columns.
}

\item{k}{
The rank parameter (k <= min(N,M))
}
}

\value{
  The output matrix which has N-rows and k-columns.
}

\author{Koki Tsuyuzaki}

\examples{
  if(interactive()){
    # Test data
    matdata <- matrix(runif(10*20), nrow=10, ncol=20)
    mySVD(matdata, k=3)
  }
}
\keyword{methods}