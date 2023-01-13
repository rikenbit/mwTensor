\name{myICA}
\docType{methods}
\alias{myICA}

\title{
Independent Component Analysis (ICA) as an example of user-defined matrix decomposition.
}

\description{
The input data is assumed to be a matrix.
When algorithms of MWCAParams and CoupledMWCAParams are specified as "myICA",
This function is called in MWCA and CoupledMWCA.
}

\usage{
myICA(Xn, k)
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

\references{
A. Hyvarinen. (1999). Fast and Robust Fixed-Point Algorithms for Independent Component Analysis. \emph{IEEE Transactions on Neural Networks}, 10(3), 626-634.
}

\examples{
  if(interactive()){
    # Test data
    matdata <- matrix(runif(10*20), nrow=10, ncol=20)
    # Perform ICA
    myICA(matdata, k=3)
  }
}
\keyword{methods}
