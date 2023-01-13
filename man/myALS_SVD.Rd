\name{myALS_SVD}
\docType{methods}
\alias{myALS_SVD}

\title{
Alternating Least Square Singular Value Decomposition (ALS-SVD) as an example of user-defined matrix decomposition.
}

\description{
The input data is assumed to be a matrix.
When algorithms of MWCAParams and CoupledMWCAParams are specified as "myALS_SVD",
This function is called in MWCA and CoupledMWCA.
}

\usage{
myALS_SVD(Xn, k, L2=1e-10, iter=30)
}

\arguments{
\item{Xn}{
The input matrix which has N-rows and M-columns.
}

\item{k}{
The rank parameter (k <= min(N,M))
}

\item{L2}{
The regularization parameter (Default: 1e-10)
}

\item{iter}{
The number of iteration (Default: 30)
}
}

\value{
  The output matrix which has N-rows and k-columns.
}

\author{Koki Tsuyuzaki}

\references{
Madeleine Udell et al., (2016). Generalized Low Rank Models, \emph{Foundations and Trends in Machine Learning}, 9(1).
}
\examples{
  if(interactive()){
    # Test data
    matdata <- matrix(runif(10*20), nrow=10, ncol=20)
    # Perform ALS-SVD
    myALS_SVD(matdata, k=3, L2=0.1, iter=10)
  }
}
\keyword{methods}