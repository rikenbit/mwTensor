\name{myCX}
\docType{methods}
\alias{myCX}

\title{
CX Decomposition as an example of user-defined matrix decomposition.
}

\description{
The input data is assumed to be a matrix.
When algorithms of MWCAParams and CoupledMWCAParams are specified as "myCX",
This function is called in MWCA and CoupledMWCA.
}

\usage{
myCX(Xn, k)
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
Petros Drineas et al., (2008). Relative-Error CUR Matrix Decompositions, \emph{SIAM Journal on Matrix Analysis and Applications}, 30(2), 844-881.
}

\examples{
  if(interactive()){
    # Test data
    matdata <- matrix(runif(10*20), nrow=10, ncol=20)
    myCX(matdata, k=3)
  }
}
\keyword{methods}