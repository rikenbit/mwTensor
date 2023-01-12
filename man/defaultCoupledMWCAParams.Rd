\name{defaultCoupledMWCAParams}
\docType{methods}
\alias{defaultCoupledMWCAParams}

\title{
Default parameters for CoupledMWCA
}

\description{
The input list is assumed to contain multiple arrays.
}

\usage{
defaultCoupledMWCAParams(Xs, common_model)
}

\arguments{
\item{Xs}{A list object containing multiple arrays}
\item{common_model}{A list object to describe the relationship between dimensions of each tensor and factor matrices extracted from the tensor}
}

\value{
CoupledMWCAParams object.
}

\references{
Andrzej Cichocki et al., (2016). Tensor Networks for Dimensionality Reduction and Large-scale Optimization: Part 1 Low-Rank Tensor Decompositions

Andrzej Cichocki et al., (2015). Tensor Decompositions for Signal Processing Applications, \emph{IEEE SIGNAL PROCESSING MAGAZINE}
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link{CoupledMWCAParams-class}} and \code{\link{MWCAResult-class}}.
}

\examples{
  if(interactive()){
  	# Test data (multiple arrays)
    Xs=list(
        X1=array(runif(7*4), dim=c(7,4)),
        X2=array(runif(4*5*6), dim=c(4,5,6)),
        X3=array(runif(6*8), dim=c(6,8)))
                
    common_model=list(
        X1=list(I1="A1", I2="A2"),
        X2=list(I2="A2", I3="A3", I4="A4"),
        X3=list(I4="A4", I5="A5"))

    params <- defaultCoupledMWCAParams(X)
  }
}
\keyword{methods}
