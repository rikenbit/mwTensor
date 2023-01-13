\name{CoupledMWCA}
\docType{methods}
\alias{CoupledMWCA,CoupledMWCAParams-method}
\alias{CoupledMWCA}

\title{
Coupled Multi-way Component Analysis (CoupledMWCA)
}

\description{
The input is assumed to be a CoupledMWCAParams object.
}

\usage{
CoupledMWCA(params)
}

\arguments{
\item{params}{CoupledMWCAParams object}
}

\value{
CoupledMWCAResult object.
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link{CoupledMWCAParams-class}} and \code{\link{CoupledMWCAResult-class}}.
}

\examples{
  if(interactive()){
    # Test data (multiple arrays)
    Xs=list(
        X1=array(runif(7*4), dim=c(7,4)),
        X2=array(runif(4*5*6), dim=c(4,5,6)),
        X3=array(runif(6*8), dim=c(6,8)))
    # Setting of factor matrices
    common_model=list(
        X1=list(I1="A1", I2="A2"),
        X2=list(I2="A2", I3="A3", I4="A4"),
        X3=list(I4="A4", I5="A5"))
    # Default Parameters
    params <- defaultCoupledMWCAParams(Xs=Xs, common_model=common_model)
    # Perform Coupled MWCA
    out <- CoupledMWCA(params)
  }
}
\keyword{methods}
