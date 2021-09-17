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
    Xs <- toyModel("coupled_CP_Easy")

    params <- new("CoupledMWCAParams", Xs=Xs)
    out <- CoupledMWCA(params)
  }
}
\keyword{methods}
