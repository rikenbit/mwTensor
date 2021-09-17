\name{plotTensor3Ds}
\docType{methods}
\alias{plotTensor3Ds}

\title{
Plot function for visualization of tensor data structure
}

\description{
Multiple multi-dimensional arrays and matrices are visualized simultaneously.
}

\usage{
plotTensor3Ds(Xs)
}

\arguments{
  \item{Xs}{
A List object containing multi-dimensional array (or matrix) in
each element.
  }
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link[nnTensor]{plotTensor3D}} and \code{\link[nnTensor]{plotTensor2D}}.
}

\examples{
Xs <- toyModel(model = "coupled_CP_Easy")

tmp <- tempdir()

png(filename=paste0(tmp, "/couled_CP.png"))
plotTensor3Ds(Xs)
dev.off()
}

\keyword{methods}