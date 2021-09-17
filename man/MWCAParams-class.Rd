\name{MWCAParams-class}
\docType{class}
\alias{MWCAParams-class}
\title{Class "MWCAParams"}

\description{
The parameter object to be specified against MWCA function.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("MWCAParams", ...)}.
}

\section{Slots}{
   \describe{
    \item{X:}{A high-dimensional array.}
    \item{mask:}{A mask array having the same dimension of X.}
    \item{algorithms:}{Algorithms used to decompose the matricised tensor in each mode.}
    \item{dims:}{The lower dimension of each factor matrix.}
    \item{transpose:}{Whether the factor matrix is transposed to calculate core tensor.}
    \item{viz:}{Whether the output is visualized.}
    \item{figdir:}{When viz=TRUE, whether the plot is output in the directory.}
  }
}

\section{Methods}{
  \describe{
    \item{MWCA}{Function to peform MWCA.}
  }
}

\seealso{
  \code{\link{MWCAResult-class}}, \code{\link{MWCA}}
}
