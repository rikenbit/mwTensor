\name{CoupledMWCAParams-class}
\docType{class}
\alias{CoupledMWCAParams-class}
\title{Class "CoupledMWCAParams"}

\description{
The parameter object to be specified against CoupledMWCA function.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CoupledMWCAParams", ...)}.
}

\section{Slots}{
   \describe{
    MWCAParams has four settings as follows.
    For each setting, the list must have the same structure.

    \emph{1. Data-wise setting}
    Each item must be a list object that is as long as the number of data and is named after the data.
    \item{X:}{A list containing multiple high-dimensional arrays.}
    \item{mask:}{A list containing multiple high-dimensional arrays, in which 0 or 1 values are filled to specify the missing elements.}
    \item{weight:}{A list containing multiple high-dimensional arrays, in which some numeric values are specified to weigth each data.}

    \emph{2. Model setting}
    Each item must be a nested list object that is as long as the number of data and is named after the data.
    \item{model:}{Each element of the list must be a list corresponding the dimention name of data and Factor matrix name.}

    \emph{3. Factor matrix-wise setting}
    Each item must be a list object that is as long as the number of factor matrices and is named after the factor matrices.

    \item{initial:}{The initial values of factor matrices. If nothing is specified, random matrices are used.}
    \item{algorithms:}{Algorithms used to decompose the matricised tensor in each mode.}
    \item{iteration:}{The number of iterations.}
    \item{decomp:}{If FALSE is specified, unit matrix is used as the factor matrix.}
    \item{fix:}{If TRUE is specified, the factor matrix is not updated in the iteration.}
    \item{dims:}{The lower dimension of each factor matrix.}
    \item{transpose:}{Whether the factor matrix is transposed to calculate core tensor.}

    \emph{4. Other option}
    Each item must to be a vector of length 1.
    \item{coretype:}{If "CP" is specified, all the core tensors become diagonal core tensors. If "Tucker" is specified, all the core tensors become dense core tensors.}
    \item{thr:}{The threshold to stop the iteration. The higher the value, the faster the iteration will stop.}
    \item{viz:}{Whether the output is visualized.}
    \item{figdir:}{When viz=TRUE, whether the plot is output in the directory.}
    \item{verbose:}{Whether the process is monitored by verbose messages.}
  }
}

\section{Methods}{
  \describe{
    \item{CoupledMWCA}{Function to peform CoupledMWCA.}
  }
}

\seealso{
  \code{\link{CoupledMWCAResult-class}}, \code{\link{CoupledMWCA}}
}
