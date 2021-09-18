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

    \item{Xs:}{A list containing multiple high-dimensional arrays.}
    \item{mask:}{A list containing multiple high-dimensional arrays, in which 0 or 1 values are filled to specify the missing elements.}
    \item{weights:}{A list containing multiple high-dimensional arrays, in which some numeric values are specified to weigth each data.}

    \emph{2. Common Model setting}
    Each item must be a nested list object that is as long as the number of data and is named after the data.

    \item{common_model:}{Each element of the list must be a list corresponding the dimention name of data and common factor matrices name.}

    \emph{3. Common Factor matrix-wise setting}
    Each item must be a list object that is as long as the number of common factor matrices and is named after the factor matrices.

    \item{common_initial:}{The initial values of common factor matrices. If nothing is specified, random matrices are used.}
    \item{common_algorithms:}{Algorithms used to decompose the matricised tensor in each mode.}
    \item{common_iteration:}{The number of iterations.}
    \item{common_decomp:}{If FALSE is specified, unit matrix is used as the common factor matrix.}
    \item{common_fix:}{If TRUE is specified, the common factor matrix is not updated in the iteration.}
    \item{common_dims:}{The lower dimension of each common factor matrix.}
    \item{common_transpose:}{Whether the common factor matrix is transposed to calculate core tensor.}
    \item{common_coretype:}{If "CP" is specified, all the core tensors become diagonal core tensors. If "Tucker" is specified, all the core tensors become dense core tensors.}

    \emph{4. Specific Model setting}
    Each item must be a nested list object that is as long as the number of data and is named after the data.

    \item{specific_model:}{Each element of the list must be a list corresponding the dimention name of data and data specific factor matrices name.}

    \emph{5. Specific Factor matrix-wise setting}
    Each item must be a list object that is as long as the number of data specific factor matrices and is named after the factor matrices.

    \item{specific_initial:}{The initial values of data specific factor matrices. If nothing is specified, random matrices are used.}
    \item{specific_algorithms:}{Algorithms used to decompose the matricised tensor in each mode.}
    \item{specific_iteration:}{The number of iterations.}
    \item{specific_decomp:}{If FALSE is specified, unit matrix is used as the data specific factor matrix.}
    \item{specific_fix:}{If TRUE is specified, the data specific factor matrix is not updated in the iteration.}
    \item{specific_dims:}{The lower dimension of each data specific factor matrix.}
    \item{specific_transpose:}{Whether the data specific factor matrix is transposed to calculate core tensor.}
    \item{specific_coretype:}{If "CP" is specified, all the core tensors become diagonal core tensors. If "Tucker" is specified, all the core tensors become dense core tensors.}

    \emph{6. Other option}
    Each item must to be a vector of length 1.

    \item{specific:}{Whether data specific factor matrices are also calculated.}
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
