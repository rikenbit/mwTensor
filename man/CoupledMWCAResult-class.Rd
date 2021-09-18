\name{CoupledMWCAResult-class}
\docType{class}
\alias{CoupledMWCAResult-class}
\title{Class "CoupledMWCAResult"}

\description{
The result object genarated by CoupledMWCA function.
}

\section{Slots}{
   \describe{
    \item{weights:}{weights of CoupledMWCAParams.}
    \item{common_model:}{common_model of CoupledMWCAParams.}
    \item{common_initial:}{common_initial of CoupledMWCAParams.}
    \item{common_algorithms:}{common_algorithms of CoupledMWCAParams.}
    \item{common_iteration:}{common_iteration of CoupledMWCAParams.}
    \item{common_decomp:}{common_decomp of CoupledMWCAParams.}
    \item{common_fix:}{common_fix of CoupledMWCAParams.}
    \item{common_dims:}{common_dims of CoupledMWCAParams.}
    \item{common_transpose:}{common_transpose of CoupledMWCAParams.}
    \item{common_coretype:}{common_coretype of CoupledMWCAParams.}
    \item{common_factors:}{Common factor matrices of CoupledMWCA.}
    \item{common_cores:}{Common core tensors of CoupledMWCA.}
    \item{specific_model:}{specific_model of CoupledMWCAParams.}
    \item{specific_initial:}{specific_initial of CoupledMWCAParams.}
    \item{specific_algorithms:}{specific_algorithms of CoupledMWCAParams.}
    \item{specific_iteration:}{specific_iteration of CoupledMWCAParams.}
    \item{specific_decomp:}{specific_decomp of CoupledMWCAParams.}
    \item{specific_fix:}{specific_fix of CoupledMWCAParams.}
    \item{specific_dims:}{specific_dims of CoupledMWCAParams.}
    \item{specific_transpose:}{specific_transpose of CoupledMWCAParams.}
    \item{specific_coretype:}{specific_coretype of CoupledMWCAParams.}
    \item{specific_factors:}{Data specific factor matrices of CoupledMWCA.}
    \item{specific_cores:}{Data specific core tensors of CoupledMWCA.}
    \item{specific:}{specific of CoupledMWCAParams.}
    \item{thr:}{thr of CoupledMWCAParams.}
    \item{viz:}{viz of CoupledMWCAParams.}
    \item{figdir:}{figdir of CoupledMWCAParams.}
    \item{verbose:}{verbose of CoupledMWCAParams.}
    \item{rec_error:}{The reconstructed error.}
    \item{train_error:}{Training Error. train_error + test_error = rec_error.}
    \item{test_error:}{Test Error. train_error + test_error = rec_error.}
    \item{rel_change:}{The relative change of each iteration step.}
  }
}

\seealso{
  \code{\link{CoupledMWCAParams-class}}, \code{\link{CoupledMWCA}}
}
