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
    \item{model:}{model of CoupledMWCAParams.}
    \item{initial:}{initial of CoupledMWCAParams.}
    \item{algorithms:}{algorithms of CoupledMWCAParams.}
    \item{iteration:}{iteration of CoupledMWCAParams.}
    \item{decomp:}{decomp of CoupledMWCAParams.}
    \item{fix:}{fix of CoupledMWCAParams.}
    \item{dims:}{dims of CoupledMWCAParams.}
    \item{transpose:}{transpose of CoupledMWCAParams.}
    \item{coretype:}{coretype of CoupledMWCAParams.}
    \item{thr:}{thr of CoupledMWCAParams.}
    \item{viz:}{viz of CoupledMWCAParams.}
    \item{figdir:}{figdir of CoupledMWCAParams.}
    \item{verbose:}{verbose of CoupledMWCAParams.}
    \item{factors:}{The factor matrices of CoupledMWCA.}
    \item{cores:}{The core tensor of CoupledMWCA.}
    \item{rec_error:}{The reconstructed error.}
    \item{train_error:}{Training Error. train_error + test_error = rec_error.}
    \item{test_error:}{Test Error. train_error + test_error = rec_error.}
    \item{rel_change:}{The relative change of each iteration step.}
  }
}

\seealso{
  \code{\link{CoupledMWCAParams-class}}, \code{\link{CoupledMWCA}}
}
