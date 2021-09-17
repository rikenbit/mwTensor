\name{MWCAResult-class}
\docType{class}
\alias{MWCAResult-class}
\title{Class "MWCAResult"}

\description{
The result object genarated by MWCA function.
}

\section{Slots}{
   \describe{
    \item{algorithms:}{algorithm of MWCAParams.}
    \item{dims:}{dims of MWCAParams.}
    \item{transpose:}{transpose of MWCAParams.}
    \item{viz:}{viz of MWCAParams.}
    \item{figdir:}{figdir of MWCAParams.}
    \item{factors:}{The factor matrices of MWCA.}
    \item{core:}{The core tensor of MWCA.}
    \item{rec_error:}{The reconstructed error.}
    \item{train_error:}{Training Error. train_error + test_error = rec_error.}
    \item{test_error:}{Test Error. train_error + test_error = rec_error.}
  }
}

\seealso{
  \code{\link{MWCAParams-class}}, \code{\link{MWCA}}
}
