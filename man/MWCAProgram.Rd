\name{MWCAProgram}
\alias{MWCAProgram}
\alias{MWCAProgramBlock}
\alias{MWCAProgramFactor}
\alias{MWCAProgramRefinement}
\alias{print.MWCAProgram}
\alias{summary.MWCAProgram}

\title{Factorization Program Representation}

\description{
A lightweight S3-based internal representation for factorization programs.
This is a structural description layer between problem specification and
the existing solver parameters (\code{\linkS4class{MWCAParams}},
\code{\linkS4class{CoupledMWCAParams}}).

Designed as a foundation for future recursive decomposition support
(decomposing factors obtained from an initial decomposition).
Currently only depth-0 (no refinement) and depth-1 (one-step factor
refinement) programs are representable. Full recursive optimization
is \strong{not} implemented.
}

\usage{
MWCAProgram(blocks, factors, refinements = list())
MWCAProgramBlock(modes, factor_map, type = "common", weight = 1)
MWCAProgramFactor(mode, dim, status = "decomposed",
                  algorithm = "mySVD", type = "common")
MWCAProgramRefinement(source_factor, algorithm = "mySVD", dim = 2L)
}

\arguments{
  \item{blocks}{Named list of block descriptors (created by \code{MWCAProgramBlock}).}
  \item{factors}{Named list of factor descriptors (created by \code{MWCAProgramFactor}).}
  \item{refinements}{Named list of refinement descriptors (created by
    \code{MWCAProgramRefinement}). Default is empty.}
  \item{modes}{Character vector of mode names for the block.}
  \item{factor_map}{Named character vector mapping mode names to factor names.}
  \item{type}{One of \code{"common"} or \code{"specific"}.}
  \item{weight}{Numeric weight for the block (default 1).}
  \item{mode}{Character. The mode name associated with a factor.}
  \item{dim}{Integer. Target lower dimension.}
  \item{status}{One of \code{"decomposed"} (updated by solver),
    \code{"fixed"} (initialized but not updated), or
    \code{"frozen"} (identity-like, not decomposed).}
  \item{algorithm}{Character or NULL. Decomposition algorithm name.}
  \item{source_factor}{Character. Name of the factor to refine.}
}

\value{
  \code{MWCAProgram} returns an S3 object of class \code{"MWCAProgram"}.
  \code{MWCAProgramBlock}, \code{MWCAProgramFactor}, and
  \code{MWCAProgramRefinement} return S3 objects of their respective classes.
}

\author{Koki Tsuyuzaki}

\seealso{
\code{\link{validateMWCAProgram}}, \code{\link{compileMWCAProgram}},
\code{\link{CoupledMWCA}}, \code{\link{MWCA}}.
}

\examples{
  if(interactive()){
    # A simple 3-block coupled program
    prog <- MWCAProgram(
      blocks=list(
        X1=MWCAProgramBlock(modes=c("I1","I2"),
          factor_map=c(I1="A1", I2="A2")),
        X2=MWCAProgramBlock(modes=c("I2","I3","I4"),
          factor_map=c(I2="A2", I3="A3", I4="A4")),
        X3=MWCAProgramBlock(modes=c("I4","I5"),
          factor_map=c(I4="A4", I5="A5"))),
      factors=list(
        A1=MWCAProgramFactor(mode="I1", dim=3),
        A2=MWCAProgramFactor(mode="I2", dim=3),
        A3=MWCAProgramFactor(mode="I3", dim=5),
        A4=MWCAProgramFactor(mode="I4", dim=4),
        A5=MWCAProgramFactor(mode="I5", dim=4)))
    print(prog)
    validateMWCAProgram(prog)

    # One-step refinement (structural only; compilation not yet supported)
    prog_ref <- MWCAProgram(
      blocks=prog$blocks, factors=prog$factors,
      refinements=list(
        R1=MWCAProgramRefinement(source_factor="A2",
          algorithm="myNMF", dim=2)))
    validateMWCAProgram(prog_ref)
  }
}

\keyword{classes}
