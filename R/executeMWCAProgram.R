#' Execute an MWCAProgram (Experimental)
#'
#' Compiles and runs an \code{MWCAProgram}, including optional one-step
#' factor refinements. For programs without refinements, this is
#' equivalent to \code{compileMWCAProgram} followed by
#' \code{MWCA}/\code{CoupledMWCA}.
#'
#' \strong{Two-phase architecture:}
#' \enumerate{
#'   \item \strong{Compile + solve}: the base program (blocks + factors,
#'     without refinements) is compiled to solver parameters via
#'     \code{\link{compileMWCAProgram}} and then passed to the
#'     appropriate solver.
#'   \item \strong{Post-hoc refinement}: each refinement specification
#'     is applied to the completed fit via \code{\link{refineFactor}}.
#'     This is a one-shot decomposition of a factor matrix, not an
#'     iterative joint optimization.
#' }
#'
#' \strong{Design note}: \code{compileMWCAProgram} rejects programs
#' that contain refinements because the solver parameters
#' (\code{MWCAParams}/\code{CoupledMWCAParams}) cannot represent
#' refinement steps. \code{executeMWCAProgram} handles this by
#' compiling only the base program and applying refinements
#' separately. This is a proof-of-concept for "decomposition of a
#' decomposition" and does \emph{not} implement full recursive
#' optimization.
#'
#' @param program An \code{MWCAProgram} object.
#' @param Xs Named list of input arrays.
#' @param ... Additional parameters passed to \code{compileMWCAProgram}.
#' @return A list with components:
#' \describe{
#'   \item{fit}{The \code{MWCAResult} or \code{CoupledMWCAResult}.}
#'   \item{refinements}{Named list of \code{RefinedFactor} objects
#'     (empty list if no refinements were specified).}
#' }
#' @export
executeMWCAProgram <- function(program, Xs, ...){
    stopifnot(inherits(program, "MWCAProgram"))

    # Compile the base program (without refinements)
    base_program <- program
    base_program$refinements <- list()
    params <- compileMWCAProgram(base_program, Xs, ...)

    # Run the main decomposition
    if(is(params, "MWCAParams")){
        fit <- MWCA(params)
    } else {
        fit <- CoupledMWCA(params)
    }

    # Apply refinements
    refined <- list()
    for(rname in names(program$refinements)){
        ref_spec <- program$refinements[[rname]]
        refined[[rname]] <- refineFactor(
            fit=fit,
            factor_name=ref_spec$source_factor,
            algorithm=ref_spec$algorithm,
            dim=ref_spec$dim)
    }

    list(fit=fit, refinements=refined)
}
