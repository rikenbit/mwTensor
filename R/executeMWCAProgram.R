#' Execute an MWCAProgram (Experimental)
#'
#' Compiles and runs an \code{MWCAProgram}, including optional one-step
#' factor refinements. For programs without refinements, this is
#' equivalent to \code{compileMWCAProgram} followed by \code{CoupledMWCA}.
#'
#' Refinements are applied \strong{after} the main decomposition completes,
#' using \code{\link{refineFactor}}.
#'
#' @param program An \code{MWCAProgram} object.
#' @param Xs Named list of input arrays.
#' @param ... Additional parameters passed to \code{compileMWCAProgram}.
#' @return A list with components:
#' \describe{
#'   \item{fit}{The \code{MWCAResult} or \code{CoupledMWCAResult}.}
#'   \item{refinements}{Named list of \code{RefinedFactor} objects
#'     (empty list if no refinements).}
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
