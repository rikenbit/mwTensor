#' Factorization Program Representation
#'
#' A lightweight internal representation for factorization programs.
#' Designed as an abstraction layer between problem specification and
#' the existing \code{MWCAParams}/\code{CoupledMWCAParams} solver parameters.
#'
#' This is a foundation for future recursive decomposition support
#' (decomposing factors obtained from an initial decomposition).
#' Currently, only depth-0 (no refinement) and depth-1 (one-step
#' factor refinement) programs are supported. Full recursive
#' optimization is NOT implemented.
#'
#' @name MWCAProgram
#' @section Structure:
#' An \code{MWCAProgram} is an S3 list with class \code{"MWCAProgram"}
#' containing:
#' \describe{
#'   \item{blocks}{Named list of block descriptors (see \code{MWCAProgramBlock}).}
#'   \item{factors}{Named list of factor descriptors (see \code{MWCAProgramFactor}).}
#'   \item{refinements}{Named list of refinement descriptors
#'     (see \code{MWCAProgramRefinement}). May be empty.}
#' }
NULL

# ================================================================
# Constructors
# ================================================================

#' Create an MWCAProgram
#'
#' @param blocks Named list of block descriptors created by \code{MWCAProgramBlock}.
#' @param factors Named list of factor descriptors created by \code{MWCAProgramFactor}.
#' @param refinements Named list of refinement descriptors created by
#'   \code{MWCAProgramRefinement}. Default is empty.
#' @return An \code{MWCAProgram} object.
#' @export
MWCAProgram <- function(blocks, factors, refinements=list()){
    stopifnot(is.list(blocks), is.list(factors), is.list(refinements))
    obj <- list(blocks=blocks, factors=factors, refinements=refinements)
    class(obj) <- "MWCAProgram"
    obj
}

#' Create a block descriptor
#'
#' @param modes Character vector of mode names for this block.
#' @param factor_map Named character vector mapping mode names to factor names.
#' @param type One of \code{"common"} or \code{"specific"}.
#' @param weight Numeric weight for this block (default 1).
#' @return A list with class \code{"MWCAProgramBlock"}.
#' @export
MWCAProgramBlock <- function(modes, factor_map, type="common", weight=1){
    stopifnot(is.character(modes), length(modes) >= 1)
    stopifnot(is.character(factor_map), !is.null(names(factor_map)))
    stopifnot(type %in% c("common", "specific"))
    stopifnot(is.numeric(weight), length(weight) == 1)
    obj <- list(modes=modes, factor_map=factor_map, type=type, weight=weight)
    class(obj) <- "MWCAProgramBlock"
    obj
}

#' Create a factor descriptor
#'
#' @param mode Character. The mode name this factor is associated with.
#' @param dim Integer. The target lower dimension.
#' @param status One of \code{"decomposed"}, \code{"fixed"}, \code{"frozen"}.
#'   \code{"decomposed"}: will be updated by the solver.
#'   \code{"fixed"}: initialized but not updated (fix=TRUE, decomp=TRUE).
#'   \code{"frozen"}: identity-like, not decomposed (decomp=FALSE).
#' @param algorithm Character or NULL. Algorithm name (e.g. \code{"mySVD"}).
#' @param type One of \code{"common"} or \code{"specific"}.
#' @return A list with class \code{"MWCAProgramFactor"}.
#' @export
MWCAProgramFactor <- function(mode, dim, status="decomposed",
                              algorithm="mySVD", type="common"){
    stopifnot(is.character(mode), length(mode) == 1)
    stopifnot(is.numeric(dim), length(dim) == 1, dim >= 1)
    stopifnot(status %in% c("decomposed", "fixed", "frozen"))
    stopifnot(is.null(algorithm) || is.character(algorithm))
    stopifnot(type %in% c("common", "specific"))
    obj <- list(mode=mode, dim=dim, status=status,
                algorithm=algorithm, type=type)
    class(obj) <- "MWCAProgramFactor"
    obj
}

#' Create a refinement descriptor
#'
#' Specifies that a factor from the parent decomposition should itself
#' be decomposed in a one-step sub-decomposition.
#'
#' @param source_factor Character. Name of the factor to refine.
#' @param algorithm Character. Algorithm for the sub-decomposition.
#' @param dim Integer. Target dimension for the sub-decomposition.
#' @return A list with class \code{"MWCAProgramRefinement"}.
#' @export
MWCAProgramRefinement <- function(source_factor, algorithm="mySVD", dim=2L){
    stopifnot(is.character(source_factor), length(source_factor) == 1)
    stopifnot(is.character(algorithm), length(algorithm) == 1)
    stopifnot(is.numeric(dim), length(dim) == 1, dim >= 1)
    obj <- list(source_factor=source_factor, algorithm=algorithm, dim=dim)
    class(obj) <- "MWCAProgramRefinement"
    obj
}

# ================================================================
# Validator
# ================================================================

#' Validate an MWCAProgram
#'
#' Checks structural consistency of the program without running any
#' computation. Returns a structured result similar to
#' \code{checkCoupledMWCA}.
#'
#' @param program An \code{MWCAProgram} object.
#' @return A list with \code{ok}, \code{errors}, \code{warnings}, \code{summary}.
#' @export
validateMWCAProgram <- function(program){
    stopifnot(inherits(program, "MWCAProgram"))
    errors <- character(0)
    warnings <- character(0)

    blocks <- program$blocks
    factors <- program$factors
    refinements <- program$refinements

    # Names must be present
    if(is.null(names(blocks)) || any(names(blocks) == "")){
        errors <- c(errors, "All blocks must be named")
    }
    if(is.null(names(factors)) || any(names(factors) == "")){
        errors <- c(errors, "All factors must be named")
    }
    if(length(refinements) > 0 &&
       (is.null(names(refinements)) || any(names(refinements) == ""))){
        errors <- c(errors, "All refinements must be named")
    }

    # Factor references in blocks must exist
    all_factor_names <- names(factors)
    for(bname in names(blocks)){
        block <- blocks[[bname]]
        refs <- as.character(block$factor_map)
        missing <- setdiff(refs, all_factor_names)
        if(length(missing) > 0){
            errors <- c(errors, paste0(
                "Block '", bname, "' references undefined factor(s): ",
                paste(missing, collapse=", ")))
        }
        # factor_map names must match modes
        map_modes <- names(block$factor_map)
        if(!all(map_modes %in% block$modes)){
            bad <- setdiff(map_modes, block$modes)
            errors <- c(errors, paste0(
                "Block '", bname, "' factor_map references undefined mode(s): ",
                paste(bad, collapse=", ")))
        }
        # All modes should have a factor mapping
        unmapped <- setdiff(block$modes, map_modes)
        if(length(unmapped) > 0){
            errors <- c(errors, paste0(
                "Block '", bname, "' has unmapped mode(s): ",
                paste(unmapped, collapse=", ")))
        }
    }

    # Refinement names must not collide with factor names.
    # Refinements produce RefinedFactor objects keyed by their name;
    # if a refinement shares a name with a factor, references become
    # ambiguous (is "A2" the base factor or the refinement output?).
    if(length(refinements) > 0){
        name_collision <- intersect(names(refinements), all_factor_names)
        if(length(name_collision) > 0){
            errors <- c(errors, paste0(
                "Refinement name(s) collide with factor name(s): ",
                paste(name_collision, collapse=", "),
                ". Use distinct names for refinements."))
        }
    }

    # Refinement source_factor must reference a defined factor
    for(rname in names(refinements)){
        ref <- refinements[[rname]]
        if(!(ref$source_factor %in% all_factor_names)){
            errors <- c(errors, paste0(
                "Refinement '", rname, "' references undefined factor '",
                ref$source_factor, "'"))
        }
    }

    # Depth check: a refinement's source_factor must be a base factor
    # from the program, not the output of another refinement.
    # Since refinement outputs are keyed by refinement name, depth > 1
    # would mean one refinement's source_factor equals another
    # refinement's name (i.e., trying to refine a refinement's output).
    refinement_names <- names(refinements)
    for(rname in refinement_names){
        src <- refinements[[rname]]$source_factor
        if(src %in% refinement_names){
            errors <- c(errors, paste0(
                "Refinement '", rname, "' targets '", src,
                "' which is itself a refinement output ",
                "(depth > 1 not supported)"))
        }
    }

    # Warn if multiple refinements target the same source factor
    if(length(refinements) >= 2){
        src_factors <- vapply(refinements, function(r) r$source_factor,
            character(1))
        dup_src <- src_factors[duplicated(src_factors)]
        if(length(dup_src) > 0){
            warnings <- c(warnings, paste0(
                "Multiple refinements target the same factor: ",
                paste(unique(dup_src), collapse=", ")))
        }
    }

    # Check factor type consistency with block type
    for(bname in names(blocks)){
        block <- blocks[[bname]]
        for(fname in as.character(block$factor_map)){
            if(fname %in% all_factor_names){
                if(factors[[fname]]$type != block$type){
                    warnings <- c(warnings, paste0(
                        "Factor '", fname, "' has type '", factors[[fname]]$type,
                        "' but is used in block '", bname,
                        "' with type '", block$type, "'"))
                }
            }
        }
    }

    # Warn if no blocks
    if(length(blocks) == 0){
        warnings <- c(warnings, "Program has no blocks")
    }

    # Build summary
    ok <- length(errors) == 0
    n_err <- length(errors)
    n_warn <- length(warnings)
    if(ok && n_warn == 0){
        summary_str <- "Program is valid"
    } else if(ok){
        summary_str <- paste0("Program is valid with ", n_warn, " warning(s)")
    } else {
        summary_str <- paste0("Program is invalid: ", n_err, " error(s), ",
            n_warn, " warning(s)")
    }
    list(ok=ok, errors=errors, warnings=warnings, summary=summary_str)
}

# ================================================================
# Print / Summary
# ================================================================

#' @export
print.MWCAProgram <- function(x, ...){
    cat("MWCAProgram\n")
    cat(sprintf("  Blocks:      %d\n", length(x$blocks)))
    cat(sprintf("  Factors:     %d\n", length(x$factors)))
    cat(sprintf("  Refinements: %d\n", length(x$refinements)))
    if(length(x$blocks) > 0){
        cat("  Block details:\n")
        for(nm in names(x$blocks)){
            b <- x$blocks[[nm]]
            fnames <- paste(as.character(b$factor_map), collapse=", ")
            cat(sprintf("    %s [%s]: modes=%s -> factors={%s}\n",
                nm, b$type, paste(b$modes, collapse=","), fnames))
        }
    }
    if(length(x$factors) > 0){
        cat("  Factor details:\n")
        for(nm in names(x$factors)){
            f <- x$factors[[nm]]
            algo <- if(is.null(f$algorithm)) "none" else f$algorithm
            cat(sprintf("    %s [%s]: mode=%s, dim=%d, status=%s, algo=%s\n",
                nm, f$type, f$mode, f$dim, f$status, algo))
        }
    }
    if(length(x$refinements) > 0){
        cat("  Refinements:\n")
        for(nm in names(x$refinements)){
            r <- x$refinements[[nm]]
            cat(sprintf("    %s: refine %s with %s (dim=%d)\n",
                nm, r$source_factor, r$algorithm, r$dim))
        }
    }
    invisible(x)
}

#' @export
summary.MWCAProgram <- function(object, ...){
    v <- validateMWCAProgram(object)
    cat("MWCAProgram Summary\n")
    cat(sprintf("  %d block(s), %d factor(s), %d refinement(s)\n",
        length(object$blocks), length(object$factors),
        length(object$refinements)))
    cat(sprintf("  Status: %s\n", v$summary))
    if(length(v$errors) > 0){
        cat("  Errors:\n")
        for(e in v$errors) cat(sprintf("    - %s\n", e))
    }
    if(length(v$warnings) > 0){
        cat("  Warnings:\n")
        for(w in v$warnings) cat(sprintf("    - %s\n", w))
    }
    invisible(v)
}
