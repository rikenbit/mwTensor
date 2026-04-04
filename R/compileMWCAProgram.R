#' Compile an MWCAProgram to Solver Parameters
#'
#' Converts a validated, non-recursive \code{MWCAProgram} into
#' \code{MWCAParams} (single-block, no refinements) or
#' \code{CoupledMWCAParams} (multi-block, no refinements).
#'
#' Programs with refinements cannot currently be compiled; this
#' function will error. Refinement compilation is reserved for
#' future recursive decomposition support.
#'
#' @param program An \code{MWCAProgram} object.
#' @param Xs Named list of input arrays, matching block names.
#' @param ... Additional parameters passed to the compiled params
#'   (e.g. \code{thr}, \code{verbose}).
#' @return An \code{MWCAParams} or \code{CoupledMWCAParams} object.
#' @export
compileMWCAProgram <- function(program, Xs, ...){
    stopifnot(inherits(program, "MWCAProgram"))

    # Validate first
    v <- validateMWCAProgram(program)
    if(!v$ok){
        stop(paste0("Cannot compile invalid program: ",
            paste(v$errors, collapse="; ")))
    }

    # Refinements not yet compilable
    if(length(program$refinements) > 0){
        stop("Compilation of programs with refinements is not yet supported")
    }

    # Xs must be a named list matching blocks
    stopifnot(is.list(Xs), !is.null(names(Xs)))
    block_names <- names(program$blocks)
    missing_data <- setdiff(block_names, names(Xs))
    if(length(missing_data) > 0){
        stop(paste0("Xs is missing data for block(s): ",
            paste(missing_data, collapse=", ")))
    }

    extras <- list(...)

    # Separate common and specific blocks/factors
    common_blocks <- list()
    specific_blocks <- list()
    for(bname in block_names){
        b <- program$blocks[[bname]]
        if(b$type == "common"){
            common_blocks[[bname]] <- b
        } else {
            specific_blocks[[bname]] <- b
        }
    }
    has_specific <- length(specific_blocks) > 0

    common_factors <- Filter(function(f) f$type == "common", program$factors)
    specific_factors <- Filter(function(f) f$type == "specific", program$factors)

    # Single block, no specific -> MWCAParams
    if(length(block_names) == 1 && !has_specific){
        return(.compileSingleBlock(program, Xs, common_factors, extras))
    }

    # Multi-block or has specific -> CoupledMWCAParams
    .compileMultiBlock(program, Xs, common_blocks, specific_blocks,
                       common_factors, specific_factors, has_specific, extras)
}

# ----------------------------------------------------------------
# Single-block compiler -> MWCAParams
# ----------------------------------------------------------------
.compileSingleBlock <- function(program, Xs, common_factors, extras){
    bname <- names(program$blocks)[1]
    block <- program$blocks[[bname]]
    X <- Xs[[bname]]
    stopifnot(is.array(X))

    # Order factors by mode order in the block
    fnames <- as.character(block$factor_map[block$modes])
    algorithms <- character(length(fnames))
    dims <- numeric(length(fnames))
    for(i in seq_along(fnames)){
        f <- program$factors[[fnames[i]]]
        algorithms[i] <- if(is.null(f$algorithm)) "mySVD" else f$algorithm
        dims[i] <- f$dim
    }

    # Status -> transpose: frozen factors use transpose=TRUE conceptually
    # but in MWCAParams, transpose is a single logical for all modes
    # For simplicity, use FALSE (matching existing defaults)
    transpose <- if(!is.null(extras$transpose)) extras$transpose else FALSE

    new("MWCAParams",
        X=X,
        mask=if(!is.null(extras$mask)) extras$mask else NULL,
        pseudocount=if(!is.null(extras$pseudocount)) extras$pseudocount
            else .Machine$double.eps,
        algorithms=algorithms,
        dims=as.integer(dims),
        transpose=transpose,
        viz=if(!is.null(extras$viz)) extras$viz else FALSE,
        figdir=if(!is.null(extras$figdir)) extras$figdir else NULL)
}

# ----------------------------------------------------------------
# Multi-block compiler -> CoupledMWCAParams
# ----------------------------------------------------------------
.compileMultiBlock <- function(program, Xs, common_blocks, specific_blocks,
                               common_factors, specific_factors,
                               has_specific, extras){
    block_names <- names(program$blocks)
    Xs_ordered <- Xs[block_names]

    # Build common_model: list(X1=list(I1="A1", I2="A2"), ...)
    # Must cover ALL Xs, not just common blocks
    common_model <- list()
    for(bname in names(common_blocks)){
        block <- common_blocks[[bname]]
        model_entry <- as.list(block$factor_map[block$modes])
        common_model[[bname]] <- model_entry
    }
    # Add dummy common model entries for blocks that only have specific factors
    for(bname in block_names){
        if(!(bname %in% names(common_model))){
            block <- program$blocks[[bname]]
            n_modes <- length(block$modes)
            dummy_Inames <- paste0("I_", bname, "_", seq_len(n_modes))
            dummy_Anames <- paste0("A_", bname, "_", seq_len(n_modes))
            names(dummy_Anames) <- dummy_Inames
            common_model[[bname]] <- as.list(dummy_Anames)
            # Add dummy common factors
            for(k in seq_len(n_modes)){
                fname <- dummy_Anames[k]
                common_factors[[fname]] <- MWCAProgramFactor(
                    mode=dummy_Inames[k], dim=1L,
                    status="frozen", algorithm="mySVD", type="common")
            }
        }
    }

    # Build factor-level settings for common
    cf_names <- names(common_factors)
    common_initial <- .defaultListFactors_prog(cf_names)
    common_algorithms <- lapply(common_factors, function(f){
        if(is.null(f$algorithm)) "mySVD" else f$algorithm
    })
    common_dims <- lapply(common_factors, function(f) f$dim)
    common_decomp <- lapply(common_factors, function(f) f$status != "frozen")
    common_fix <- lapply(common_factors, function(f) f$status == "fixed")
    common_transpose <- .defaultListFactors_prog(cf_names, FALSE)
    common_iteration <- .defaultListFactors_prog(cf_names,
        if(!is.null(extras$iteration)) extras$iteration else 100)

    # Build specific model and settings
    if(has_specific){
        specific_model <- list()
        for(bname in names(specific_blocks)){
            block <- specific_blocks[[bname]]
            model_entry <- as.list(block$factor_map[block$modes])
            specific_model[[bname]] <- model_entry
        }
        # Ensure specific_model covers all Xs
        for(bname in block_names){
            if(!(bname %in% names(specific_model))){
                # Add empty specific model for blocks without specific factors
                # Need dummy modes - use block's modes with unique specific factor names
                block <- program$blocks[[bname]]
                n_modes <- length(block$modes)
                dummy_Jnames <- paste0("J_", bname, "_", seq_len(n_modes))
                dummy_Bnames <- paste0("B_", bname, "_", seq_len(n_modes))
                names(dummy_Bnames) <- dummy_Jnames
                specific_model[[bname]] <- as.list(dummy_Bnames)
                # Add dummy factors
                for(k in seq_len(n_modes)){
                    fname <- dummy_Bnames[k]
                    specific_factors[[fname]] <- MWCAProgramFactor(
                        mode=dummy_Jnames[k], dim=1L,
                        status="frozen", algorithm="mySVD", type="specific")
                }
            }
        }

        sf_names <- names(specific_factors)
        specific_initial <- .defaultListFactors_prog(sf_names)
        specific_algorithms <- lapply(specific_factors, function(f){
            if(is.null(f$algorithm)) "mySVD" else f$algorithm
        })
        specific_dims <- lapply(specific_factors, function(f) f$dim)
        specific_decomp <- lapply(specific_factors, function(f) f$status != "frozen")
        specific_fix <- lapply(specific_factors, function(f) f$status == "fixed")
        specific_transpose <- .defaultListFactors_prog(sf_names, FALSE)
        specific_iteration <- .defaultListFactors_prog(sf_names,
            if(!is.null(extras$iteration)) extras$iteration else 100)
    } else {
        # Default specific model (same as defaultCoupledMWCAParams)
        Bnames <- paste0("B",
            seq(sum(unlist(lapply(Xs_ordered, function(x) length(dim(x)))))))
        specific_model <- .defaultSpecificModel(Xs_ordered, block_names, Bnames)
        sf_names <- Bnames
        specific_initial <- .defaultListFactors_prog(sf_names)
        specific_algorithms <- .defaultListFactors_prog(sf_names, "mySVD")
        specific_dims <- .defaultListFactors_prog(sf_names, 2)
        specific_decomp <- .defaultListFactors_prog(sf_names, TRUE)
        specific_fix <- .defaultListFactors_prog(sf_names, FALSE)
        specific_transpose <- .defaultListFactors_prog(sf_names, FALSE)
        specific_iteration <- .defaultListFactors_prog(sf_names, 100)
    }

    # Weights
    weights <- lapply(block_names, function(bname){
        program$blocks[[bname]]$weight
    })
    names(weights) <- block_names

    # Mask
    mask_list <- lapply(block_names, function(x) NULL)
    names(mask_list) <- block_names

    # Ensure model lists are in block_names order (must match Xs order)
    common_model <- common_model[block_names]
    specific_model <- specific_model[block_names]

    new("CoupledMWCAParams",
        Xs=Xs_ordered,
        mask=mask_list,
        pseudocount=if(!is.null(extras$pseudocount)) extras$pseudocount
            else .Machine$double.eps,
        weights=weights,
        common_model=common_model,
        common_initial=common_initial,
        common_algorithms=common_algorithms,
        common_iteration=common_iteration,
        common_decomp=common_decomp,
        common_fix=common_fix,
        common_dims=common_dims,
        common_transpose=common_transpose,
        common_coretype=if(!is.null(extras$common_coretype)) extras$common_coretype
            else "Tucker",
        specific_model=specific_model,
        specific_initial=specific_initial,
        specific_algorithms=specific_algorithms,
        specific_iteration=specific_iteration,
        specific_decomp=specific_decomp,
        specific_fix=specific_fix,
        specific_dims=specific_dims,
        specific_transpose=specific_transpose,
        specific_coretype=if(!is.null(extras$specific_coretype))
            extras$specific_coretype else "Tucker",
        specific=has_specific,
        thr=if(!is.null(extras$thr)) extras$thr else 1e-10,
        viz=if(!is.null(extras$viz)) extras$viz else FALSE,
        figdir=if(!is.null(extras$figdir)) extras$figdir else NULL,
        verbose=if(!is.null(extras$verbose)) extras$verbose else FALSE)
}

.defaultListFactors_prog <- function(names, val=NULL){
    out <- list()
    length(out) <- length(names)
    names(out) <- names
    if(!is.null(val)){
        out[] <- val
    }
    out
}
