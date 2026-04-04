#' Static Validation of CoupledMWCA Parameters
#'
#' Validates a \code{CoupledMWCAParams} object without running optimization.
#' Collects all errors and warnings instead of stopping on the first failure.
#'
#' @param params A \code{CoupledMWCAParams} object.
#' @return A list with components:
#' \describe{
#'   \item{ok}{Logical. \code{TRUE} if no errors were found.}
#'   \item{errors}{Character vector of error messages (empty if \code{ok} is \code{TRUE}).}
#'   \item{warnings}{Character vector of warning messages.}
#'   \item{normalized_params}{The input \code{params} object if valid, or \code{NULL} if errors exist.}
#'   \item{summary}{A single human-readable summary string.}
#' }
#' @examples
#' Xs <- toyModel("coupled_CP_Easy")
#' common_model <- list(
#'     X1=list(I1="A1", I2="A2"),
#'     X2=list(I2="A2", I3="A3", I4="A4"),
#'     X3=list(I4="A4", I5="A5"))
#' params <- defaultCoupledMWCAParams(Xs, common_model)
#' result <- checkCoupledMWCA(params)
#' result$ok
setMethod("checkCoupledMWCA",
    signature(params="CoupledMWCAParams"),
    function(params){
        .checkCoupledMWCA_static(params)
    })

.checkCoupledMWCA_static <- function(params){
    errors <- character(0)
    warnings <- character(0)

    # Helper: run a check, capture stop() as an error message
    .capture <- function(expr){
        tryCatch(expr, error = function(e){
            errors <<- c(errors, conditionMessage(e))
        })
    }

    # -------------------------------------------------------
    # 1. Basic structural checks
    # -------------------------------------------------------
    if(!is.list(params@Xs) || length(params@Xs) == 0){
        errors <- c(errors, "params@Xs must be a non-empty list")
    } else if(is.null(names(params@Xs))){
        errors <- c(errors, "params@Xs must be a named list")
    }
    if(!is.list(params@common_model) || length(params@common_model) == 0){
        errors <- c(errors, "params@common_model must be a non-empty list")
    }
    if(length(errors) > 0){
        return(.makeCheckResult(errors, warnings, NULL))
    }

    # -------------------------------------------------------
    # 2. Data-level checks (each sub-check individually)
    # -------------------------------------------------------
    XsSizes <- lapply(params@Xs, function(p){dim(p)})
    Isnames <- unlist(lapply(params@common_model, names))
    uniqIsnames <- unique(Isnames)
    IsSizes_other <- .IsSizes(XsSizes, Isnames)

    .capture(.checkCoupledMWCA_OptionStructure(params))
    .capture(.checkCoupledMWCA_Xs(params, uniqIsnames, Isnames, XsSizes))
    .capture(.checkCoupledMWCA_mask(params, XsSizes))
    .capture(.checkCoupledMWCA_weights(params))
    .capture(.checkCoupledMWCA_figdir(params))
    .capture(.checkCoupledMWCA_thr(params))

    # -------------------------------------------------------
    # 3. Common factor checks (each sub-check individually)
    # -------------------------------------------------------
    common_Xsnames <- names(params@common_model)
    common_Isnames <- unlist(lapply(params@common_model, names))
    uniq_common_Isnames <- unique(common_Isnames)
    IsSizes_common <- .IsSizes(XsSizes, common_Isnames)
    common_Asnames <- unlist(lapply(params@common_model, unlist))
    uniq_common_Asnames <- unique(common_Asnames)
    common_Dims <- unlist(lapply(uniq_common_Asnames,
        function(x){params@common_dims[[x]]}))
    common_As_Is <- cbind(common_Asnames, common_Isnames)

    .capture(.checkCoupledMWCA_OptionStructure_common(params))
    .capture(.checkCoupledMWCA_model_common(params, common_Xsnames,
        common_Isnames, uniq_common_Isnames, common_Asnames, common_As_Is))

    # Build info data.frame (may fail if names mismatch, so guard it)
    common_As_Is_Dims_IsSizes <- tryCatch(
        data.frame(uniq_common_Asnames, uniq_common_Isnames,
            common_Dims, IsSizes_common[uniq_common_Isnames]),
        error = function(e) NULL)

    if(!is.null(common_As_Is_Dims_IsSizes)){
        .capture(.checkCoupledMWCA_initial_common(params, common_As_Is_Dims_IsSizes))
        .capture(.checkCoupledMWCA_dims_common(params, common_As_Is_Dims_IsSizes))
        .capture(.checkCoupledMWCA_ranks_common(params, common_As_Is_Dims_IsSizes))
    }

    .capture(.initMWCA_algorithms_common(params))
    .capture(.checkCoupledMWCA_iteration_common(params))
    .capture(.checkCoupledMWCA_decomp_common(params))
    .capture(.checkCoupledMWCA_fix_common(params))
    .capture(.checkCoupledMWCA_transpose_common(params))
    .capture(.checkCoupledMWCA_coretype_common(params))

    # -------------------------------------------------------
    # 4. Specific factor checks (each sub-check individually)
    # -------------------------------------------------------
    if(params@specific){
        specific_Xsnames <- names(params@specific_model)
        specific_Isnames <- unlist(lapply(params@specific_model, names))
        uniq_specific_Isnames <- unique(specific_Isnames)
        IsSizes_specific <- .IsSizes(XsSizes, specific_Isnames)
        specific_Asnames <- unlist(lapply(params@specific_model, unlist))
        uniq_specific_Asnames <- unique(specific_Asnames)
        specific_Dims <- unlist(lapply(uniq_specific_Asnames,
            function(x){params@specific_dims[[x]]}))
        specific_As_Is <- cbind(specific_Asnames, specific_Isnames)

        .capture(.checkCoupledMWCA_OptionStructure_specific(params))
        .capture(.checkCoupledMWCA_model_specific(params, specific_Xsnames,
            specific_Isnames, uniq_specific_Isnames, specific_Asnames, specific_As_Is))

        specific_As_Is_Dims_IsSizes <- tryCatch(
            data.frame(uniq_specific_Asnames, uniq_specific_Isnames,
                specific_Dims, IsSizes_specific[uniq_specific_Isnames]),
            error = function(e) NULL)

        if(!is.null(specific_As_Is_Dims_IsSizes)){
            .capture(.checkCoupledMWCA_initial_specific(params, specific_As_Is_Dims_IsSizes))
            .capture(.checkCoupledMWCA_dims_specific(params, specific_As_Is_Dims_IsSizes))
            .capture(.checkCoupledMWCA_ranks_specific(params, specific_As_Is_Dims_IsSizes))
        }

        .capture(.initMWCA_algorithms_specific(params))
        .capture(.checkCoupledMWCA_iteration_specific(params))
        .capture(.checkCoupledMWCA_decomp_specific(params))
        .capture(.checkCoupledMWCA_fix_specific(params))
        .capture(.checkCoupledMWCA_transpose_specific(params))
        .capture(.checkCoupledMWCA_coretype_specific(params))
    }

    # -------------------------------------------------------
    # 5. Cross-cutting checks
    # -------------------------------------------------------

    # Common and specific factor names must not overlap
    common_Anames <- tryCatch(
        unique(unlist(lapply(params@common_model, unlist))),
        error = function(e) character(0))
    if(params@specific){
        specific_Anames <- tryCatch(
            unique(unlist(lapply(params@specific_model, unlist))),
            error = function(e) character(0))
        overlap <- intersect(common_Anames, specific_Anames)
        if(length(overlap) > 0){
            errors <- c(errors, paste0(
                "Factor names overlap between common_model and specific_model: ",
                paste(overlap, collapse=", ")))
        }
    }

    # Number of modes in model must match ndim of Xs
    for(xname in names(params@common_model)){
        if(xname %in% names(params@Xs)){
            model_modes <- length(params@common_model[[xname]])
            data_modes <- length(dim(params@Xs[[xname]]))
            if(model_modes != data_modes){
                errors <- c(errors, paste0(
                    "common_model[['", xname, "']] has ", model_modes,
                    " modes but Xs[['", xname, "']] has ", data_modes,
                    " dimensions"))
            }
        }
    }
    if(params@specific){
        for(xname in names(params@specific_model)){
            if(xname %in% names(params@Xs)){
                model_modes <- length(params@specific_model[[xname]])
                data_modes <- length(dim(params@Xs[[xname]]))
                if(model_modes != data_modes){
                    errors <- c(errors, paste0(
                        "specific_model[['", xname, "']] has ", model_modes,
                        " modes but Xs[['", xname, "']] has ", data_modes,
                        " dimensions"))
                }
            }
        }
    }

    # Warn if all factors are fixed
    all_fixed_common <- all(unlist(params@common_fix))
    if(isTRUE(all_fixed_common)){
        if(!params@specific || all(unlist(params@specific_fix))){
            warnings <- c(warnings,
                "All factor matrices are fixed; CoupledMWCA will not update any factors")
        }
    }

    # Warn if all decomp=FALSE
    all_nodecomp_common <- all(!unlist(params@common_decomp))
    if(isTRUE(all_nodecomp_common)){
        if(!params@specific || all(!unlist(params@specific_decomp))){
            warnings <- c(warnings,
                "All decomp flags are FALSE; no matrix factorization will be applied")
        }
    }

    # CP coretype requires equal dims
    if(params@common_coretype == "CP"){
        for(i in seq_along(params@common_model)){
            factor_names <- unlist(params@common_model[[i]])
            if(length(factor_names) >= 2){
                factor_dims <- unlist(lapply(factor_names, function(fn){
                    params@common_dims[[fn]]
                }))
                if(!is.null(factor_dims) && !all(factor_dims == factor_dims[1])){
                    errors <- c(errors, paste0(
                        "CP coretype requires equal dims for all factors in block ",
                        names(params@common_model)[i],
                        " but got dims: ",
                        paste(factor_dims, collapse=", "),
                        " for factors ",
                        paste(factor_names, collapse=", ")))
                }
            }
        }
    }
    if(params@specific && params@specific_coretype == "CP"){
        for(i in seq_along(params@specific_model)){
            factor_names <- unlist(params@specific_model[[i]])
            if(length(factor_names) >= 2){
                factor_dims <- unlist(lapply(factor_names, function(fn){
                    params@specific_dims[[fn]]
                }))
                if(!is.null(factor_dims) && !all(factor_dims == factor_dims[1])){
                    errors <- c(errors, paste0(
                        "CP coretype requires equal dims for all factors in block ",
                        names(params@specific_model)[i],
                        " but got dims: ",
                        paste(factor_dims, collapse=", "),
                        " for factors ",
                        paste(factor_names, collapse=", ")))
                }
            }
        }
    }

    # Build result
    normalized <- if(length(errors) == 0) params else NULL
    .makeCheckResult(errors, warnings, normalized)
}

.makeCheckResult <- function(errors, warnings, normalized_params){
    ok <- length(errors) == 0
    n_err <- length(errors)
    n_warn <- length(warnings)
    if(ok && n_warn == 0){
        summary <- "Validation passed"
    } else if(ok){
        summary <- paste0("Validation passed with ", n_warn, " warning(s)")
    } else {
        summary <- paste0("Validation failed: ", n_err, " error(s), ",
            n_warn, " warning(s)")
    }
    list(ok=ok,
         errors=errors,
         warnings=warnings,
         normalized_params=normalized_params,
         summary=summary)
}
