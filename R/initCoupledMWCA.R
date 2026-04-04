#' Initialize CoupledMWCA Factors and Cores
#'
#' Generates normalized, reproducible initial values for a CoupledMWCA problem
#' without running iterative optimization. This is a companion to
#' \code{\link{checkCoupledMWCA}} and can be used to inspect or store
#' initial states before calling \code{\link{CoupledMWCA}}.
#'
#' @param params A \code{CoupledMWCAParams} object.
#' @param seed An optional integer seed for reproducibility. When \code{NULL}
#'   (default), no seed management is performed.
#' @param init_policy Character string specifying the initialization strategy.
#'   One of \code{"random"} (default), \code{"svd"}, or \code{"nonneg_random"}.
#' @return A \code{CoupledMWCAInit} object containing:
#' \describe{
#'   \item{params}{The input \code{CoupledMWCAParams}.}
#'   \item{common_factors}{Named list of initialized common factor matrices.}
#'   \item{common_cores}{List of initialized common core tensors.}
#'   \item{specific_factors}{Named list of initialized specific factor matrices
#'     (or \code{list(NULL)} when \code{specific=FALSE}).}
#'   \item{specific_cores}{List of initialized specific core tensors
#'     (or \code{list(NULL)} when \code{specific=FALSE}).}
#'   \item{init_policy}{The policy used.}
#'   \item{seed}{The seed used (or \code{NULL}).}
#' }
#' @examples
#' Xs <- toyModel("coupled_CP_Easy")
#' common_model <- list(
#'     X1=list(I1="A1", I2="A2"),
#'     X2=list(I2="A2", I3="A3", I4="A4"),
#'     X3=list(I4="A4", I5="A5"))
#' params <- defaultCoupledMWCAParams(Xs, common_model)
#' init <- initCoupledMWCA(params, seed=42L)
#' @export
setMethod("initCoupledMWCA",
    signature(params="CoupledMWCAParams"),
    function(params, seed=NULL, init_policy="random"){
        .initCoupledMWCA_public(params, seed, init_policy)
    })

.initCoupledMWCA_public <- function(params, seed, init_policy){
    # Validate init_policy
    valid_policies <- c("random", "svd", "nonneg_random")
    if(!(init_policy %in% valid_policies)){
        stop(paste0("init_policy must be one of: ",
            paste(valid_policies, collapse=", ")))
    }

    # Seed management
    old_seed <- NULL
    if(!is.null(seed)){
        if(exists(".Random.seed", envir=.GlobalEnv)){
            old_seed <- get(".Random.seed", envir=.GlobalEnv)
        }
        set.seed(seed)
        on.exit({
            if(is.null(old_seed)){
                rm(".Random.seed", envir=.GlobalEnv)
            } else {
                assign(".Random.seed", old_seed, envir=.GlobalEnv)
            }
        })
    }

    # Prepare masked data (same as existing .initCoupledMWCA)
    Xs <- lapply(params@Xs, as.tensor)
    mask <- .initCoupledMWCA_mask(params)
    Ms <- list()
    M_NAs <- Xs
    length(Ms) <- length(Xs)
    for(i in seq_along(M_NAs)){
        M_NAs[[i]]@data[] <- 1
        M_NAs[[i]]@data[which(is.na(Xs[[i]]@data))] <- 0
        if(is.null(params@mask[[i]])){
            Ms[[i]] <- M_NAs[[i]]@data
        } else {
            Ms[[i]] <- mask[[i]]
        }
    }
    pMs <- Ms
    for(i in seq_along(Xs)){
        Xs[[i]]@data[which(is.na(Xs[[i]]@data))] <- params@pseudocount
        Xs[[i]]@data[which(Xs[[i]]@data == 0)] <- params@pseudocount
        pMs[[i]][which(pMs[[i]] == 0)] <- params@pseudocount
    }
    MaskedXs <- .multiplyList(Xs, pMs)

    # Initialize common factors
    common_As <- .initFactors_policy(
        params=params,
        MaskedXs=MaskedXs,
        model=params@common_model,
        initial=params@common_initial,
        algorithms=params@common_algorithms,
        decomp=params@common_decomp,
        dims=params@common_dims,
        init_policy=init_policy)

    # Initialize common cores
    common_Ss <- .Projections(MaskedXs, common_As,
        params@common_model, params@common_transpose,
        params@common_coretype)

    # Initialize specific factors/cores
    if(params@specific){
        X_not_bars <- .subtractList(MaskedXs,
            .recTensors(common_Ss, common_As, params@common_model))
        specific_As <- .initFactors_policy(
            params=params,
            MaskedXs=X_not_bars,
            model=params@specific_model,
            initial=params@specific_initial,
            algorithms=params@specific_algorithms,
            decomp=params@specific_decomp,
            dims=params@specific_dims,
            init_policy=init_policy)
        specific_Ss <- .Projections(X_not_bars, specific_As,
            params@specific_model, params@specific_transpose,
            params@specific_coretype)
    } else {
        specific_As <- list(NULL)
        specific_Ss <- list(NULL)
    }

    new("CoupledMWCAInit",
        params=params,
        common_factors=common_As,
        common_cores=common_Ss,
        specific_factors=specific_As,
        specific_cores=specific_Ss,
        init_policy=init_policy,
        seed=seed)
}

# Factor initialization with policy selection
.initFactors_policy <- function(params, MaskedXs, model, initial,
                                algorithms, decomp, dims, init_policy){
    Anames <- unique(names(initial))
    out <- initial
    for(n in seq_along(Anames)){
        Aname <- Anames[n]
        idx <- .searchFactor(model, Aname)
        i <- idx$i
        j <- idx$j
        i0 <- i[1]
        j0 <- j[1]
        dim1 <- dim(MaskedXs[[i0]])[j0]
        dim2 <- dims[[n]]
        if(decomp[[n]]){
            if(!is.null(initial[[n]])){
                # User-supplied initial value: use as-is
                out[[n]] <- initial[[n]]
            } else {
                # Generate initial value based on policy
                Xn <- .catXs(MaskedXs, i, j)
                out[[n]] <- .initFactor_by_policy(
                    Xn, dim1, dim2, init_policy, algorithms[[Aname]])
            }
        } else {
            # No decomposition: identity-like matrix
            out[[n]] <- t(.unitMat(dim1, dim2))
        }
    }
    out
}

# Dispatch to specific init policy
.initFactor_by_policy <- function(Xn, dim1, dim2, init_policy, algorithm_name){
    if(init_policy == "random"){
        t(.randMat(dim1, dim2))
    } else if(init_policy == "svd"){
        .initFactor_svd(Xn, dim2)
    } else if(init_policy == "nonneg_random"){
        .initFactor_nonneg_random(dim1, dim2)
    } else {
        stop(paste0("Unknown init_policy: ", init_policy))
    }
}

# SVD-based initialization
# Xn has nrow=dim1 (mode dimension), ncol=concatenated other dimensions
# We want the top-k left singular vectors as rows of the factor matrix (k x dim1)
.initFactor_svd <- function(Xn, k){
    nk <- min(k, min(dim(Xn)))
    s <- svd(Xn, nu=nk, nv=0)
    U <- s$u[, seq_len(nk), drop=FALSE]
    # If k > available singular vectors, pad with random columns
    if(ncol(U) < k){
        pad <- matrix(runif(nrow(U) * (k - ncol(U))),
            nrow=nrow(U), ncol=k - ncol(U))
        U <- cbind(U, pad)
    }
    t(U)
}

# Non-negative random initialization
.initFactor_nonneg_random <- function(dim1, dim2){
    mat <- matrix(abs(runif(dim1 * dim2)), nrow=dim1, ncol=dim2)
    mat <- .normMat(mat, "row")
    t(mat)
}

# Inject CoupledMWCAInit factors into params@common_initial/specific_initial
# so that the existing .initCoupledMWCA uses them as starting values.
.injectInit <- function(init){
    params <- init@params
    # Inject common factors
    if(!identical(init@common_factors, list(NULL))){
        params@common_initial <- init@common_factors
    }
    # Inject specific factors
    if(params@specific && !identical(init@specific_factors, list(NULL))){
        params@specific_initial <- init@specific_factors
    }
    params
}
