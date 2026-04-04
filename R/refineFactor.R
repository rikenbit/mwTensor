#' One-Step Factor Refinement (Experimental)
#'
#' Takes a factor matrix from a completed MWCA or CoupledMWCA fit and
#' applies a single additional matrix factorization to decompose it
#' further.
#'
#' @details
#' \strong{Mathematical interpretation:}
#'
#' In MWCA/CoupledMWCA, each factor matrix \code{A} has shape \code{k x n}
#' where \code{k} is the lower dimension and \code{n} is the original
#' observation dimension. Each row of \code{A} is a basis vector in
#' the n-dimensional observation space.
#'
#' \code{refineFactor} treats \code{t(A)} (shape \code{n x k}) as a
#' new observed matrix and decomposes it:
#'
#' \deqn{t(A) \approx U \cdot V}
#'
#' where \code{U} is \code{n x dim} and \code{V} is \code{dim x k}.
#' Equivalently, \code{A \approx t(V) \cdot t(U)}, so the original
#' k basis vectors are approximated by \code{dim} sub-basis vectors.
#'
#' The returned \code{sub_factors} slot holds \code{t(U)} (shape
#' \code{dim x n}): the new lower-rank basis vectors in observation
#' space. The \code{coef} slot holds \code{t(V)} (shape \code{k x dim}):
#' the coefficients expressing the original factor rows in terms of
#' the sub-basis.
#'
#' If the original decomposition was \code{X = S x_m A_m x ...}, then
#' after refinement of \code{A_m}:
#'
#' \deqn{X \approx (S \times_m coef) \times_m sub\_factors \times \ldots}
#'
#' Computing this updated core is left to the caller.
#'
#' This is \strong{not} a full recursive decomposition engine.
#' Only one level of refinement is supported.
#'
#' @param fit An \code{MWCAResult} or \code{CoupledMWCAResult} object.
#' @param factor_name For \code{CoupledMWCAResult}: a character string
#'   naming the factor (e.g. \code{"A2"}). For \code{MWCAResult}: an
#'   integer index into the factors list.
#' @param algorithm Character. One of the built-in decomposition function
#'   names (\code{"mySVD"}, \code{"myALS_SVD"}, \code{"myNMF"},
#'   \code{"myICA"}, \code{"myCX"}) or any function in the global
#'   environment with signature \code{f(Xn, k)}.
#' @param dim Integer. Target lower dimension for the refinement.
#' @return A \code{RefinedFactor} object with slots:
#' \describe{
#'   \item{source_object}{The original fit object.}
#'   \item{source_factor_name}{Character name/index of the extracted factor.}
#'   \item{source_factor}{The original factor matrix A (k x n).}
#'   \item{algorithm}{Algorithm used for refinement.}
#'   \item{dim}{Target dimension used.}
#'   \item{sub_factors}{t(U): the refined sub-basis (dim x n).}
#'   \item{coef}{t(V): coefficients (k x dim) such that
#'     A is approximately coef \%*\% sub_factors.}
#' }
#' @examples
#' if(interactive()){
#'   X <- matrix(runif(20*30), nrow=20, ncol=30)
#'   params <- defaultMWCAParams(X)
#'   params@dims <- c(5L, 5L)
#'   fit <- MWCA(params)
#'   ref <- refineFactor(fit, 1L, algorithm="mySVD", dim=2L)
#'   dim(ref@sub_factors) # 2 x 20
#'   dim(ref@coef)        # 5 x 2
#'   # Verify: source_factor ~ coef %*% sub_factors
#'   max(abs(ref@source_factor - ref@coef \%*\% ref@sub_factors))
#' }
#' @export
refineFactor <- function(fit, factor_name, algorithm="mySVD", dim=2L){
    # --- Extract the source factor matrix ---
    source_factor <- .extractFactor(fit, factor_name)
    factor_label <- as.character(factor_name)

    # --- Resolve algorithm function ---
    # Built-in algorithms are exported from this package. User-supplied
    # algorithm names are resolved via match.fun(), which searches the
    # standard R search path (package namespaces, global environment).
    f <- tryCatch(
        match.fun(algorithm),
        error = function(e){
            stop(paste0(
                "Algorithm '", algorithm, "' is not a built-in or ",
                "accessible function. Built-in algorithms: ",
                "mySVD, myALS_SVD, myNMF, myICA, myCX"),
                call.=FALSE)
        }
    )

    # --- Validate dim ---
    dim <- as.integer(dim)
    if(length(dim) != 1 || dim < 1){
        stop("dim must be a positive integer")
    }

    # source_factor A is k x n.
    # Treat t(A) (n x k) as an observed matrix.
    # f(t(A), dim) returns U (n x dim): the left factor.
    # Then: t(A) ~ U %*% V  where V = ginv(U) %*% t(A), shape dim x k.
    # So:   A ~ t(V) %*% t(U)
    # sub_factors = t(U): dim x n  (new basis vectors)
    # coef        = t(V): k x dim  (coefficients for original rows)
    Xn <- t(source_factor)  # n x k
    if(dim > ncol(Xn)){
        stop(paste0("dim (", dim, ") exceeds the number of columns (",
            ncol(Xn), ") of the transposed source factor"))
    }

    # --- Apply one-step decomposition ---
    U <- f(Xn, dim)                     # n x dim
    sub_factors <- t(U)                  # dim x n
    # Compute coefficient matrix: V = ginv(U) %*% Xn, then coef = t(V)
    V <- ginv(U) %*% Xn                 # dim x k
    coef <- t(V)                         # k x dim

    new("RefinedFactor",
        source_object=fit,
        source_factor_name=factor_label,
        source_factor=source_factor,
        algorithm=algorithm,
        dim=dim,
        sub_factors=sub_factors,
        coef=coef)
}

# --- Factor extraction dispatcher ---
.extractFactor <- function(fit, factor_name){
    if(is(fit, "MWCAResult")){
        .extractFactor_MWCA(fit, factor_name)
    } else if(is(fit, "CoupledMWCAResult")){
        .extractFactor_Coupled(fit, factor_name)
    } else {
        stop("fit must be an MWCAResult or CoupledMWCAResult")
    }
}

.extractFactor_MWCA <- function(fit, factor_name){
    idx <- factor_name
    if(is.character(idx)){
        idx <- as.integer(idx)
    }
    if(!is.numeric(idx) || length(idx) != 1){
        stop("For MWCAResult, factor_name must be an integer index")
    }
    idx <- as.integer(idx)
    if(idx < 1 || idx > length(fit@factors)){
        stop(paste0("Factor index ", idx, " is out of range [1, ",
            length(fit@factors), "]"))
    }
    mat <- fit@factors[[idx]]
    if(!is.matrix(mat)){
        mat <- as.matrix(mat)
    }
    mat
}

.extractFactor_Coupled <- function(fit, factor_name){
    if(!is.character(factor_name) || length(factor_name) != 1){
        stop("For CoupledMWCAResult, factor_name must be a single character string")
    }
    if(factor_name %in% names(fit@common_factors)){
        mat <- fit@common_factors[[factor_name]]
    } else if(factor_name %in% names(fit@specific_factors)){
        mat <- fit@specific_factors[[factor_name]]
    } else {
        all_names <- c(names(fit@common_factors), names(fit@specific_factors))
        stop(paste0("Factor '", factor_name, "' not found. Available: ",
            paste(all_names, collapse=", ")))
    }
    if(!is.matrix(mat)){
        mat <- as.matrix(mat)
    }
    mat
}
