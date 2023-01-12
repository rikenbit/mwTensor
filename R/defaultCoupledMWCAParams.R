defaultCoupledMWCAParams <- function(Xs, common_model){
    stopifnot(is.list(Xs))
    # Setting
    Xnames <- paste0("X", seq_len(length(Xs)))
    Anames <- unique(unlist(common_model))
    Bnames <- paste0("B",
        seq(sum(unlist(lapply(Xs, function(x){length(dim(x))})))))
    specific_model <- .defaultSpecificModel(Xs, Xnames, Bnames)
    # Default Parameters
    new("CoupledMWCAParams",
        # 1. Data-wise setting
        Xs=Xs,
        mask=.defaultListXs(Xs, Xnames),
        weights=.defaultListXs(Xs, Xnames, 1),
        # 2. Common Model setting
        common_model=common_model,
        # 3. Common Factor matrix-wise setting
        common_initial=.defaultListFactors(Anames),
        common_algorithms=.defaultListFactors(Anames, "mySVD"),
        common_iteration=.defaultListFactors(Anames, 100),
        common_decomp=.defaultListFactors(Anames, TRUE),
        common_fix=.defaultListFactors(Anames, FALSE),
        common_dims=.defaultListFactors(Anames, 2),
        common_transpose=.defaultListFactors(Anames, FALSE),
        common_coretype="Tucker",
        # 4. Specific Model setting
        specific_model=specific_model,
        # 5. Specific Factor matrix-wise setting
        specific_initial=.defaultListFactors(Bnames),
        specific_algorithms=.defaultListFactors(Bnames, "mySVD"),
        specific_iteration=.defaultListFactors(Bnames, 100),
        specific_decomp=.defaultListFactors(Bnames, TRUE),
        specific_fix=.defaultListFactors(Bnames, FALSE),
        specific_dims=.defaultListFactors(Bnames, 2),
        specific_transpose=.defaultListFactors(Bnames, FALSE),
        specific_coretype="Tucker",
        # 6. Other option
        specific=FALSE,
        thr=1e-10,
        viz=FALSE,
        figdir=NULL,
        verbose=FALSE)
}

.defaultListXs <- function(Xs, Xnames, val=NULL){
    out <- list()
    length(out) <- length(Xs)
    names(out) <- Xnames
    if(!is.null(val)){
        out[] <- val
    }
    out
}

.defaultListFactors <- function(Anames, val=NULL){
    out <- list()
    length(out) <- length(Anames)
    names(out) <- Anames
    if(!is.null(val)){
        out[] <- val
    }
    out
}

.defaultSpecificModel <- function(Xs, Xnames, Bnames){
    out <- list()
    length(out) <- length(Xs)
    names(out) <- Xnames
    start <- 1
    for(i in seq_along(out)){
        out[[i]] <- .defaultSpecificModel_Small(start, start + length(dim(Xs[[i]])) - 1)
        start <- start + length(dim(Xs[[i]]))
    }
    out
}

.defaultSpecificModel_Small <- function(start, end){
    out <- list()
    length(out) <- end - start + 1
    names(out) <- paste0("J", start:end)
    out[] <- paste0("B", start:end)
    out
}
