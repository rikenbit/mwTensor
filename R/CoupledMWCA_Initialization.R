.initCoupledMWCA <- function(params){
    # mask
    mask <- .initCoupledMWCA_mask(params)
    # Data Formatting
    Xs <- lapply(params@Xs, as.tensor)
    Ms <- lapply(mask, as.tensor)
    MaskedXs <- .multiplyList(Xs, Ms)
    #
    # Common Objects
    #
    # algorithms -> function
    common_fs <- .initCoupledMWCA_func_common(params)
    # Factor Matrices
    if(params@verbose){
        cat("Initialization step (Common Factor Matrices)...\n")
    }
    common_As <- .initCoupledMWCA_common_As(params, MaskedXs, common_fs)
    if(params@verbose){
        cat("Initialization step (Common Core Tensors)...\n")
    }
    # Cores
    common_Ss <- .Projections(MaskedXs, common_As,
        params@common_model, params@common_transpose,
        params@common_coretype)
    #
    # Specific Objects
    #
    if(params@specific){
        specific_fs <- .initCoupledMWCA_func_specific(params)
        if(params@verbose){
            cat("Initialization step (Specific Factor Matrices)...\n")
        }
        X_not_bars <- .subtractList(MaskedXs,
            .recTensors(common_Ss, common_As, params@common_model))
        specific_As <- .initCoupledMWCA_specific_As(
            params, X_not_bars, specific_fs)
        if(params@verbose){
            cat("Initialization step (Specific Core Tensors)...\n")
        }
        specific_Ss <- .Projections(X_not_bars, specific_As,
            params@specific_model, params@specific_transpose,
            params@specific_coretype)
        X_tildes <- .recTensors(specific_Ss, specific_As,
            params@specific_model)
    }else{
        specific_fs <- list(NULL)
        specific_As <- list(NULL)
        specific_Ss <- list(NULL)
        X_tildes <- .emptyList(params@Xs)
    }
    # 5. threshold
    rec_error <- params@thr * 10
    train_error <- params@thr * 10
    test_error <- params@thr * 10
    rel_change <- params@thr * 10
    # Output
    list(mask=mask,
        common_fs=common_fs, specific_fs=specific_fs,
        common_As=common_As, specific_As=specific_As,
        common_Ss=common_Ss, specific_Ss=specific_Ss,
        X_tildes=X_tildes,
        Xs=Xs, Ms=Ms, MaskedXs=MaskedXs,
        rec_error=rec_error, train_error=train_error,
        test_error=test_error, rel_change=rel_change)
}

# 1. mask
.initCoupledMWCA_mask <- function(params){
    lapply(seq_along(params@mask), function(i){
        p <- params@mask[[i]]
        if(is.null(p)){
            p <- params@Xs[[i]]
            p[] <- 1
        }
        p
    })
}

# 2. algorithms -> function
.initCoupledMWCA_func_common <- function(params){
    lapply(params@common_algorithms, function(p){
        if(!is.null(p)){
            eval(parse(text=p))
        }
    })
}
.initCoupledMWCA_func_specific <- function(params){
    lapply(params@specific_algorithms, function(p){
        if(!is.null(p)){
            eval(parse(text=p))
        }
    })
}

# 3. initial
.rbind_list <- function(L){
    nr <- nrow(L[[1]])
    out <- unlist(lapply(L, as.vector))
    dim(out) <- c(nr, length(out)/nr)
    out
}

.sum_list <- function(L){
    out <- L[[1]]
    for(i in 2:length(L)){
        out <- out + L[[i]]
    }
    out
}

.catXs <- function(Xs, i, j){
    out <- lapply(seq_along(i), function(n){
        i_n <- i[n]
        j_n <- j[n]
        if("Tensor" %in% class(Xs[[i_n]])){
            t(cs_unfold(Xs[[i_n]], m = j_n)@data)
        }else{
            t(cs_unfold(as.tensor(Xs[[i_n]]), m = j_n)@data)
        }
    })
    .rbind_list(out)
}

.initCoupledMWCA_common_As <- function(params, MaskedXs, common_fs){
    init <- params@common_initial
    Anames <- unique(names(init))
    for(n in seq_along(Anames)){
        Aname <- Anames[n]
        if(params@verbose){
            cat(paste0(n, " / ", length(Anames), "\r"))
        }
        idx <- .searchFactor(params@common_model, Anames[n])
        # Data index
        i <- idx$i
        # Factor index
        j <- idx$j
        # First Factor Matrix index
        i0 <- idx$i[1]
        j0 <- idx$j[1]
        # Size of each Factor matrix
        dim1 <- dim(MaskedXs[[i0]])[j0]
        dim2 <- params@common_dims[[n]]
        if(params@common_decomp[[n]]){
            # Update
            if(is.null(init[[n]])){
                # Decompotision method
                f <- common_fs[[Aname]]
                Xn <- .catXs(MaskedXs, i, j)
                tmpA <- t(f(Xn, k=dim2))
            }else{
                # Initial value
                tmpA <- init[[n]]
            }
        }else{
            tmpA <- t(.unitMat(dim1, dim2))
        }
        # Substitute each Factor
        init[[n]] <- tmpA
    }
    init
}

.initCoupledMWCA_specific_As <- function(params, X_not_bars, specific_fs){
    init <- params@specific_initial
    Anames <- unique(names(init))
    for(n in seq_along(Anames)){
        Aname <- Anames[n]
        if(params@verbose){
            cat(paste0(n, " / ", length(Anames), "\r"))
        }
        idx <- .searchFactor(params@specific_model, Anames[n])
        # Data index
        i <- idx$i
        # Factor index
        j <- idx$j
        # Size of each Factor matrix
        dim1 <- dim(X_not_bars[[i]])[j]
        dim2 <- params@specific_dims[[n]]
        if(params@specific_decomp[[n]]){
            # Update
            if(is.null(init[[n]])){
                # Decompotision method
                f <- specific_fs[[Aname]]
                Xn <- .catXs(X_not_bars, i, j)
                tmpA <- t(f(Xn, k=dim2))
            }else{
                # Initial value
                tmpA <- init[[n]]
            }
        }else{
            tmpA <- t(.unitMat(dim1, dim2))
        }
        # Substitute each Factor
        init[[n]] <- tmpA
    }
    init
}
