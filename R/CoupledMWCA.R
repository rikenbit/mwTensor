.CoupledMWCA <- function(params){
    # Argument Check
    .checkCoupledMWCA_common(params)
    if(params@specific){
        .checkCoupledMWCA_specific(params)
        # Iteration Setting
        max.iter <- max(c(unlist(params@common_iteration),
            unlist(params@specific_iteration)))
    }else{
        # Iteration Setting
        max.iter <- max(unlist(params@common_iteration))
    }
    .checkCoupledMWCA_other(params)
    # Initialization
    int <- .initCoupledMWCA(params)
    # 3. Common Factor matrix-wise setting
    common_initial <- int$common_As
    common_Anames <- names(int$common_As)
    # 5. Specific Factor matrix-wise setting
    specific_initial <- int$specific_As
    specific_Anames <- names(int$specific_As)
    # Visualization
    if(params@viz){
        X_bars <- .recTensors(Ss=int$common_Ss, As=int$common_As,
            params@common_model)
        if(is.null(params@figdir)){
            plotTensor3Ds(X_bars)
        }else{
            png(filename = paste0(params@figdir, "/0.png"),
                width=2500, height=.figheight(length(X_bars)))
            plotTensor3Ds(X_bars)
            dev.off()
        }
    }
    # Iteration
    iter <- 1
    while ((int$rec_error[iter] > params@thr) && (iter <= max.iter)){
        X_not_tildes <- .subtractList(int$MaskedXs, int$X_tildes)
        # Update Common Factor Matrices
        for(n in seq_along(common_Anames)){
            if(!params@common_fix[[n]] && params@common_decomp[[n]] &&
                (params@common_iteration[[n]] >= iter)){
                if(params@verbose){
                    cat(paste0(n, " / ", length(common_Anames),
                        " Common factor matricies are being updated\r"))
                }
                int$common_As[[n]] <- .update_As(common_Anames,
                    params@common_model, X_not_tildes, params@weights,
                    int$common_As, params@common_transpose,
                    params@common_dims, n, params@common_coretype,
                    int$common_fs)
            }
        }
        # Update Common Core Tensor
        int$common_Ss <- .Projections(X_not_tildes,
            int$common_As, params@common_model, params@common_transpose,
            params@common_coretype)
        X_bars <- .recTensors(Ss=int$common_Ss, As=int$common_As,
            params@common_model)
        if(params@specific){
            X_not_bars <- .subtractList(int$MaskedXs, X_bars)
            # Update Specific Factor Matrices
            for(n in seq_along(specific_Anames)){
                if(!params@specific_fix[[n]] && params@specific_decomp[[n]] &&
                    (params@specific_iteration[[n]] >= iter)){
                    if(params@verbose){
                        cat(paste0(n, " / ", length(specific_Anames),
                            " Specific factor matricies are being updated\r"))
                    }
                    int$specific_As[[n]] <- .update_As(specific_Anames,
                        params@specific_model, X_not_bars, params@weights,
                        int$specific_As, params@specific_transpose,
                        params@specific_dims, n, params@specific_coretype,
                        int$specific_fs)
                }
            }
            # Update Specific Core Tensor
            int$specific_Ss <- .Projections(X_not_bars, int$specific_As,
                params@specific_model, params@specific_transpose,
                params@specific_coretype)
            int$X_tildes <- .recTensors(Ss=int$specific_Ss,
                As=int$specific_As, params@specific_model)
        }
        # Visualization
        if(params@viz){
            if(is.null(params@figdir)){
                plotTensor3Ds(X_bars)
            }else{
                png(filename = paste0(params@figdir, "/", iter, ".png"),
                    width=2500, height=.figheight(length(X_bars)))
                plotTensor3Ds(X_bars)
                dev.off()
            }
        }
        # Verbose
        if(params@verbose){
             cat(paste0(iter, " / ", max.iter,
                " |Previous Error - Error| / Error = ",
                int$rel_change[iter], "\n"))
        }
        if(is.nan(int$rel_change[iter])){
            stop("NaN is generated. Please run again or change the parameters.\n")
        }
        # After Update
        iter <- iter + 1
        int$rec_error[iter] <- .recErrors(int$MaskedXs, X_bars) +
            .recErrors(int$MaskedXs, int$X_tildes)
        int$train_error[iter] <- .recErrors(int$MaskedXs, X_bars, int$Ms) +
            .recErrors(int$MaskedXs, int$X_tildes, int$Ms)
        int$test_error[iter] <- .recErrors(int$MaskedXs, X_bars, int$Ms, minus=TRUE) +
            .recErrors(int$MaskedXs, int$X_tildes, int$Ms, minus=TRUE)
        int$rel_change[iter] <- abs(int$rec_error[iter-1] - int$rec_error[iter]) /
            int$rec_error[iter]
    }
    names(int$rec_error) <- c("offset",
        seq_len(length(int$rec_error) - 1))
    names(int$train_error) <- c("offset",
        seq_len(length(int$train_error) - 1))
    names(int$test_error) <- c("offset",
        seq_len(length(int$test_error) - 1))
    names(int$rel_change) <- c("offset",
        seq_len(length(int$rel_change) - 1))
    # Visualization
    if(params@viz){
        if(is.null(params@figdir)){
            plotTensor3Ds(X_bars)
        }else{
            png(filename = paste0(params@figdir, "/original.png"),
                width=2500, height=.figheight(length(X_bars)))
            plotTensor3Ds(int$MaskedXs)
            dev.off()
            png(filename = paste0(params@figdir, "/finish.png"),
                width=2500, height=.figheight(length(X_bars)))
            plotTensor3Ds(X_bars)
            dev.off()
        }
    }
    # Output
    return(new("CoupledMWCAResult",
        # Data-wise setting
        weights=params@weights,
        # Common Factor Matrices
        common_model=params@common_model,
        common_initial=common_initial,
        common_algorithms=params@common_algorithms,
        common_iteration=params@common_iteration,
        common_decomp=params@common_decomp,
        common_fix=params@common_fix,
        common_dims=params@common_dims,
        common_transpose=params@common_transpose,
        common_coretype=params@common_coretype,
        common_factors=int$common_As,
        common_cores=int$common_Ss,
        # Specific Factor Matrices
        specific_model=params@specific_model,
        specific_initial=specific_initial,
        specific_algorithms=params@specific_algorithms,
        specific_iteration=params@specific_iteration,
        specific_decomp=params@specific_decomp,
        specific_fix=params@specific_fix,
        specific_dims=params@specific_dims,
        specific_transpose=params@specific_transpose,
        specific_coretype=params@specific_coretype,
        specific_factors=int$specific_As,
        specific_cores=int$specific_Ss,
        # Other option
        specific=params@specific,
        thr=params@thr,
        viz=params@viz,
        figdir=params@figdir,
        verbose=params@verbose,
        # Iteration
        rec_error=int$rec_error,
        train_error=int$train_error,
        test_error=int$test_error,
        rel_change=int$rel_change))
}

.update_As <- function(Anames, model, Xs, weights, As, transpose, dims, n, coretype, fs){
    Aname <- Anames[n]
    idx <- .searchFactor(model, Aname)
    # Data index
    i <- idx$i
    # Factor index
    j <- idx$j
    # First Factor Matrix index
    i0 <- idx$i[1]
    j0 <- idx$j[1]
    # Size of each Factor matrix
    if(coretype == "Tucker"){
        A <- .Tucker_ALS(Xs, weights,
            As, model, i, j, transpose)
    }
    if(coretype == "CP"){
        A <- .CP_ALS(Xs, weights,
            As, model, i, j, transpose)
    }
    # Normalization
    A <- .normMat(A)
    # User's Original Matrix Factorization Methods
    f <- fs[[Aname]]
    if(!is.null(f) && ncol(A) > 1){
        A <- t(f(A, k=dims[[n]]))
        # Substitute each Factor
        oldA <- As[[n]]
        .reArrangeRows(A, oldA)
    }else{
        t(A)
    }
}

.reArrangeRows <- function(A, oldA){
    if(nrow(A) == 1){
        A
    }else{
        cor.matrix <- cor(t(A), t(oldA))
        abs.cor.matrix <- abs(cor(t(A), t(oldA)))
        abs.cor.matrix[which(is.na(abs.cor.matrix))] <- 0
        rows <- paste0("Row", seq(nrow(A)))
        rownames(abs.cor.matrix) <- rows
        colnames(abs.cor.matrix) <- seq(nrow(A))
        g <- graph_from_incidence_matrix(abs.cor.matrix, weighted=TRUE)
        index <- as.numeric(as.vector(max_bipartite_match(g)$matching[rows]))
        # Flip sign
        A[index, ] * sign(cor.matrix[cbind(seq(nrow(A)), index)])
    }
}

.ndim <- function(X){
    length(dim(X))
}

.Tucker_ALS <- function(Xs, weights, As, model, i, j, transpose){
    out <- lapply(seq_along(i), function(ii){
        i_n <- i[ii] # Data Index
        j_n <- j[ii] # Factor Matrix Index in X_n
        Xn <- Xs[[i_n]] # X_n
        weight <- weights[[i_n]] # Weight for X_n
        # Non j_n Factor Matrices in X_n
        j_n_ <- setdiff(seq_len(.ndim(Xn)), j_n)
        Anames_ <- unlist(lapply(j_n_, function(x){
            model[[i_n]][[x]]
        }))
        An_ <- lapply(Anames_, function(jj){As[[jj]]}) # Non A_n
        # Non transpose_n
        transpose_ <- lapply(Anames_, function(jj){transpose[[jj]]})
        Xn_ <- .Projection(Xn, An_, idx=j_n_, transpose=transpose_)@data
        perm <- c(j_n, j_n_)
        Xn_ <- aperm(Xn_, perm)
        weight * t(cs_unfold(as.tensor(Xn_), m = 1)@data)
    })
    .rbind_list(out)
}

.CP_ALS <- function(Xs, weights, As, model, i, j, transpose){
    out <- lapply(seq_along(i), function(ii){
        i_n <- i[ii] # Data Index
        j_n <- j[ii] # Factor Matrix Index in X_n
        weight <- weights[[i_n]]
        Xn <- Xs[[i_n]]
        # Non j_n Factor Matrices in X_n
        j_n_ <- setdiff(seq_len(.ndim(Xn)), j_n)
        Anames_ <- unlist(lapply(j_n_, function(x){
            model[[i_n]][[x]]
        }))
        An_ <- lapply(Anames_, function(jj){t(As[[jj]])}) # Non A_n
        if(length(An_) != 1){
            An_ <- ginv(khatri_rao_list(An_))
        }else{
            An_ <- ginv(An_[[1]])
        }
        perm <- c(j_n, j_n_)
        Xn <- aperm(Xn@data, perm)
        Xn <- weight * cs_unfold(as.tensor(Xn), m = 1)@data
        # Non transpose_n
        t(Xn) %*% t(An_)
    })
    if(length(out) == 1){
        out[[1]]
    }else{
        .sum_list(out)
    }
}

.recErrors <- function(Xs, Ys, Ms=NULL, minus=FALSE){
    if(is.null(Ms)){
        out <- lapply(seq_along(Xs), function(i){
            .recError(Xs[[i]], Ys[[i]])
        })
    }else{
        out <- lapply(seq_along(Xs), function(i){
            M <- Ms[[i]]
            if(minus){
                M <- 1 - M
            }
            .recError(M*Xs[[i]], M*Ys[[i]])
        })
    }
    sum(unlist(out))
}
