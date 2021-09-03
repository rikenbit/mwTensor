.dataItems <- c("Xs", "dimnames", "mask", "weights")
.factorItems <- c("initial", "algorithms", "iteration", "decomp", "fix", "dims")

.checkCoupledMWCA_ListNames <- function(params){
    namesXs <- names(params@Xs)
    # e.g. "X1", "X2", "X3"
    datanames <- lapply(.dataItems, function(d){
        eval(parse(text=paste0("names(params@", d, ")")))
    })
    stopifnot(identical(datanames[[1]], datanames[[2]]))
    stopifnot(identical(datanames[[1]], datanames[[3]]))
    stopifnot(identical(datanames[[1]], datanames[[4]]))
    # e.g. "A1", "A2", "A3", "A4", "A5"
    factoritems <- lapply(.factorItems, function(d){
        eval(parse(text=paste0("params@", d)))
    })
    lapply(namesXs, function(n){
        stopifnot(identical(names(factoritems[[1]][[n]]),
            names(factoritems[[2]][[n]])))
        stopifnot(identical(names(factoritems[[1]][[n]]),
            names(factoritems[[3]][[n]])))
        stopifnot(identical(names(factoritems[[1]][[n]]),
            names(factoritems[[4]][[n]])))
        stopifnot(identical(names(factoritems[[1]][[n]]),
            names(factoritems[[5]][[n]])))
        stopifnot(identical(names(factoritems[[1]][[n]]),
            names(factoritems[[6]][[n]])))
    })
}

.all.equal <- function(x){
    all(x[1] == x)
}

.checkCoupledMWCA_Xs <- function(params){
    lapply(params@Xs, function(p){
        stopifnot(is.array(p))
    })
    dims <- unlist(lapply(params@Xs, function(p){dim(p)}))
    u_dimnames <- unique(unlist(params@dimnames))
    dimnames <- unlist(params@dimnames)
    lapply(u_dimnames, function(u){
        target <- which(dimnames == u)
        stopifnot(.all.equal(dims[target]))
    })
}

.checkCoupledMWCA_dimnames <- function(params){
    lapply(params@dimnames, function(p){
        stopifnot(is.character(p))
    })
}

.checkCoupledMWCA_mask <- function(params, dd){
    lapply(seq_along(params@mask), function(i){
        p <- params@mask[i][[1]]
        if(!is.null(p)){
            stopifnot(identical(dim(p), dd[[i]]))
        }
    })
}

.checkCoupledMWCA_weights <- function(params, len_dd){
    stopifnot(length(params@weights) == len_dd)
}

.checkCoupledMWCA_initial <- function(params, len_dd, dd){
    stopifnot(length(params@initial) == len_dd)
    for(l in seq_along(params@initial)){
        init <- params@initial[[l]]
        for(l2 in seq_along(init)){
            init2 <- init[[l2]]
            if(!is.null(init2)){
                stopifnot(identical(dim(init2),
                    c(params@dims[[l]][[l2]], dd[[l]][[l2]])))
            }
        }
    }
}

.initMWCA_algorithms <- function(params, len_dd, dd){
    stopifnot(length(params@algorithms) == len_dd)
    lapply(seq_along(params@algorithms), function(i){
        p <- params@algorithms[[i]]
        stopifnot(length(p) == length(dd[[i]]))
        grp <- unlist(lapply(p, function(pp){
            grep(pp, ls(.GlobalEnv))
        }))
        stopifnot(all(grp != 0))
    })
}

.checkCoupledMWCA_iteration <- function(params, len_dd){
    stopifnot(length(params@iteration) == len_dd)
    lapply(params@iteration, function(p){
        stopifnot(all(unlist(p) %% 1 == 0))
    })
}

.checkCoupledMWCA_decomp <- function(params, len_dd){
    stopifnot(length(params@decomp) == len_dd)
    lapply(params@decomp, function(p){
        stopifnot(all(is.logical(unlist(p))))
    })
}

.checkCoupledMWCA_fix <- function(parms, len_dd){
    stopifnot(length(params@fix) == len_dd)
    lapply(params@fix, function(p){
        stopifnot(all(is.logical(unlist(p))))
    })
}

.checkCoupledMWCA_dims <- function(params, len_dd, dd){
    stopifnot(length(params@dims) == len_dd)
    lapply(seq_along(params@dims), function(i){
        p <- unlist(params@dims[[i]])
        stopifnot(all(dd[[i]] - p >= 0))
    })
}

.checkCoupledMWCA_transpose <- function(params){
    stopifnot(is.logical(params@transpose))
}

.checkCoupledMWCA_figdir <- function(params){
    if(!is.null(params@figdir)){
        stopifnot(is.character(params@figdir))
    }
}

.checkCoupledMWCA_thr <- function(params){
    stopifnot(params@thr >= 0)
}

.checkCoupledMWCA_ranks <- function(params){
    dims <- params@dims
    lapply(seq_along(dims), function(n){
        dim_x <- dim(params@Xs[[n]])
        dim_l <- unlist(dims[[n]])
        if(length(dim_x) == 2){
            if(dims[[n]][[1]] != dims[[n]][[2]]){
                stop("Factor matrix must has the same rank in row/column dimension")
            }
        }else{
            all_n <- seq_along(dim_x)
            if(1 %in% dim_l){
                not1 <- dim_l[setdiff(all_n, which(dim_l == 1))]
                if(!all(not1[1] == not1)){
                    stop("If a rank is 1, all the other ranks must be the same values.")
                }
            }else{
                dim_proj <- unlist(lapply(all_n, function(an){
                    prod(dim_l[setdiff(all_n, an)])
                }))
                stopifnot(all(dim_proj >= dim_l))
            }
        }
    })
}

.checkCoupledMWCA <- function(params){
    # setting
    dd <- lapply(params@Xs, function(p){
        dim(p)
    })
    len_dd <- length(params@Xs)
    # List Names
    .checkCoupledMWCA_ListNames(params)
    # Xs
    .checkCoupledMWCA_Xs(params)
    # dimnames
    .checkCoupledMWCA_dimnames(params)
    # mask
    .checkCoupledMWCA_mask(params, dd)
    # weights
    .checkCoupledMWCA_weights(params, len_dd)
    # initial
    .checkCoupledMWCA_initial(params, len_dd, dd)
    # algorithms
    .initMWCA_algorithms(params, len_dd, dd)
    # iteration
    .checkCoupledMWCA_iteration(params, len_dd)
    # decomp
    .checkCoupledMWCA_decomp(params, len_dd)
    # fix
    .checkCoupledMWCA_fix(parms, len_dd)
    # dims
    .checkCoupledMWCA_dims(params, len_dd, dd)
    # transpose
    .checkCoupledMWCA_transpose(params)
    # figdir
    .checkCoupledMWCA_figdir(params)
    # rank
    .checkCoupledMWCA_ranks(params)
    # thr
    .checkCoupledMWCA_thr(params)
}

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

.lowDimNames <- function(n){
    paste0("LOW_DIM_", seq(n))
}

.high_low <- function(params){
    ldm <- unique(unlist(params@dimnames))
    high_low <- .lowDimNames(length(ldm))
    names(high_low) <- ldm
    high_low
}

.callFunc <- function(params, i0, j0){
    eval(parse(text=params@algorithms[[i0]][[j0]]))
}

.rbind_list <- function(L){
    nr <- nrow(L[[1]])
    out <- unlist(lapply(L, as.vector))
    dim(out) <- c(nr, length(out)/nr)
    out
}

.ndim <- function(X){
    length(dim(X))
}

.catXs <- function(params, i, j){
    out <- lapply(seq_along(i), function(n){
        i_n <- i[n]
        j_n <- j[n]
        t(cs_unfold(as.tensor(params@Xs[[i_n]]), m = j_n)@data)
    })
    .rbind_list(out)
}

.catXs_ <- function(params, weights, As, i, j, transpose){
    out <- lapply(seq_along(i), function(n){
        i_n <- i[n]
        j_n <- j[n]
        weight <- params@weights[[n]]
        Xn <- params@Xs[[i_n]]
        j_n_ <- setdiff(seq_len(.ndim(Xn)), j_n)
        An_ <- lapply(j_n_, function(jj){As[[i_n]][[jj]]})
        Xn_ <- .Projection(Xn, An_, idx=j_n_, transpose=transpose)@data
        perm <- c(j_n, j_n_)
        Xn_ <- aperm(Xn_, perm)
        weight * t(cs_unfold(as.tensor(Xn_), m = 1)@data)
    })
    .rbind_list(out)
}

.initCoupledMWCA_initial_A <- function(params, high_low){
    initial <- params@initial
    Anames <- unique(unlist(lapply(initial, function(p){names(p)})))
    for(n in seq_along(Anames)){
        if(params@verbose){
            cat(paste0(n, " / ", length(Anames), "\n"))
        }
        idx <- .searchFactor(initial, Anames[[n]])
        # Data index
        i <- idx$i
        # Factor index
        j <- idx$j
        # First Factor Matrix index
        i0 <- idx$i[1]
        j0 <- idx$j[1]
        # Size of each Factor matrix
        l1 <- dim(params@Xs[[i0]])[[j0]]
        l2 <- params@dims[[i0]][[j0]]
        if(params@decomp[[i0]][[j0]]){
            # Decompotision method
            f <- .callFunc(params, i0, j0)
            Xns <- .catXs(params, i, j)
            A0 <- t(f(Xns, k=l2))
        }else{
            A0 <- t(.unitMat(l1, l2))
        }
        # Substitute each Factor
        for(k in seq_along(i)){
            ii <- i[k]
            jj <- j[k]
            initial[[ii]][[jj]] <- A0
        }
    }
    initial
}

.Projections <- function(Xs, A, transpose){
    lapply(seq_along(Xs), function(i){
        .Projection(Xs[[i]], A[[i]], transpose=transpose)
    })
}

.evalFunction <- function(params, algorithms){
    lapply(seq_along(params@decomp), function(i){
        out <- c()
        for(j in seq_along(params@decomp[[i]])){
            if(params@decomp[[i]][[j]]){
                a <- algorithms[[i]][[j]]
                out[j] <- lapply(a, function(aa){
                    eval(parse(text=aa))})
            }else{
                out[j] <- ""
            }
        }
        out
    })
}

.initCoupledMWCA <- function(params){
    # mask
    mask <- .initCoupledMWCA_mask(params)
    # initial & dimnames
    high_low <- .high_low(params)
    if(params@verbose){
        cat("Initialization step (Factor Matrices: As)...\n")
    }
    initial <- .initCoupledMWCA_initial_A(params, high_low)
    if(params@verbose){
        cat("Initialization step (Core Tensors: Ss)...\n")
    }
    core <- .Projections(params@Xs, initial, params@transpose)
    # algorithms -> functions
    fs <- .evalFunction(params, params@algorithms)
    # threshold
    rec_error <- params@thr * 10
    train_error <- params@thr * 10
    test_error <- params@thr * 10
    rel_change <- params@thr * 10
    # Output
    list(mask=mask, high_low=high_low,
        initial=initial, core=core, fs=fs,
        rec_error=rec_error, train_error=train_error,
        test_error=test_error, rel_change=rel_change)
}

.CoupledMWCA <- function(params){
    # Argument Check
    .checkCoupledMWCA(params)
    # Initialization
    int <- .initCoupledMWCA(params)
    # Setting
    Xs <- lapply(params@Xs, as.tensor)
    Ms <- lapply(int$mask, as.tensor)
    dimnames <- params@dimnames
    high_low <- int$high_low
    weights <- params@weights
    initial <- int$initial
    As <- int$initial
    Anames <- unique(unlist(lapply(As, function(p){names(p)})))
    Ss <- int$core
    algorithms <- params@algorithms
    fs <- int$fs
    iteration <- params@iteration
    max.iter <- max(unlist(params@iteration))
    decomp <- params@decomp
    fix <- params@fix
    dims <- params@dims
    transpose <- params@transpose
    viz <- params@viz
    figdir <- params@figdir
    thr <- params@thr
    verbose <- params@verbose
    rec_error <- int$rec_error
    train_error <- int$train_error
    test_error <- int$test_error
    rel_change <- int$rel_change
    iter <- 1
    # Iteration
    while ((rec_error[iter] > thr) && (iter <= max.iter)){
        X_bars <- .recTensors(Ss=Ss, As=As)
        pre_Error <- .recErrors(Xs, X_bars)
        # Update Factor Matrices
        for(n in seq_along(Anames)){
            idx <- .searchFactor(As, Anames[[n]])
            # Data index
            i <- idx$i
            # Factor index
            j <- idx$j
            # First Factor Matrix index
            i0 <- idx$i[1]
            j0 <- idx$j[1]
            # Size of each Factor matrix
            l2 <- params@dims[[i0]][[j0]]
            if(params@decomp[[i0]][[j0]] &&
                (params@iteration[[i0]][[j0]] >= iter)){
                # Decompotision methods
                f <- .callFunc(params, i0, j0)
                # Xns <- .catXs(params, i, j)
                Xns <- .catXs_(params, weights, As, i, j, transpose)
                A <- t(f(Xns, k=l2))
                # Substitute each Factor
                for(k in seq_along(i)){
                    ii <- i[k]
                    jj <- j[k]
                    As[[ii]][[jj]] <- A
                }
            }
        }
        # Update Core Tensor
        Ss <- .Projections(params@Xs, As, transpose)
        # After Update
        iter <- iter + 1
        rec_error[iter] <- .recErrors(Xs, X_bars)
        train_error[iter] <- .recErrors(Xs, X_bars, Ms)
        test_error[iter] <- .recErrors(Xs, X_bars, Ms, minus=TRUE)
        rel_change[iter] <- abs(pre_Error - rec_error[iter]) / rec_error[iter]
        # Visualization
        if(viz){
            if(is.null(figdir)){
                plotTensor3Ds(X_bars)
            }else{
                png(filename = paste0(figdir, "/", iter, ".png"),
                    width=1000, height=.figheight(length(X_bars)))
                plotTensor3Ds(X_bars)
                dev.off()
            }
        }
        # Verbose
        if(verbose){
             cat(paste0(iter - 1, " / ", max.iter,
                " |Previous Error - Error| / Error = ",
                rel_change[iter], "\n"))
        }
        if(is.nan(rel_change[iter])){
            stop("NaN is generated. Please run again or change the parameters.\n")
        }
    }
    names(rec_error) <- c("offset", 1:(iter - 1))
    names(train_error) <- c("offset", 1:(iter - 1))
    names(test_error) <- c("offset", 1:(iter - 1))
    names(rel_change) <- c("offset", 1:(iter - 1))
    # Visualization
    if(viz){
        if(is.null(figdir)){
            plotTensor3Ds(X_bars)
        }else{
            png(filename = paste0(figdir, "/original.png"),
                width=1000, height=.figheight(length(X_bars)))
            plotTensor3Ds(Xs)
            dev.off()
            png(filename = paste0(figdir, "/finish.png"),
                width=1000, height=.figheight(length(X_bars)))
            plotTensor3Ds(X_bars)
            dev.off()
        }
    }
    # Output
    return(new("CoupledMWCAResult",
        dimnames=dimnames,
        weights=weights,
        initial=initial,
        algorithms=algorithms,
        decomp=decomp,
        fix=fix,
        dims=dims,
        transpose=transpose,
        viz=viz,
        figdir=figdir,
        verbose=verbose,
        factors=As,
        cores=Ss,
        rec_error=rec_error,
        train_error=train_error,
        test_error=test_error,
        rel_change=rel_change))
}
