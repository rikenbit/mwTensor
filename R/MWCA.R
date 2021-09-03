.checkMWCA_X <- function(params){
    stopifnot(is.array(params@X))
}

.checkMWCA_mask <- function(params){
    if(!is.null(params@mask)){
        stopifnot(is.array(params@mask))
        stopifnot(identical(dim(params@mask), dd))
    }
}

.checkMWCA_dims <- function(params, len_dd, dd, ld){
    stopifnot(len_dd == length(params@dims))
    stopifnot(all(dd - ld >= 0))
}

.checkMWCA_transpose <- function(params){
    stopifnot(is.logical(params@transpose))
}

.checkMWCA_figdir <- function(params){
    if(!is.null(params@figdir)){
        stopifnot(is.character(params@figdir))
    }
}

.checkMWCA_algorithms <- function(params, len_dd){
    stopifnot(len_dd == length(params@algorithms))
    lapply(params@algorithms, function(p){
        stopifnot(length(grep(p, ls(.GlobalEnv))) != 0)
    })
}

.checkMWCA <- function(params){
    # setting
    dd <- dim(params@X)
    len_dd <- length(dim(params@X))
    ld <- params@dims
    # data
    .checkMWCA_X(params)
    # mask
    .checkMWCA_mask(params)
    # dims
    .checkMWCA_dims(params, len_dd, dd, ld)
    # transpose
    .checkMWCA_transpose(params)
    # figdir
    .checkMWCA_figdir(params)
    # algorithms
    .checkMWCA_algorithms(params, len_dd)
}

.initMWCA_mask <- function(params){
    if(is.null(params@mask)){
        mask <- params@X
        mask[] <- 1
    }
    mask
}

.initMWCA_initial_A <- function(params){
    lapply(seq_along(dim(params@X)), function(i){
        l1 <- dim(params@X)[i]
        l2 <- params@dims[i]
        if(params@decomp[i]){
            t(.randMat(l1, l2))
        }else{
            t(.unitMat(l1, l2))
        }
    })
}

.initMWCA_initial_S <- function(params, initial, transpose){
    .Projection(params@X, initial, transpose=transpose)
}

.initMWCA <- function(params){
    # mask
    mask <- .initMWCA_mask(params)
    # initial
    initial <- .initMWCA_initial_A(params)
    core <- .initMWCA_initial_S(params, initial, params@transpose)
    # Output
    list(mask=mask, initial=initial, core=core)
}

.MWCA <- function(params){
    # Argument Check
    .checkMWCA(params)
    # Initialization
    int <- .initMWCA(params)
    # Setting
    X <- as.tensor(params@X)
    M <- as.tensor(int$mask)
    A <- int$initial
    S <- int$core
    algorithms <- params@algorithms
    f <- lapply(algorithms, function(a){eval(parse(text=a))})
    decomp <- params@decomp
    fix <- params@fix
    dims <- params@dims
    transpose <- params@transpose
    viz <- params@viz
    figdir <- params@figdir
    # Update Factor Matrices
    for(n in seq_along(dim(X))){
        if(!fix[n] && decomp[n]){
            Xn <- t(cs_unfold(X, m = n)@data)
            Mn <- t(cs_unfold(M, m = n)@data)
            A[[n]] <- t(f[[n]](Xn*Mn, dims[n]))
        }
    }
    # Update Core Tensor
    S <- .Projection(X@data, A, transpose=transpose)
    # After Update
    X_bar <- recTensor(S=S, A=A)
    rec_error <- .recError(X, X_bar)
    train_error <- .recError(M*X, M*X_bar)
    test_error <- .recError((1-M)*X, (1-M)*X_bar)
    # Visualization
    if(viz){
        if(.ndim(X) != 3){
            message("Only three-order tensor can be visualized")
        }else{
            if(is.null(figdir)){
                layout(t(1:2))
                plotTensor3D(X)
                plotTensor3D(X_bar)
            }else{
                png(filename = paste0(figdir, "/original.png"))
                plotTensor3D(X)
                dev.off()
                png(filename = paste0(figdir, "/finish.png"))
                plotTensor3D(X_bar)
                dev.off()
            }
        }
    }
    # Output
    return(new("MWCAResult",
            algorithms=algorithms,
            decomp=decomp,
            dims=dims,
            transpose=transpose,
            viz=viz,
            figdir=figdir,
            factors=A,
            core=S@data,
            rec_error=rec_error,
            train_error=train_error,
            test_error=test_error))
}
