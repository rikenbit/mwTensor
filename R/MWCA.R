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
    dims <- params@dims
    transpose <- params@transpose
    viz <- params@viz
    figdir <- params@figdir
    # Update Factor Matrices
    for(n in seq_along(dim(X))){
        Xn <- t(cs_unfold(X, m = n)@data)
        Mn <- t(cs_unfold(M, m = n)@data)
        A[[n]] <- t(f[[n]](Xn*Mn, dims[n]))
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
