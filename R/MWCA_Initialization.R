.initMWCA <- function(params){
    X <- params@X
    # NA mask
    M_NA <- X
    M_NA[] <- 1
    M_NA[which(is.na(X))] <- 0
    if(is.null(params@mask)){
        M <- M_NA
    }else{
        M <- params@mask
    }
    pM <- M
    # Pseudo count
    X[which(is.na(X))] <- params@pseudocount
    X[which(X == 0)] <- params@pseudocount
    pM[which(pM == 0)] <- params@pseudocount
    # initial
    A <- .initMWCA_initial_A(params)
    S <- .initMWCA_initial_S(params, A, params@transpose)
    # Output
    list(X=X, M=M, pM=pM, M_NA=M_NA, A=A, S=S)
}

.initMWCA_initial_A <- function(params){
    lapply(seq_along(dim(params@X)), function(i){
        l1 <- dim(params@X)[i]
        l2 <- params@dims[i]
        t(.randMat(l1, l2))
    })
}

.initMWCA_initial_S <- function(params, initial, transpose){
    .Projection(params@X, initial, transpose=transpose)
}
