.initMWCA <- function(params){
    # mask
    mask <- .initMWCA_mask(params)
    # initial
    initial <- .initMWCA_initial_A(params)
    core <- .initMWCA_initial_S(params, initial, params@transpose)
    # Output
    list(mask=mask, initial=initial, core=core)
}

.initMWCA_mask <- function(params){
    p <- params@mask
    if(is.null(p)){
        p <- params@X
        p[] <- 1
}
    p
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
