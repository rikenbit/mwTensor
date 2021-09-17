.checkMWCA <- function(params){
    # setting
    dd <- dim(params@X)
    len_dd <- length(dim(params@X))
    ld <- params@dims
    # data
    .checkMWCA_X(params)
    # mask
    .checkMWCA_mask(params, dd)
    # dims
    .checkMWCA_dims(params, len_dd, dd, ld)
    # figdir
    .checkMWCA_figdir(params)
    # algorithms
    .checkMWCA_algorithms(params, len_dd)
}

.checkMWCA_X <- function(params){
    if(!is.array(params@X)){
        stop("Specify params@X as array")
    }
    if(anyNA(params@X)){
        stop("Remove NA from params@X")
    }
    if(.anyNaN(params@X)){
        stop("Remove NaN from params@X")
    }
    if(.anyInf(params@X)){
        stop("Remove Inf or -Inf from params@X")
    }
}

.checkMWCA_mask <- function(params, dd){
    if(!is.null(params@mask)){
        if(!is.array(params@mask)){
            stop("Specify params@mask as array")
        }
        if(!identical(dim(params@mask), dd)){
            msg <- paste0("The dimension of params@mask must be ",
                "the same as that of params@X")
            stop(msg)
        }
        l0 <- length(which(params@mask == 0))
        l1 <- length(which(params@mask == 1))
        if(length(params@mask) != (l0 + l1)){
            stop("Specify the mask tensor with 0 or 1")
        }
    }
}

.checkMWCA_dims <- function(params, len_dd, dd, ld){
    if(len_dd != length(params@dims)){
        msg <- paste0("length(params@dims) must be the same of ",
            "length(dim(params@X))")
        stop(msg)
    }
    if(!all(dd - ld >= 0)){
        msg <- paste0("All the params@dims must be less than ",
            "the dimension of params@X")
        stop(msg)
    }
}

.checkMWCA_figdir <- function(params){
    if(!is.null(params@figdir)){
        if(!is.character(params@figdir)){
            stop("params@figdir must be character")
        }
    }
}

.checkMWCA_algorithms <- function(params, len_dd){
    if(len_dd != length(params@algorithms)){
        msg <- paste0("length(params@algorithms) must be the same of ",
            "length(dim(params@X))")
        stop(msg)
    }
    lapply(params@algorithms, function(p){
        # Built-in functions
        if(p %ni% c("mySVD", "myALS_SVD", "myNMF", "myICA", "myCX")){
            # User's custom functions
            if(length(grep(p, ls(.GlobalEnv))) == 0){
                msg <- paste0(p, " is not defined in .GlovalEnv")
                stop(msg)
            }
        }
    })
}
