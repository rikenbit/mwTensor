.checkCoupledMWCA_other <- function(params){
    # Setting (Data)
    XsSizes <- lapply(params@Xs, function(p){dim(p)})
    Isnames <- unlist(lapply(params@common_model, names))
    uniqIsnames <- unique(Isnames)
    IsSizes <- .IsSizes(XsSizes, Isnames)
	# Option Structure
	.checkCoupledMWCA_OptionStructure(params)
    # Xs
    .checkCoupledMWCA_Xs(params, uniqIsnames, Isnames, XsSizes)
    # mask
    .checkCoupledMWCA_mask(params, XsSizes)
    # weights
    .checkCoupledMWCA_weights(params)
    # figdir
    .checkCoupledMWCA_figdir(params)
    # thr
    .checkCoupledMWCA_thr(params)
}

.dataItems <- c("Xs", "mask", "weights")
.optionItems <- c("thr", "viz", "figdir", "verbose")
.checkCoupledMWCA_OptionStructure <- function(params){
    # 1. "X1", "X2", "X3"
    datanames <- lapply(.dataItems, function(d){
        eval(parse(text=paste0("names(params@", d, ")")))
    })
    for(i in 2:length(datanames)){
        if(!identical(datanames[[1]], datanames[[i]])){
            msg <- paste0("names(params@", .dataItems[i], ") ",
                "must be the same of names(params@Xs)")
            stop(msg)
        }
    }
    # thr/viz/figdir/verbose
    lapply(.optionItems, function(d){
        objname <- paste0("params@", d)
        obj <- eval(parse(text=objname))
        if(length(obj[[1]]) != 1 && !is.null(obj)){
            msg <- paste0(objname, " must be the length-1 vector or NULL")
            stop(msg)
        }
    })
}

# 4. Xs: Array/Size Check
.checkCoupledMWCA_Xs <- function(params, uniqIsnames, Isnames, XsSizes){
    lapply(seq_along(params@Xs), function(i){
        p <- params@Xs[[i]]
        if(!is.array(p)){
            stop("Each element of params@Xs[[", i, "]] must be a array")
        }
        if(anyNA(p)){
            stop("Remove NA from params@Xs[[", i, "]]")
        }
        if(.anyNaN(p)){
            stop("Remove NaN from params@Xs[[", i, "]]")
        }
        if(.anyInf(p)){
            stop("Remove Inf or -Inf from params@Xs[[", i, "]]")
        }
    })
    dims <- unlist(XsSizes)
    lapply(uniqIsnames, function(u){
        target <- which(Isnames == u)
        if(!.all.equal(dims[target])){
            msg <- paste0("The same I index (e.g. I1) must be ",
                "the same dimension (e.g. 20)")
            stop(msg)
        }
    })
}

# mask: Null/Size Check
.checkCoupledMWCA_mask <- function(params, XsSizes){
    lapply(seq_along(params@mask), function(i){
        p <- params@mask[i][[1]]
        if(!is.null(p)){
            if(!identical(dim(p), XsSizes[[i]])){
                msg <- paste0("dim(params@mask[", i, "]) must be the same as ",
                    "that of dim(params@Xs[", i, "])")
                stop(msg)
            }
            l0 <- length(which(p == 0))
            l1 <- length(which(p == 1))
            if(length(p) != (l0 + l1)){
                stop("Specify the mask tensor with 0 or 1")
            }
        }
    })
}

# weights: Length/Numeric Check
.checkCoupledMWCA_weights <- function(params){
    XsLength <- length(params@Xs) # 3
    if(length(params@weights) != XsLength){
        stop("length(params@weights) must be the same as length(params@Xs)")
    }
    if(!is.numeric(unlist(params@weights))){
        stop("params@weights must be specified as a numerical vector")
    }
}

# figdir: Null/Character Character Check
.checkCoupledMWCA_figdir <- function(params){
    if(!is.null(params@figdir)){
        if(!is.character(params@figdir)){
            stop("params@figdir must be character")
        }
    }
}

# thr: Value Check
.checkCoupledMWCA_thr <- function(params){
    if(params@thr < 0){
        stop("params@thr must be a positive value")
    }
}
