.checkCoupledMWCA_common <- function(params){
    # Setting (Data)
    common_Xsnames <- names(params@common_model)
    XsSizes <- lapply(params@Xs, function(p){dim(p)})
    common_Isnames <- unlist(lapply(params@common_model, names))
    uniq_common_Isnames <- unique(common_Isnames)
    IsSizes <- .IsSizes(XsSizes, common_Isnames)
    # Setting (common)
    common_Asnames <- unlist(lapply(params@common_model, unlist))
    uniq_common_Asnames <- unique(common_Asnames)
    common_Dims <- unlist(lapply(uniq_common_Asnames,
        function(x){params@common_dims[[x]]}))
    common_As_Is <- cbind(common_Asnames, common_Isnames)
    # option structure
    .checkCoupledMWCA_OptionStructure_common(params)
    # model
    .checkCoupledMWCA_model_common(params, common_Xsnames, common_Isnames, uniq_common_Isnames,
        common_Asnames, common_As_Is)
    common_As_Is_Dims_IsSizes <- data.frame(uniq_common_Asnames, uniq_common_Isnames,
        common_Dims, IsSizes[uniq_common_Isnames])
    # initial
    .checkCoupledMWCA_initial_common(params, common_As_Is_Dims_IsSizes)
    # algorithms
    .initMWCA_algorithms_common(params)
    # iteration
    .checkCoupledMWCA_iteration_common(params)
    # decomp
    .checkCoupledMWCA_decomp_common(params)
    # fix
    .checkCoupledMWCA_fix_common(params)
    # dims
    .checkCoupledMWCA_dims_common(params, common_As_Is_Dims_IsSizes)
    # transpose
    .checkCoupledMWCA_transpose_common(params)
    # rank
    .checkCoupledMWCA_ranks_common(params, common_As_Is_Dims_IsSizes)
    # coretype
    .checkCoupledMWCA_coretype_common(params)
}

# OptionStructure: List structure Check
.common_factorItems <- c("common_initial", "common_algorithms", "common_iteration",
    "common_decomp", "common_fix", "common_dims", "common_transpose")
.checkCoupledMWCA_OptionStructure_common <- function(params){
    # 2. "A1", "A2", "A3", "A4", "A5"
    factoritems <- lapply(.common_factorItems, function(d){
        eval(parse(text=paste0("params@", d)))
    })
    for(i in 2:length(factoritems)){
        if(!identical(names(factoritems[[1]]), names(factoritems[[i]]))){
            msg <- paste0("names(params@", .common_factorItems[i], ") ",
                "must be the same of names(params@common_initial)")
            stop(msg)
        }
    }
}

# model: Consistency of Xs/Is/As Names Check
.checkCoupledMWCA_model_common <- function(params, common_Xsnames,
    common_Isnames, uniq_common_Isnames, common_Asnames, common_As_Is){
    # Check: Xs names
    if(!identical(common_Xsnames, names(params@Xs))){
        stop("names(params@common_model) must be the same of names(params@Xs)")
    }
    # Check: Frequency of I/A
    if(!identical(sort(as.vector(table(common_Isnames))),
        sort(as.vector(table(common_Asnames))))){
        msg <- paste0("The number of dimensions and ",
            "the number of lower dimensions are different")
        stop(msg)
    }
    # Check: Same I, Same A
    lapply(uniq_common_Isnames, function(x){
        target <- which(common_As_Is[,"common_Isnames"] == x)
        if(length(unique(common_As_Is[,"common_Asnames"][target])) != 1){
            stop("The same I index (I1) has the same A index (e.g. A1)")
        }
    })
}

# initial: Null/Size Check
.checkCoupledMWCA_initial_common <- function(params, common_As_Is_Dims_IsSizes){
    for(l in seq_along(params@common_initial)){
        init <- params@common_initial[[l]]
        dim_high <- common_As_Is_Dims_IsSizes$IsSizes[l]
        dim_low <- common_As_Is_Dims_IsSizes$common_Dims[l]
        if(!is.null(init)){
            if(dim(init)[1] != dim_low){
                msg <- paste0("dim(params@common_initial[[", l, "]][1] ",
                    "must be ", dim_low, " as specified in params@common_model")
                stop(msg)
            }
            if(dim(init)[2] != dim_high){
                msg <- paste0("dim(params@common_initial[[", l, "]][2] ",
                    "must be ", dim_high, " as specified in params@common_model")
                stop(msg)
            }
        }
    }
}

# algorithms: Function Exist Check
.initMWCA_algorithms_common <- function(params){
    lapply(seq_along(params@common_algorithms), function(i){
        p <- params@common_algorithms[[i]]
        if(!is.null(p)){
            # Built-in functions
            if(p %ni% c("mySVD", "myALS_SVD", "myNMF", "myICA", "myCX")){
                # User's custom functions
                if(length(grep(p, ls(.GlobalEnv))) == 0){
                    msg <- paste0(p, " is not defined in .GlovalEnv")
                    stop(msg)
                }
            }
        }
    })
}

# iteration: Integer Check
.checkCoupledMWCA_iteration_common <- function(params){
    lapply(params@common_iteration, function(p){
        if(!all(p %% 1 == 0)){
            stop("params@common_iteration must be specified as an integer vector")
        }
    })
}

# decomp: Logical Check
.checkCoupledMWCA_decomp_common <- function(params){
    lapply(params@common_decomp, function(p){
        if(!all(is.logical(unlist(p)))){
            stop("params@common_iteration must be specified as a logical vector")
        }
    })
}

# fix: Logical Check
.checkCoupledMWCA_fix_common <- function(params){
    lapply(params@common_fix, function(p){
        if(!all(is.logical(unlist(p)))){
            stop("params@common_iteration must be specified as a logical vector")
        }
    })
}

# dims: Size Check
.checkCoupledMWCA_dims_common <- function(params, common_As_Is_Dims_IsSizes){
    for(l in seq_along(params@common_initial)){
        dim_high <- common_As_Is_Dims_IsSizes$IsSizes[l]
        dim_low <- common_As_Is_Dims_IsSizes$common_Dims[l]
        if(dim_high < dim_low){
            msg <- paste0("At least one too large lower dimension ",
                "was specified in params@common_model")
            stop(msg)
        }
    }
}

# transpose: Logical Check
.checkCoupledMWCA_transpose_common <- function(params){
    lapply(params@common_transpose, function(p){
        if(!all(is.logical(unlist(p)))){
            stop("params@common_transpose must be specified as a logical vector")
        }
    })
}

# ranks: Mathematically Inpossible Values Check
.checkCoupledMWCA_ranks_common <- function(params, common_As_Is_Dims_IsSizes){
    # Check: Matrix Case
    .checkCoupledMWCA_ranks_matrix(params, common_As_Is_Dims_IsSizes)
    # Check: Tensor Case 1
    .checkCoupledMWCA_ranks_one(params, common_As_Is_Dims_IsSizes)
    # Check: Tensor Case 2
    .checkCoupledMWCA_ranks_projected(params, common_As_Is_Dims_IsSizes)
}

.checkCoupledMWCA_ranks_matrix <- function(params, common_As_Is_Dims_IsSizes){
    info <- data.frame(
        Asnames=common_As_Is_Dims_IsSizes$uniq_common_Asnames,
        Dims=common_As_Is_Dims_IsSizes$common_Dims,
        IsSizes=common_As_Is_Dims_IsSizes[,4])
    .checkRanks_matrix(params@common_model, info)
}

.checkCoupledMWCA_ranks_one <- function(params, common_As_Is_Dims_IsSizes){
    info <- data.frame(
        Asnames=common_As_Is_Dims_IsSizes$uniq_common_Asnames,
        Dims=common_As_Is_Dims_IsSizes$common_Dims,
        IsSizes=common_As_Is_Dims_IsSizes[,4])
    .checkRanks_one(params@common_model, info)
}

.checkCoupledMWCA_ranks_projected <- function(params, common_As_Is_Dims_IsSizes){
    info <- data.frame(
        Asnames=common_As_Is_Dims_IsSizes$uniq_common_Asnames,
        Dims=common_As_Is_Dims_IsSizes$common_Dims,
        IsSizes=common_As_Is_Dims_IsSizes[,4])
    .checkRanks_projected(params@common_model, info)
}

# coretype: Value Check
.checkCoupledMWCA_coretype_common <- function(params){
    if(params@common_coretype %ni% c("Tucker", "CP")){
        stop("params@common_coretype must be 'Tucker' or 'CP'")
    }
}
