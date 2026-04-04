.checkCoupledMWCA_specific <- function(params){
    # Setting (Data)
    specific_Xsnames <- names(params@specific_model)
    XsSizes <- lapply(params@Xs, function(p){dim(p)})
    specific_Isnames <- unlist(lapply(params@specific_model, names))
    uniq_specific_Isnames <- unique(specific_Isnames)
    IsSizes <- .IsSizes(XsSizes, specific_Isnames)
    # Setting (specific)
    specific_Asnames <- unlist(lapply(params@specific_model, unlist))
    uniq_specific_Asnames <- unique(specific_Asnames)
    specific_Dims <- unlist(lapply(uniq_specific_Asnames,
        function(x){params@specific_dims[[x]]}))
    specific_As_Is <- cbind(specific_Asnames, specific_Isnames)
    # option structure
    .checkCoupledMWCA_OptionStructure_specific(params)
    # model
    .checkCoupledMWCA_model_specific(params, specific_Xsnames, specific_Isnames, uniq_specific_Isnames,
        specific_Asnames, specific_As_Is)
    specific_As_Is_Dims_IsSizes <- data.frame(uniq_specific_Asnames, uniq_specific_Isnames,
        specific_Dims, IsSizes[uniq_specific_Isnames])
    # initial
    .checkCoupledMWCA_initial_specific(params, specific_As_Is_Dims_IsSizes)
    # algorithms
    .initMWCA_algorithms_specific(params)
    # iteration
    .checkCoupledMWCA_iteration_specific(params)
    # decomp
    .checkCoupledMWCA_decomp_specific(params)
    # fix
    .checkCoupledMWCA_fix_specific(params)
    # dims
    .checkCoupledMWCA_dims_specific(params, specific_As_Is_Dims_IsSizes)
    # transpose
    .checkCoupledMWCA_transpose_specific(params)
    # rank
    .checkCoupledMWCA_ranks_specific(params, specific_As_Is_Dims_IsSizes)
    # coretype
    .checkCoupledMWCA_coretype_specific(params)
}

# OptionStructure: List structure Check
.specific_factorItems <- c("specific_initial", "specific_algorithms", "specific_iteration",
    "specific_decomp", "specific_fix", "specific_dims", "specific_transpose")
.checkCoupledMWCA_OptionStructure_specific <- function(params){
    # 2. "A1", "A2", "A3", "A4", "A5"
    factoritems <- lapply(.specific_factorItems, function(d){
        eval(parse(text=paste0("params@", d)))
    })
    for(i in 2:length(factoritems)){
        if(!identical(names(factoritems[[1]]), names(factoritems[[i]]))){
            msg <- paste0("names(params@", .specific_factorItems[i], ") ",
                "must be the same of names(params@specific_initial)")
            stop(msg)
        }
    }
}

# model: Consistency of Xs/Is/As Names Check
.checkCoupledMWCA_model_specific <- function(params, specific_Xsnames,
    specific_Isnames, uniq_specific_Isnames, specific_Asnames, specific_As_Is){
    # Check: Xs names
    if(!identical(specific_Xsnames, names(params@Xs))){
        stop("names(params@specific_model) must be the same of names(params@Xs)")
    }
    # Check: Frequency of I/A
    if(!identical(sort(as.vector(table(specific_Isnames))),
        sort(as.vector(table(specific_Asnames))))){
        msg <- paste0("The number of dimensions and ",
            "the number of lower dimensions are different")
        stop(msg)
    }
    # Check: Same I, Same A
    lapply(uniq_specific_Isnames, function(x){
        target <- which(specific_As_Is[,"specific_Isnames"] == x)
        if(length(unique(specific_As_Is[,"specific_Asnames"][target])) != 1){
            stop("The same I index (I1) has the same A index (e.g. A1)")
        }
    })
}

# initial: Null/Size Check
.checkCoupledMWCA_initial_specific <- function(params, specific_As_Is_Dims_IsSizes){
    for(l in seq_along(params@specific_initial)){
        init <- params@specific_initial[[l]]
        dim_high <- specific_As_Is_Dims_IsSizes$IsSizes[l]
        dim_low <- specific_As_Is_Dims_IsSizes$specific_Dims[l]
        if(!is.null(init)){
            if(dim(init)[1] != dim_low){
                msg <- paste0("dim(params@specific_initial[[", l, "]][1] ",
                    "must be ", dim_low, " as specified in params@specific_model")
                stop(msg)
            }
            if(dim(init)[2] != dim_high){
                msg <- paste0("dim(params@specific_initial[[", l, "]][2] ",
                    "must be ", dim_high, " as specified in params@specific_model")
                stop(msg)
            }
        }
    }
}

# algorithms: Function Exist Check
.initMWCA_algorithms_specific <- function(params){
    lapply(seq_along(params@specific_algorithms), function(i){
        p <- params@specific_algorithms[[i]]
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
.checkCoupledMWCA_iteration_specific <- function(params){
    lapply(params@specific_iteration, function(p){
        if(!all(p %% 1 == 0)){
            stop("params@specific_iteration must be specified as an integer vector")
        }
    })
}

# decomp: Logical Check
.checkCoupledMWCA_decomp_specific <- function(params){
    lapply(params@specific_decomp, function(p){
        if(!all(is.logical(unlist(p)))){
            stop("params@specific_iteration must be specified as a logical vector")
        }
    })
}

# fix: Logical Check
.checkCoupledMWCA_fix_specific <- function(params){
    lapply(params@specific_fix, function(p){
        if(!all(is.logical(unlist(p)))){
            stop("params@specific_iteration must be specified as a logical vector")
        }
    })
}

# dims: Size Check
.checkCoupledMWCA_dims_specific <- function(params, specific_As_Is_Dims_IsSizes){
    for(l in seq_along(params@specific_initial)){
        dim_high <- specific_As_Is_Dims_IsSizes$IsSizes[l]
        dim_low <- specific_As_Is_Dims_IsSizes$specific_Dims[l]
        if(dim_high < dim_low){
            msg <- paste0("At least one too large lower dimension ",
                "was specified in params@specific_model")
            stop(msg)
        }
    }
}

# transpose: Logical Check
.checkCoupledMWCA_transpose_specific <- function(params){
    lapply(params@specific_transpose, function(p){
        if(!all(is.logical(unlist(p)))){
            stop("params@specific_transpose must be specified as a logical vector")
        }
    })
}

# ranks: Mathematically Inpossible Values Check
.checkCoupledMWCA_ranks_specific <- function(params, specific_As_Is_Dims_IsSizes){
    # Check: Matrix Case
    .checkCoupledMWCA_ranks_matrix_specific(params, specific_As_Is_Dims_IsSizes)
    # Check: Tensor Case 1
    .checkCoupledMWCA_ranks_one_specific(params, specific_As_Is_Dims_IsSizes)
    # Check: Tensor Case 2
    .checkCoupledMWCA_ranks_projected_specific(params, specific_As_Is_Dims_IsSizes)
}

.checkCoupledMWCA_ranks_matrix_specific <- function(params, specific_As_Is_Dims_IsSizes){
    info <- data.frame(
        Asnames=specific_As_Is_Dims_IsSizes$uniq_specific_Asnames,
        Dims=specific_As_Is_Dims_IsSizes$specific_Dims,
        IsSizes=specific_As_Is_Dims_IsSizes[,4])
    .checkRanks_matrix(params@specific_model, info)
}

.checkCoupledMWCA_ranks_one_specific <- function(params, specific_As_Is_Dims_IsSizes){
    info <- data.frame(
        Asnames=specific_As_Is_Dims_IsSizes$uniq_specific_Asnames,
        Dims=specific_As_Is_Dims_IsSizes$specific_Dims,
        IsSizes=specific_As_Is_Dims_IsSizes[,4])
    .checkRanks_one(params@specific_model, info)
}

.checkCoupledMWCA_ranks_projected_specific <- function(params, specific_As_Is_Dims_IsSizes){
    info <- data.frame(
        Asnames=specific_As_Is_Dims_IsSizes$uniq_specific_Asnames,
        Dims=specific_As_Is_Dims_IsSizes$specific_Dims,
        IsSizes=specific_As_Is_Dims_IsSizes[,4])
    .checkRanks_projected(params@specific_model, info)
}

# coretype: Value Check
.checkCoupledMWCA_coretype_specific <- function(params){
    if(params@specific_coretype %ni% c("Tucker", "CP")){
        stop("params@specific_coretype must be 'Tucker' or 'CP'")
    }
}
