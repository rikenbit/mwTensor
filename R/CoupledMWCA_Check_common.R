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

.IsSizes <- function(XsSizes, common_Isnames){
    out <- unlist(XsSizes)
    names(out) <- common_Isnames
    out
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
    lapply(seq_along(params@common_model), function(i){
        x <- params@common_model[[i]]
        if(length(x) == 2){
            target <- unlist(lapply(x, function(xx){
                which(common_As_Is_Dims_IsSizes$uniq_common_Asnames == xx)
            }))
            if(!.all.equal(common_As_Is_Dims_IsSizes$common_Dims[target])){
                msg <- paste0("params@common_model[[", i, "]] is a matrix.")
                msg <- paste(c(msg, "In such a case, the lower dimension of ",
                    paste(common_As_Is_Dims_IsSizes$uniq_common_Asnames[target], collapse=", ")
                    , "must be the same number in params@common_dims."), collapse=" ")
                stop(msg)
            }
        }
    })
}

.checkCoupledMWCA_ranks_one <- function(params, common_As_Is_Dims_IsSizes){
    lapply(seq_along(params@common_model), function(i){
        x <- params@common_model[[i]]
        target <- unlist(lapply(x, function(xx){
            which(common_As_Is_Dims_IsSizes$uniq_common_Asnames == xx)
        }))
        dim_low <- common_As_Is_Dims_IsSizes$common_Dims[target]
        if(1 %in% dim_low){
            not1 <- dim_low[setdiff(seq_along(dim_low), which(dim_low == 1))]
            if(!.all.equal(not1)){
                msg <- paste0(c("The lower dimension 1 is specified",
                    "as at least one of",
                    paste(common_As_Is_Dims_IsSizes$uniq_common_Asnames[target], collapse=", ")
                    ), collapse=" ")
                msg <- paste0(msg, " to decompose a ",
                    "higher-order tensor (length(dim(X)) >= 3). ",
                    "In such a case, all the other lower dimensions ",
                    "must be the same number in params@common_dims.")
                stop(msg)
            }
        }
    })
}

.checkCoupledMWCA_ranks_projected <- function(params, common_As_Is_Dims_IsSizes){
    lapply(seq_along(params@common_model), function(i){
        x <- params@common_model[[i]]
        target <- unlist(lapply(x, function(xx){
            which(common_As_Is_Dims_IsSizes$uniq_common_Asnames == xx)
        }))
        dim_high <- common_As_Is_Dims_IsSizes$IsSizes[target]
        dim_low <- common_As_Is_Dims_IsSizes$common_Dims[target]
        dim_idx <- seq_along(dim_low)
        dim_proj <- unlist(lapply(dim_idx, function(d){
            prod(dim_low[setdiff(dim_idx, d)])
        }))
        if(!all(dim_proj >= dim_low)){
            msg <- paste0("After the projection of ",
                names(params@common_model)[i],
                ", the dimension is smaller than at least one of ",
                paste(common_As_Is_Dims_IsSizes$uniq_common_Asnames[target], collapse=", "),
                " in a dimension. Change the lower dimension of ",
                paste(common_As_Is_Dims_IsSizes$uniq_common_Asnames[target], collapse=", "),
                " in params@common_dims.")
            stop(msg)
        }
    })
}

# coretype: Value Check
.checkCoupledMWCA_coretype_common <- function(params){
    if(params@common_coretype %ni% c("Tucker", "CP")){
        stop("params@common_coretype must be 'Tucker' or 'CP'")
    }
}
