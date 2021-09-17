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

.IsSizes <- function(XsSizes, specific_Isnames){
    out <- unlist(XsSizes)
    names(out) <- specific_Isnames
    out
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
    .checkCoupledMWCA_ranks_matrix(params, specific_As_Is_Dims_IsSizes)
    # Check: Tensor Case 1
    .checkCoupledMWCA_ranks_one(params, specific_As_Is_Dims_IsSizes)
    # Check: Tensor Case 2
    .checkCoupledMWCA_ranks_projected(params, specific_As_Is_Dims_IsSizes)
}

.checkCoupledMWCA_ranks_matrix <- function(params, specific_As_Is_Dims_IsSizes){
    lapply(seq_along(params@specific_model), function(i){
        x <- params@specific_model[[i]]
        if(length(x) == 2){
            target <- unlist(lapply(x, function(xx){
                which(specific_As_Is_Dims_IsSizes$uniq_specific_Asnames == xx)
            }))
            if(!.all.equal(specific_As_Is_Dims_IsSizes$specific_Dims[target])){
                msg <- paste0("params@specific_model[[", i, "]] is a matrix.")
                msg <- paste(c(msg, "In such a case, the lower dimension of ",
                    paste(specific_As_Is_Dims_IsSizes$uniq_specific_Asnames[target], collapse=", ")
                    , "must be the same number in params@specific_dims."), collapse=" ")
                stop(msg)
            }
        }
    })
}

.checkCoupledMWCA_ranks_one <- function(params, specific_As_Is_Dims_IsSizes){
    lapply(seq_along(params@specific_model), function(i){
        x <- params@specific_model[[i]]
        target <- unlist(lapply(x, function(xx){
            which(specific_As_Is_Dims_IsSizes$uniq_specific_Asnames == xx)
        }))
        dim_low <- specific_As_Is_Dims_IsSizes$specific_Dims[target]
        if(1 %in% dim_low){
            not1 <- dim_low[setdiff(seq_along(dim_low), which(dim_low == 1))]
            if(!.all.equal(not1)){
                msg <- paste0(c("The lower dimension 1 is specified",
                    "as at least one of",
                    paste(specific_As_Is_Dims_IsSizes$uniq_specific_Asnames[target], collapse=", ")
                    ), collapse=" ")
                msg <- paste0(msg, " to decompose a ",
                    "higher-order tensor (length(dim(X)) >= 3). ",
                    "In such a case, all the other lower dimensions ",
                    "must be the same number in params@specific_dims.")
                stop(msg)
            }
        }
    })
}

.checkCoupledMWCA_ranks_projected <- function(params, specific_As_Is_Dims_IsSizes){
    lapply(seq_along(params@specific_model), function(i){
        x <- params@specific_model[[i]]
        target <- unlist(lapply(x, function(xx){
            which(specific_As_Is_Dims_IsSizes$uniq_specific_Asnames == xx)
        }))
        dim_high <- specific_As_Is_Dims_IsSizes$IsSizes[target]
        dim_low <- specific_As_Is_Dims_IsSizes$specific_Dims[target]
        dim_idx <- seq_along(dim_low)
        dim_proj <- unlist(lapply(dim_idx, function(d){
            prod(dim_low[setdiff(dim_idx, d)])
        }))
        if(!all(dim_proj >= dim_low)){
            msg <- paste0("After the projection of ",
                names(params@specific_model)[i],
                ", the dimension is smaller than at least one of ",
                paste(specific_As_Is_Dims_IsSizes$uniq_specific_Asnames[target], collapse=", "),
                " in a dimension. Change the lower dimension of ",
                paste(specific_As_Is_Dims_IsSizes$uniq_specific_Asnames[target], collapse=", "),
                " in params@specific_dims.")
            stop(msg)
        }
    })
}

# coretype: Value Check
.checkCoupledMWCA_coretype_specific <- function(params){
    if(params@specific_coretype %ni% c("Tucker", "CP")){
        stop("params@specific_coretype must be 'Tucker' or 'CP'")
    }
}
