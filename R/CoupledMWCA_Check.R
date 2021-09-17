.checkCoupledMWCA_common <- function(params){
    # 1. Setting
    Xsnames <- names(params@common_model) # X1,X2,X3,...
    XsSizes <- lapply(params@Xs, function(p){dim(p)}) # $X1 30 50, $X2 50 50,...
    Isnames <- unlist(lapply(params@common_model, names)) # I1,I2,I3,...(duplicated)
    uniqIsnames <- unique(Isnames) # I1,I2,I3,...
    IsSizes <- .IsSizes(XsSizes, Isnames) # I1 (30), I2 (50), ...
    common_Asnames <- unlist(lapply(params@common_model, unlist)) # A1,A2,A3,...(duplicated)
    uniq_common_Asnames <- unique(common_Asnames) # A1,A2,A3,...
    common_Dims <- unlist(lapply(uniq_common_Asnames,
        function(x){params@common_dims[[x]]})) # 5,5,.
    common_As_Is <- cbind(common_Asnames, Isnames) # A1,I1 (duplicated)
    # 2. option structure
    .checkCoupledMWCA_OptionStructure_common(params)
    # 3. model
    .checkCoupledMWCA_model_common(params, Xsnames, Isnames, uniqIsnames,
        Asnames, common_As_Is)
    common_As_Is_Dims_IsSizes <- data.frame(uniq_common_Asnames, uniqIsnames,
        common_Dims, IsSizes[uniqIsnames]) # A1, I1, 5, 30,...
    # 4. Xs
    .checkCoupledMWCA_Xs_common(params, uniqIsnames, Isnames, XsSizes)
    # 5. mask
    .checkCoupledMWCA_mask_common(params, XsSizes)
    # 6. weights
    .checkCoupledMWCA_weights_common(params)
    # 7. initial
    .checkCoupledMWCA_initial_common(params, common_As_Is_Dims_IsSizes)
    # 8. algorithms
    .initMWCA_algorithms_common(params)
    # 9. iteration
    .checkCoupledMWCA_iteration_common(params)
    # 10. decomp
    .checkCoupledMWCA_decomp_common(params)
    # 11. fix
    .checkCoupledMWCA_fix_common(params)
    # 12. dims
    .checkCoupledMWCA_dims_common(params, common_As_Is_Dims_IsSizes)
    # 13. transpose
    .checkCoupledMWCA_transpose_common(params)
    # 14. figdir
    .checkCoupledMWCA_figdir_common(params)
    # 15. rank
    .checkCoupledMWCA_ranks_common(params, common_As_Is_Dims_IsSizes)
    # 16. coretype
    .checkCoupledMWCA_coretype_common(params)
    # 17. thr
    .checkCoupledMWCA_thr_common(params)
}

.IsSizes <- function(XsSizes, Isnames){
    out <- unlist(XsSizes)
    names(out) <- Isnames
    out
}

# 2. OptionStructure: List structure Check
.dataItems <- c("Xs", "mask", "weights")
.factorItems <- c("common_initial", "common_algorithms", "common_iteration",
    "common_decomp", "common_fix", "common_dims", "common_transpose")
.optionItems <- c("thr", "viz", "figdir", "verbose")
.checkCoupledMWCA_OptionStructure_common <- function(params){
    namesXs <- names(params@Xs)
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
    # 2. "A1", "A2", "A3", "A4", "A5"
    factoritems <- lapply(.factorItems, function(d){
        eval(parse(text=paste0("params@", d)))
    })
    for(i in 2:length(factoritems)){
        if(!identical(names(factoritems[[1]]), names(factoritems[[i]]))){
            msg <- paste0("names(params@", .factorItems[i], ") ",
                "must be the same of names(params@common_initial)")
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

# 3. model: Consistency of Xs/Is/As Names Check
.checkCoupledMWCA_model_common <- function(params, Xsnames, Isnames,
    uniqIsnames, Asnames, common_As_Is){
    # Check: Xs names
    if(!identical(Xsnames, names(params@Xs))){
        stop("names(params@common_model) must be the same of names(params@Xs)")
    }
    # Check: Frequency of I/A
    if(!identical(sort(as.vector(table(Isnames))),
        sort(as.vector(table(Asnames))))){
        msg <- paste0("The number of dimensions and ",
            "the number of lower dimensions are different")
        stop(msg)
    }
    # Check: Same I, Same A
    lapply(uniqIsnames, function(x){
        target <- which(common_As_Is[,"Isnames"] == x)
        if(length(unique(common_As_Is[,"common_Asnames"][target])) != 1){
            stop("The same I index (I1) has the same A index (e.g. A1)")
        }
    })
}

# 4. Xs: Array/Size Check
.all.equal <- function(x){
    all(x[1] == x)
}

.checkCoupledMWCA_Xs_common <- function(params, uniqIsnames, Isnames, XsSizes){
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

# 5. mask: Null/Size Check
.checkCoupledMWCA_mask_common <- function(params, XsSizes){
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

# 6. weights: Length/Numeric Check
.checkCoupledMWCA_weights_common <- function(params){
    XsLength <- length(params@Xs) # 3
    if(length(params@weights) != XsLength){
        stop("length(params@weights) must be the same as length(params@Xs)")
    }
    if(!is.numeric(unlist(params@weights))){
        stop("params@weights must be specified as a numerical vector")
    }
}

# 7. initial: Null/Size Check
.checkCoupledMWCA_initial_common <- function(params, common_As_Is_Dims_IsSizes){
    for(l in seq_along(params@common_initial)){
        init <- params@common_initial[[l]]
        dim_high <- common_As_Is_Dims_IsSizes$IsSizes[l]
        dim_low <- common_As_Is_Dims_IsSizes$Dims[l]
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

# 8. algorithms: Function Exist Check
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

# 9. iteration: Integer Check
.checkCoupledMWCA_iteration_common <- function(params){
    lapply(params@common_iteration, function(p){
        if(!all(p %% 1 == 0)){
            stop("params@common_iteration must be specified as an integer vector")
        }
    })
}

# 10. decomp: Logical Check
.checkCoupledMWCA_decomp_common <- function(params){
    lapply(params@common_decomp, function(p){
        if(!all(is.logical(unlist(p)))){
            stop("params@common_iteration must be specified as a logical vector")
        }
    })
}

# 11. fix: Logical Check
.checkCoupledMWCA_fix_common <- function(params){
    lapply(params@common_fix, function(p){
        if(!all(is.logical(unlist(p)))){
            stop("params@common_iteration must be specified as a logical vector")
        }
    })
}

# 12. dims: Size Check
.checkCoupledMWCA_dims_common <- function(params, common_As_Is_Dims_IsSizes){
    for(l in seq_along(params@common_initial)){
        dim_high <- common_As_Is_Dims_IsSizes$IsSizes[l]
        dim_low <- common_As_Is_Dims_IsSizes$Dims[l]
        if(dim_high < dim_low){
            msg <- paste0("At least one too large lower dimension ",
                "was specified in params@common_model")
            stop(msg)
        }
    }
}

# 13. transpose: Logical Check
.checkCoupledMWCA_transpose_common <- function(params){
    lapply(params@common_transpose, function(p){
        if(!all(is.logical(unlist(p)))){
            stop("params@common_transpose must be specified as a logical vector")
        }
    })
}

# 14. figdir: Null/Character Character Check
.checkCoupledMWCA_figdir_common <- function(params){
    if(!is.null(params@figdir)){
        if(!is.character(params@figdir)){
            stop("params@figdir must be character")
        }
    }
}

# 15. ranks: Mathematically Inpossible Values Check
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
            if(!.all.equal(common_As_Is_Dims_IsSizes$Dims[target])){
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
        dim_low <- common_As_Is_Dims_IsSizes$Dims[target]
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
        dim_low <- common_As_Is_Dims_IsSizes$Dims[target]
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

# 16. coretype: Value Check
.checkCoupledMWCA_coretype_common <- function(params){
    if(params@coretype %ni% c("Tucker", "CP")){
        stop("params@coretype must be 'Tucker' or 'CP'")
    }
}

# 17. thr: Value Check
.checkCoupledMWCA_thr_common <- function(params){
    if(params@thr < 0){
        stop("params@thr must be a positive value")
    }
}
