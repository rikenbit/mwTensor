.all.equal <- function(x){
    all(x[1] == x)
}

.IsSizes <- function(XsSizes, Isnames){
    out <- unlist(XsSizes)
    names(out) <- Isnames
    out
}

.emptyList <- function(X){
    for(i in seq_along(X)){
        X[[i]][] <- 0
        X[[i]] <- as.tensor(X[[i]])
    }
    X
}

.multiplyList <- function(X, Y){
    for(i in seq_along(X)){
        X[[i]] <- X[[i]] * Y[[i]]
    }
    X
}

.sumList <- function(X, Y){
    for(i in seq_along(X)){
        X[[i]] <- X[[i]] + Y[[i]]
    }
    X
}

.subtractList <- function(X, Y){
    for(i in seq_along(X)){
        X[[i]] <- X[[i]] - Y[[i]]
    }
    X
}

'%ni%' <- Negate('%in%')

.anyNaN <- function(X){
    length(which(is.nan(X))) != 0
}

.anyInf <- function(X){
    length(which(is.infinite(X))) != 0
}

.recError <- function (X = NULL, Y = NULL, notsqrt = FALSE){
    if (is(X)[1] == "matrix" && is(Y)[1] == "matrix") {
        v <- as.vector(Y - X)
    }
    else if (is(X)[1] == "Tensor" && is(Y)[1] == "Tensor") {
        v <- vec(Y - X)
    }
    if(notsqrt){
        sum(v * v, na.rm=TRUE)
    }else{
        sqrt(sum(v * v, na.rm=TRUE))
    }
}

.searchFactor <- function(model, keyword){
	i_idx <- c()
	j_idx <- c()
	for(i in seq_along(model)){
		model_i <- unlist(model[[i]])
		j <- which(model_i == keyword)
		if(length(j) != 0){
			i_idx <- c(i_idx, i)
			j_idx <- c(j_idx, j[1])
		}
	}
	list(i=i_idx, j=j_idx)
}

.recTensors <- function(Ss, As, model, reverse=FALSE){
	lapply(seq_along(Ss), function(i){
        SrelatedAsNames <- as.vector(unlist(model[[i]]))
        SrelatedAs <- lapply(SrelatedAsNames, function(x){
            As[[x]]
        })
		S <- Ss[[i]]
		idx <- seq_along(dim(S))
		recTensor(S=S, A=SrelatedAs, idx=idx, reverse=reverse)
	})
}

.Projection <- function(X, A, idx=NULL, transpose=FALSE){
	if(is.null(idx)){
		idx <- seq_along(dim(X))
	}
	if(is.list(transpose)){
		Ainv <- lapply(seq_along(A), function(a){
			if(transpose[[a]]){
				t(A[[a]])
			}else{
				ginv(A[[a]])
			}
		})
	}else{
		if(transpose){
			Ainv <- lapply(A, t)
		}else{
			Ainv <- lapply(A, ginv)
		}
	}
	if(is.array(X)){
		out <- as.tensor(X)
	}else{
		out <- X
	}
	for(a in seq_along(Ainv)){
		out <- ttm(out, t(Ainv[[a]]), m=idx[a])
	}
	out
}

.Projections <- function(Xs, As, model, transpose, coretype){
    lapply(seq_along(Xs), function(i){
        XrelatedAsNames <- as.vector(unlist(model[[i]]))
        XrelatedAs <- lapply(XrelatedAsNames, function(x){
            As[[x]]
        })
        out <- .Projection(Xs[[i]], XrelatedAs, transpose=transpose)
        if(coretype == "CP"){
            out <- .diagMat(out)
        }
        out
    })
}

.diagMat <- function(out){
    num_modes <- .ndim(out@data)
    min.s <- min(dim(out@data))
    tmp <- out
    tmp@data[] <- 0
    cmd <- paste0("for(i in seq_len(min.s)){",
        "tmp@data[",
        paste(rep("i", length=num_modes), collapse=","),
        "] <- out@data[",
            paste(rep("i", length=num_modes), collapse=","), "]}")
    eval(parse(text=cmd))
    tmp
}

.randMat <- function(l1, l2){
    mat_rand <- matrix(runif(l1*l2), nrow=l1, ncol=l2)
    .normMat(mat_rand, "row")
}

.normMat <- function(mat, direction="row"){
    if(ncol(mat) == 1){
        out <- mat / norm(mat, "F")
    }else{
        if(direction == "row"){
            nom <- apply(mat, 1, function(m){
                norm(as.matrix(m), "F")
            })
            out <- mat / nom
        }else if(direction == "column"){
            nom <- apply(mat, 2, function(m){
                norm(as.matrix(m), "F")
            })
            out <- t(t(mat) / nom)
        }else{
            stop("Wrong direction")
        }
    }
    out[which(is.na(out))] <- 0
    out
}

.unitMat <- function(l1, l2){
    mat_one <- matrix(0, nrow=l1, ncol=l2)
    diag(mat_one) <- 1
    mat_one
}

# Shared rank checks for common/specific.
# model: the model list (e.g. params@common_model or params@specific_model)
# info: data.frame with columns Asnames, Dims, IsSizes
.checkRanks_matrix <- function(model, info){
    lapply(seq_along(model), function(i){
        x <- model[[i]]
        if(length(x) == 2){
            target <- unlist(lapply(x, function(xx){
                which(info$Asnames == xx)
            }))
            if(!.all.equal(info$Dims[target])){
                msg <- paste0("model[[", i, "]] is a matrix.")
                msg <- paste(c(msg, "In such a case, the lower dimension of ",
                    paste(info$Asnames[target], collapse=", "),
                    "must be the same number in dims."), collapse=" ")
                stop(msg)
            }
        }
    })
}

.checkRanks_one <- function(model, info){
    lapply(seq_along(model), function(i){
        x <- model[[i]]
        target <- unlist(lapply(x, function(xx){
            which(info$Asnames == xx)
        }))
        dim_low <- info$Dims[target]
        if(1 %in% dim_low){
            not1 <- dim_low[setdiff(seq_along(dim_low), which(dim_low == 1))]
            if(!.all.equal(not1)){
                msg <- paste0(c("The lower dimension 1 is specified",
                    "as at least one of",
                    paste(info$Asnames[target], collapse=", ")),
                    collapse=" ")
                msg <- paste0(msg, " to decompose a ",
                    "higher-order tensor (length(dim(X)) >= 3). ",
                    "In such a case, all the other lower dimensions ",
                    "must be the same number in dims.")
                stop(msg)
            }
        }
    })
}

.checkRanks_projected <- function(model, info){
    lapply(seq_along(model), function(i){
        x <- model[[i]]
        target <- unlist(lapply(x, function(xx){
            which(info$Asnames == xx)
        }))
        dim_high <- info$IsSizes[target]
        dim_low <- info$Dims[target]
        dim_idx <- seq_along(dim_low)
        dim_proj <- unlist(lapply(dim_idx, function(d){
            prod(dim_low[setdiff(dim_idx, d)])
        }))
        if(!all(dim_proj >= dim_low)){
            msg <- paste0("After the projection of ",
                names(model)[i],
                ", the dimension is smaller than at least one of ",
                paste(info$Asnames[target], collapse=", "),
                " in a dimension. Change the lower dimension of ",
                paste(info$Asnames[target], collapse=", "),
                " in dims.")
            stop(msg)
        }
    })
}
