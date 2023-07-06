.all.equal <- function(x){
    all(x[1] == x)
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
