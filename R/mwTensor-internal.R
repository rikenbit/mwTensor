.normMat <- function(mat, direction="row"){
    if(direction == "row"){
        nom <- apply(mat, 1, function(m){
            norm(as.matrix(m), "F")
        })
        mat / nom
    }else if(direction == "column"){
        nom <- apply(mat, 2, function(m){
            norm(as.matrix(m), "F")
        })
        t(t(mat) / nom)
    }else{
    	stop("Wrong direction")
    }
}

.recError <- function (X = NULL, X_bar = NULL, notsqrt = FALSE){
    if (is(X)[1] == "matrix" && is(X_bar)[1] == "matrix") {
        v <- as.vector(X_bar - X)
    }
    else if (is(X)[1] == "Tensor" && is(X_bar)[1] == "Tensor") {
        v <- vec(X_bar - X)
    }
    if(notsqrt){
        sum(v * v)
    }else{
        sqrt(sum(v * v))
    }
}

.recErrors <- function(Xs, X_bars, Ms=NULL, minus=FALSE){
	if(is.null(Ms)){
		out <- lapply(seq_along(Xs), function(i){
			X <- Xs[[i]]
			X_bar <- X_bars[[i]]
			.recError(X, X_bar)
		})
	}else{
		out <- lapply(seq_along(Xs), function(i){
			X <- Xs[[i]]
			X_bar <- X_bars[[i]]
			M <- Ms[[i]]
			if(minus){
				M <- 1 - M
			}
			.recError(M*X, M*X_bar)
		})
	}
	sum(unlist(out))
}

.searchFactor <- function(p, keyword){
	i_idx <- c()
	j_idx <- c()
	for(i in seq_along(p)){
		p_i <- p[[i]]
		j <- which(names(p_i) == keyword)
		if(length(j) != 0){
			i_idx <- c(i_idx, i)
			j_idx <- c(j_idx, j)
		}
	}
	list(i=i_idx, j=j_idx)
}

.recTensors <- function(Ss, As, reverse=FALSE){
	lapply(seq_along(Ss), function(i){
		S <- Ss[[i]]
		A <- As[[i]]
		idx <- seq_along(dim(S))
		recTensor(S=S, A=A, idx=idx, reverse=reverse)
	})
}

.Projection <- function(X, A, idx=NULL, transpose=FALSE){
	if(is.null(idx)){
		idx <- seq_along(dim(X))
	}
	if(transpose){
		Ainv <- lapply(A, t)
	}else{
		Ainv <- lapply(A, ginv)
	}
	out <- as.tensor(X)
	for(a in seq_along(Ainv)){
		out <- ttm(out, t(Ainv[[a]]), m=idx[a])
	}
	out
}

.randMat <- function(l1, l2){
    mat_rand <- matrix(runif(l1*l2), nrow=l1, ncol=l2)
    .normMat(mat_rand, "column")
}

.unitMat <- function(l1, l2){
    mat_one <- matrix(0, nrow=l1, ncol=l2)
    diag(mat_one) <- 1
    mat_one
}
