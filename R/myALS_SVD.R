myALS_SVD <- function(Xn, k, L2=1e-10, iter=30){
    nr <- nrow(Xn)
    nc <- ncol(Xn)
    U <- .normMat(matrix(runif(nr*k), nrow=nr, ncol=k), "column")
    V <- .normMat(matrix(runif(k*nc), nrow=k, ncol=nc), "row")
    for(i in seq(iter)){
        U <- .normMat(Xn %*% t(V) %*% ginv(V %*% t(V) + diag(L2, k, k)),
            "column")
        V <- ginv(t(U) %*% U + diag(L2, k, k)) %*% t(U) %*% Xn
    }
    U
}
