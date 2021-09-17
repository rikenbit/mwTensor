mySVD <- function(Xn, k){
    svd(Xn)$u[, seq(k)]
}
