myICA <- function(Xn, k){
    .normMat(icafast(Xn, nc=k)$Y, "column")
}
