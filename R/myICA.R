myICA <- function(Xn, k){
    if(k == 1){
        .normMat(icafast(Xn, nc=k+1)$Y, "column")[,1]
    }else{
        .normMat(icafast(Xn, nc=k)$Y, "column")
    }
}
