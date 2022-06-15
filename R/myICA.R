myICA <- function(Xn, k){
    if(k == 1){
        .normMat(ICA(Xn, J=k+1)$S, "column")[,1]
    }else{
        .normMat(ICA(Xn, J=k)$S, "column")
    }
}
