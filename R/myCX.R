myCX <- function(Xn, k){
    if(k == 1){
        .normMat(CX(Xn, rank=k+1)$C, "column")[,1]
    }else{
        .normMat(CX(Xn, rank=k)$C, "column")
    }
}
