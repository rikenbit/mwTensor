myCX <- function(Xn, k){
    .normMat(CX(Xn, rank=k)$C, "column")
}
