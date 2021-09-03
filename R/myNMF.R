myNMF <- function(Xn, k, L1=1e-10, L2=1e-10){
    .normMat(NMF(abs(Xn), J=k, L1_U=L1, L2_U=L2)$U, "column")
}
