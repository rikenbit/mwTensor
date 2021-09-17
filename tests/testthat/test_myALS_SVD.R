X <- nnTensor::toyModel("NMF")

out <- myALS_SVD(X, k=10)

expect_equivalent(dim(out), c(100, 10))
