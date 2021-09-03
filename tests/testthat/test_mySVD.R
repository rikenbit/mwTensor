X <- nnTensor::toyModel("NMF")

out1 <- mySVD(X, k=10)
out2 <- myALS_SVD(X, k=10)

expect_equivalent(dim(out1), c(100, 10))
expect_equivalent(dim(out2), c(100, 10))
