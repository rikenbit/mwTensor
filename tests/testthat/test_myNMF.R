X <- nnTensor::toyModel("NMF")

out1 <- myNMF(X, k=10)

expect_equivalent(dim(out1), c(100, 10))
