X <- nnTensor::toyModel("Tucker")@data

params <- new("MWCAParams", X=X, dims=c(5,6,7), transpose=TRUE)
out <- MWCA(params)
# plotTensor3D(recTensor(as.tensor(out1_1@core), out1_1@factors))

expect_equal(length(out@algorithms), 3)
expect_equal(length(out@decomp), 3)
expect_true(is.logical(out@fix))
expect_equal(length(out@dims), 3)
expect_equal(dim(out@factors[[1]]), c(5, 50))
expect_equal(dim(out@factors[[2]]), c(6, 50))
expect_equal(dim(out@factors[[3]]), c(7, 50))
expect_equal(dim(out@core), c(5,6,7))
expect_true(is.numeric(out@rec_error))
expect_true(is.numeric(out@train_error))
expect_true(is.numeric(out@test_error))
