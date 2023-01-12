X <- nnTensor::toyModel("Tucker")@data

params <- defaultMWCAParams(X=X)
out <- MWCA(params)
# plotTensor3D(recTensor(as.tensor(out1_1@core), out1_1@factors))

# Test Output
expect_equal(length(out@algorithms), 3)
expect_equal(length(out@dims), 3)
expect_equal(dim(out@factors[[1]]), c(2, 50))
expect_equal(dim(out@factors[[2]]), c(2, 50))
expect_equal(dim(out@factors[[3]]), c(2, 50))
expect_equal(dim(out@core), c(2,2,2))
expect_true(is.numeric(out@rec_error))
expect_true(is.numeric(out@train_error))
expect_true(is.numeric(out@test_error))
