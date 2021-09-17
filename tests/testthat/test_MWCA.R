X <- nnTensor::toyModel("Tucker")@data

params <- new("MWCAParams", X=X)
out <- MWCA(params)
# plotTensor3D(recTensor(as.tensor(out1_1@core), out1_1@factors))

# Test Output
expect_equal(length(out@algorithms), 3)
expect_equal(length(out@dims), 3)
expect_equal(dim(out@factors[[1]]), c(2, 50))
expect_equal(dim(out@factors[[2]]), c(3, 50))
expect_equal(dim(out@factors[[3]]), c(4, 50))
expect_equal(dim(out@core), c(2,3,4))
expect_true(is.numeric(out@rec_error))
expect_true(is.numeric(out@train_error))
expect_true(is.numeric(out@test_error))

# Test X
X_dummy <- X
X_dummy[1,1,1] <- NA
params_dummy <- new("MWCAParams", X=X_dummy)
expect_error(MWCA(params_dummy))

X_dummy[1,1,1] <- NaN
params_dummy <- new("MWCAParams", X=X_dummy)
expect_error(MWCA(params_dummy))

X_dummy[1,1,1] <- Inf
params_dummy <- new("MWCAParams", X=X_dummy)
expect_error(MWCA(params_dummy))

# mask:
M <- X
M[] <- 1
params_dummy <- new("MWCAParams", X=X, mask=M)
expect_error(expect_error(MWCA(params_dummy)))

M[1,1,1] <- 2
params_dummy <- new("MWCAParams", X=X, mask=M)
expect_error(MWCA(params_dummy))

M[1,1,1] <- NA
params_dummy <- new("MWCAParams", X=X, mask=M)
expect_error(MWCA(params_dummy))

M[1,1,1] <- Inf
params_dummy <- new("MWCAParams", X=X, mask=M)
expect_error(MWCA(params_dummy))

# algorithms:
params_dummy <- new("MWCAParams", X=X,
	algorithms=c("myALS_SVDD", "myNMF", "myICA"))
expect_error(MWCA(params_dummy))

params_dummy <- new("MWCAParams", X=X,
	algorithms=c("myALS_SVD"))
expect_error(MWCA(params_dummy))

# dims:
params_dummy <- new("MWCAParams", X=X,
	dims=c(2, 3, 100))
expect_error(MWCA(params_dummy))

# transpose:
expect_error(new("MWCAParams", X=X, transpose="TRUE"))
