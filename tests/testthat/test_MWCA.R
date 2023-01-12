X <- array(runif(5*6*7), dim=c(5,6,7))

params <- new("MWCAParams",
	X=X,
	mask=NULL,
	algorithms=c("myALS_SVD", "myNMF", "myICA"),
	dims=c(2,3,4),
	transpose=FALSE,
	viz=FALSE,
	figdir=NULL)

out <- MWCA(params)

# Test Output
expect_equal(length(out@algorithms), 3)
expect_equal(length(out@dims), 3)
expect_equal(dim(out@factors[[1]]), c(2, 5))
expect_equal(dim(out@factors[[2]]), c(3, 6))
expect_equal(dim(out@factors[[3]]), c(4, 7))
expect_equal(dim(out@core), c(2,3,4))
expect_true(is.numeric(out@rec_error))
expect_true(is.numeric(out@train_error))
expect_true(is.numeric(out@test_error))

# Test X
X_dummy <- X
X_dummy[1,1,1] <- NA
params_dummy <- params
params_dummy@X <- X_dummy
expect_error(MWCA(params_dummy))

X_dummy[1,1,1] <- NaN
params_dummy <- params
params_dummy@X <- X_dummy
expect_error(MWCA(params_dummy))

X_dummy[1,1,1] <- Inf
params_dummy <- params
params_dummy@X <- X_dummy
expect_error(MWCA(params_dummy))

# mask:
M <- X
M[] <- 1
params_dummy <- params
params_dummy@mask <- M
expect_error(expect_error(MWCA(params_dummy)))

M[1,1,1] <- 2
params_dummy <- params
params_dummy@mask <- M
expect_error(MWCA(params_dummy))

M[1,1,1] <- NA
params_dummy <- params
params_dummy@mask <- M
expect_error(MWCA(params_dummy))

M[1,1,1] <- Inf
params_dummy <- params
params_dummy@mask <- M
expect_error(MWCA(params_dummy))

# algorithms:
params_dummy <- params
params_dummy@algorithms <- c("myALS_SVDD", "myNMF", "myICA")
expect_error(MWCA(params_dummy))

params_dummy <- params
params_dummy@algorithms <- c("myALS_SVD")
expect_error(MWCA(params_dummy))

# dims:
params_dummy <- params
params_dummy@dims <- c(2,3,100)
expect_error(MWCA(params_dummy))

# transpose:
params_dummy <- params
expect_error(params_dummy@transpose <- "TRUE")