Xs=list(
    X1=array(runif(7*4), dim=c(7,4)),
    X2=array(runif(4*5*6), dim=c(4,5,6)),
    X3=array(runif(6*8), dim=c(6,8)))
            
common_model=list(
    X1=list(I1="A1", I2="A2"),
    X2=list(I2="A2", I3="A3", I4="A4"),
    X3=list(I4="A4", I5="A5"))

params <- defaultCoupledMWCAParams(Xs, common_model)

out <- CoupledMWCA(params)

# Test Output
# Data-wise setting
expect_equal(length(out@weights), 3)
# Common Factor Matrices
expect_equal(length(out@common_model), 3)
expect_equal(length(out@common_initial), 5)
expect_equal(length(out@common_algorithms), 5)
expect_equal(length(out@common_iteration), 5)
expect_equal(length(out@common_decomp), 5)
expect_equal(length(out@common_fix), 5)
expect_equal(length(out@common_dims), 5)
expect_equal(length(out@common_transpose), 5)
expect_identical(out@common_coretype, "Tucker")
expect_equal(dim(out@common_factors[[1]]), c(2, 7))
expect_equal(dim(out@common_factors[[2]]), c(2, 4))
expect_equal(dim(out@common_factors[[3]]), c(2, 5))
expect_equal(dim(out@common_factors[[4]]), c(2, 6))
expect_equal(dim(out@common_factors[[5]]), c(2, 8))
expect_equal(dim(out@common_cores[[1]]), c(2,2))
expect_equal(dim(out@common_cores[[2]]), c(2,2,2))
expect_equal(dim(out@common_cores[[3]]), c(2,2))
# Specific Factor Matrices
expect_equal(length(out@specific_model), 3)
expect_identical(out@specific_initial, list(NULL))
expect_equal(length(out@specific_algorithms), 7)
expect_equal(length(out@specific_iteration), 7)
expect_equal(length(out@specific_decomp), 7)
expect_equal(length(out@specific_fix), 7)
expect_equal(length(out@specific_dims), 7)
expect_equal(length(out@specific_transpose), 7)
expect_identical(out@specific_coretype, "Tucker")
expect_identical(out@specific_factors, list(NULL))
expect_identical(out@specific_cores, list(NULL))
# Other option
expect_identical(out@specific, FALSE)
expect_identical(out@thr, 1e-10)
expect_equal(out@viz, FALSE)
expect_equal(out@figdir, NULL)
expect_equal(out@verbose, FALSE)
# Iteration
expect_true(is.numeric(out@rec_error))
expect_true(is.numeric(out@train_error))
expect_true(is.numeric(out@test_error))
expect_true(is.numeric(out@rel_change))