Xs <- mwTensor::toyModel("coupled_CP_Easy")

common_model <- list(
    X1=list(I1="A1", I2="A2"),
    X2=list(I2="A2", I3="A3", I4="A4"),
    X3=list(I4="A4", I5="A5"))

params <- defaultCoupledMWCAParams(Xs, common_model)

# -------------------------------------------
# 1. Basic: returns CoupledMWCAInit with correct structure
# -------------------------------------------
init <- initCoupledMWCA(params, seed=42L)
expect_true(is(init, "CoupledMWCAInit"))
expect_identical(init@init_policy, "random")
expect_identical(init@seed, 42L)
expect_true(is(init@params, "CoupledMWCAParams"))

# -------------------------------------------
# 2. Common factor shapes are correct
# -------------------------------------------
# X1 is 20x30: A1 maps I1(20)->dim 2, A2 maps I2(30)->dim 2
# X2 is 30x30x30: A2(30->2), A3(30->2), A4(30->2)
# X3 is 30x25: A4(30->2), A5(25->2)
expect_equal(dim(init@common_factors$A1), c(2, 20))
expect_equal(dim(init@common_factors$A2), c(2, 30))
expect_equal(dim(init@common_factors$A3), c(2, 30))
expect_equal(dim(init@common_factors$A4), c(2, 30))
expect_equal(dim(init@common_factors$A5), c(2, 25))

# -------------------------------------------
# 3. Common core shapes are correct
# -------------------------------------------
# 3 blocks: X1(2x2), X2(2x2x2), X3(2x2)
expect_equal(length(init@common_cores), 3)

# -------------------------------------------
# 4. Same seed produces same results (determinism)
# -------------------------------------------
init_a <- initCoupledMWCA(params, seed=123L)
init_b <- initCoupledMWCA(params, seed=123L)
for(nm in names(init_a@common_factors)){
    expect_equal(init_a@common_factors[[nm]], init_b@common_factors[[nm]],
        info=paste("common factor", nm, "should be identical with same seed"))
}

# -------------------------------------------
# 5. Different seeds produce different results
# -------------------------------------------
init_c <- initCoupledMWCA(params, seed=1L)
init_d <- initCoupledMWCA(params, seed=2L)
any_diff <- FALSE
for(nm in names(init_c@common_factors)){
    if(!identical(init_c@common_factors[[nm]], init_d@common_factors[[nm]])){
        any_diff <- TRUE
        break
    }
}
expect_true(any_diff)

# -------------------------------------------
# 6. SVD policy works and produces correct shapes
# -------------------------------------------
init_svd <- initCoupledMWCA(params, seed=42L, init_policy="svd")
expect_identical(init_svd@init_policy, "svd")
expect_equal(dim(init_svd@common_factors$A1), c(2, 20))
expect_equal(dim(init_svd@common_factors$A2), c(2, 30))
expect_equal(dim(init_svd@common_factors$A5), c(2, 25))

# SVD with same seed is deterministic
init_svd2 <- initCoupledMWCA(params, seed=42L, init_policy="svd")
for(nm in names(init_svd@common_factors)){
    expect_equal(init_svd@common_factors[[nm]], init_svd2@common_factors[[nm]])
}

# -------------------------------------------
# 7. Nonneg random policy works and produces non-negative values
# -------------------------------------------
init_nn <- initCoupledMWCA(params, seed=42L, init_policy="nonneg_random")
expect_identical(init_nn@init_policy, "nonneg_random")
for(nm in names(init_nn@common_factors)){
    expect_true(all(init_nn@common_factors[[nm]] >= 0),
        info=paste("nonneg_random factor", nm, "should be non-negative"))
}

# -------------------------------------------
# 8. decomp=FALSE produces identity-like matrices
# -------------------------------------------
params_nodecomp <- params
params_nodecomp@common_decomp <- list(A1=FALSE, A2=TRUE, A3=TRUE,
    A4=TRUE, A5=TRUE)
init_nodecomp <- initCoupledMWCA(params_nodecomp, seed=42L)
# A1: decomp=FALSE => identity-like (dim2 x dim1 = 2 x 20)
expect_equal(init_nodecomp@common_factors$A1[1,1], 1)
expect_equal(init_nodecomp@common_factors$A1[2,2], 1)
expect_equal(init_nodecomp@common_factors$A1[1,2], 0)
expect_equal(init_nodecomp@common_factors$A1[2,1], 0)

# -------------------------------------------
# 9. User-supplied initial values are preserved
# -------------------------------------------
A1_custom <- matrix(runif(2*20), nrow=2, ncol=20)
params_custom <- params
params_custom@common_initial <- list(A1=A1_custom, A2=NULL, A3=NULL,
    A4=NULL, A5=NULL)
init_custom <- initCoupledMWCA(params_custom, seed=42L)
expect_equal(init_custom@common_factors$A1, A1_custom)

# -------------------------------------------
# 10. specific=TRUE initializes specific factors
# -------------------------------------------
params_spec <- params
params_spec@specific <- TRUE
init_spec <- initCoupledMWCA(params_spec, seed=42L)
expect_false(identical(init_spec@specific_factors, list(NULL)))
expect_false(identical(init_spec@specific_cores, list(NULL)))
# Check specific factor names exist
expect_true("B1" %in% names(init_spec@specific_factors))

# -------------------------------------------
# 11. specific=FALSE returns list(NULL) for specific slots
# -------------------------------------------
expect_identical(init@specific_factors, list(NULL))
expect_identical(init@specific_cores, list(NULL))

# -------------------------------------------
# 12. Invalid init_policy is rejected
# -------------------------------------------
expect_error(initCoupledMWCA(params, init_policy="bogus"))

# -------------------------------------------
# 13. Seed does not leak into global RNG state
# -------------------------------------------
set.seed(999L)
before <- runif(1)
set.seed(999L)
init_tmp <- initCoupledMWCA(params, seed=42L)
after <- runif(1)
expect_equal(before, after,
    info="initCoupledMWCA should restore RNG state after seeded run")

# -------------------------------------------
# 14. Custom dims produce correct shapes
# -------------------------------------------
params_dims <- params
params_dims@common_dims <- list(A1=3, A2=3, A3=5, A4=4, A5=4)
init_dims <- initCoupledMWCA(params_dims, seed=42L)
expect_equal(dim(init_dims@common_factors$A1), c(3, 20))
expect_equal(dim(init_dims@common_factors$A2), c(3, 30))
expect_equal(dim(init_dims@common_factors$A3), c(5, 30))
expect_equal(dim(init_dims@common_factors$A4), c(4, 30))
expect_equal(dim(init_dims@common_factors$A5), c(4, 25))

# -------------------------------------------
# 15. CoupledMWCAInit can be passed to CoupledMWCA
# -------------------------------------------
init_for_run <- initCoupledMWCA(params, seed=42L)
out_from_init <- CoupledMWCA(init_for_run)
expect_true(is(out_from_init, "CoupledMWCAResult"))
expect_equal(length(out_from_init@common_factors), 5)
# Factor shapes should match
expect_equal(dim(out_from_init@common_factors$A1), c(2, 20))
expect_equal(dim(out_from_init@common_factors$A2), c(2, 30))
expect_true(is.numeric(out_from_init@rec_error))

# -------------------------------------------
# 16. Init with seed produces reproducible CoupledMWCA result
# -------------------------------------------
out_a <- CoupledMWCA(initCoupledMWCA(params, seed=123L))
out_b <- CoupledMWCA(initCoupledMWCA(params, seed=123L))
for(nm in names(out_a@common_factors)){
    expect_equal(out_a@common_factors[[nm]], out_b@common_factors[[nm]],
        info=paste("CoupledMWCA results should match with same init seed for", nm))
}

# -------------------------------------------
# 17. SVD-initialized run also works
# -------------------------------------------
init_svd_run <- initCoupledMWCA(params, seed=42L, init_policy="svd")
out_svd <- CoupledMWCA(init_svd_run)
expect_true(is(out_svd, "CoupledMWCAResult"))
expect_equal(length(out_svd@common_factors), 5)
