Xs <- mwTensor::toyModel("coupled_CP_Easy")

common_model <- list(
    X1=list(I1="A1", I2="A2"),
    X2=list(I2="A2", I3="A3", I4="A4"),
    X3=list(I4="A4", I5="A5"))

params <- defaultCoupledMWCAParams(Xs, common_model)

# -------------------------------------------
# 1. Valid coupled problem passes
# -------------------------------------------
res <- checkCoupledMWCA(params)
expect_true(res$ok)
expect_equal(length(res$errors), 0)
expect_equal(length(res$warnings), 0)
expect_false(is.null(res$normalized_params))
expect_true(is(res$normalized_params, "CoupledMWCAParams"))
expect_identical(res$summary, "Validation passed")

# -------------------------------------------
# 2. Mode length mismatch
# -------------------------------------------
# common_model has wrong number of modes for X2
params_bad_mode <- params
params_bad_mode@common_model <- list(
    X1=list(I1="A1", I2="A2"),
    X2=list(I2="A2", I3="A3"),
    X3=list(I4="A4", I5="A5"))
res <- checkCoupledMWCA(params_bad_mode)
expect_false(res$ok)
expect_true(length(res$errors) > 0)
expect_true(is.null(res$normalized_params))
# Should mention the mode mismatch for X2
expect_true(any(grepl("X2", res$errors)))

# -------------------------------------------
# 3. Invalid common settings: bad algorithm
# -------------------------------------------
params_bad_algo <- params
params_bad_algo@common_algorithms <- list(
    A1="mySVDD", A2="myALS_SVD", A3="myNMF", A4="myICA", A5="myCX")
res <- checkCoupledMWCA(params_bad_algo)
expect_false(res$ok)
expect_true(any(grepl("mySVDD", res$errors)))

# -------------------------------------------
# 4. Mask shape mismatch
# -------------------------------------------
bad_mask <- list(
    X1=array(1, dim=c(5, 5)),
    X2=NULL,
    X3=NULL)
params_bad_mask <- params
params_bad_mask@mask <- bad_mask
res <- checkCoupledMWCA(params_bad_mask)
expect_false(res$ok)
expect_true(any(grepl("mask", res$errors, ignore.case=TRUE)))

# -------------------------------------------
# 5. Invalid specific settings
# -------------------------------------------
params_specific <- params
params_specific@specific <- TRUE
# Corrupt the specific_model: wrong Xs names
params_specific@specific_model <- list(
    X1=list(J1="B1", J2="B2"),
    X2=list(J3="B3", J4="B4", J5="B5"),
    WRONG=list(J6="B6", J7="B7"))
res <- checkCoupledMWCA(params_specific)
expect_false(res$ok)
expect_true(length(res$errors) > 0)

# -------------------------------------------
# 6. dims too large
# -------------------------------------------
params_bad_dims <- params
params_bad_dims@common_dims <- list(A1=300, A2=3, A3=5, A4=4, A5=4)
res <- checkCoupledMWCA(params_bad_dims)
expect_false(res$ok)
expect_true(any(grepl("dimension", res$errors, ignore.case=TRUE)))

# -------------------------------------------
# 7. Weights length mismatch
# -------------------------------------------
params_bad_wt <- params
params_bad_wt@weights <- list(X1=1, X3=1)
res <- checkCoupledMWCA(params_bad_wt)
expect_false(res$ok)
expect_true(any(grepl("weights", res$errors, ignore.case=TRUE)))

# -------------------------------------------
# 8. Invalid coretype
# -------------------------------------------
params_bad_core <- params
params_bad_core@common_coretype <- "TUCKER"
res <- checkCoupledMWCA(params_bad_core)
expect_false(res$ok)
expect_true(any(grepl("coretype", res$errors, ignore.case=TRUE)))

# -------------------------------------------
# 9. CP coretype with unequal dims
# -------------------------------------------
params_cp_bad <- params
params_cp_bad@common_coretype <- "CP"
params_cp_bad@common_dims <- list(A1=3, A2=5, A3=5, A4=4, A5=4)
res <- checkCoupledMWCA(params_cp_bad)
expect_false(res$ok)
expect_true(any(grepl("CP", res$errors)))

# -------------------------------------------
# 10. CP coretype with equal dims passes
# -------------------------------------------
params_cp_ok <- params
params_cp_ok@common_coretype <- "CP"
params_cp_ok@common_dims <- list(A1=3, A2=3, A3=3, A4=3, A5=3)
res <- checkCoupledMWCA(params_cp_ok)
expect_true(res$ok)

# -------------------------------------------
# 11. Warning when all factors are fixed
# -------------------------------------------
params_all_fix <- params
params_all_fix@common_fix <- list(A1=TRUE, A2=TRUE, A3=TRUE, A4=TRUE, A5=TRUE)
res <- checkCoupledMWCA(params_all_fix)
expect_true(res$ok)
expect_true(length(res$warnings) > 0)
expect_true(any(grepl("fixed", res$warnings, ignore.case=TRUE)))

# -------------------------------------------
# 12. Valid with specific=TRUE
# -------------------------------------------
params_spec_ok <- params
params_spec_ok@specific <- TRUE
res <- checkCoupledMWCA(params_spec_ok)
expect_true(res$ok)

# -------------------------------------------
# 13. Non-integer iteration
# -------------------------------------------
params_bad_iter <- params
params_bad_iter@common_iteration <- list(A1=1.5, A2=2, A3=3, A4=4, A5=5)
res <- checkCoupledMWCA(params_bad_iter)
expect_false(res$ok)
expect_true(any(grepl("iteration", res$errors, ignore.case=TRUE)))

# -------------------------------------------
# 14. Multiple independent errors are all collected
# -------------------------------------------
params_multi <- params
# Error 1: bad algorithm
params_multi@common_algorithms <- list(
    A1="mySVDD", A2="myALS_SVD", A3="myNMF", A4="myICA", A5="myCX")
# Error 2: non-integer iteration
params_multi@common_iteration <- list(A1=1.5, A2=2, A3=3, A4=4, A5=5)
# Error 3: invalid coretype
params_multi@common_coretype <- "TUCKER"
res <- checkCoupledMWCA(params_multi)
expect_false(res$ok)
# All three errors should be present
expect_true(any(grepl("mySVDD", res$errors)))
expect_true(any(grepl("iteration", res$errors, ignore.case=TRUE)))
expect_true(any(grepl("coretype", res$errors, ignore.case=TRUE)))
expect_true(length(res$errors) >= 3)

# -------------------------------------------
# 15. dims + mask errors collected together
# -------------------------------------------
params_multi2 <- params
# Error 1: dims too large
params_multi2@common_dims <- list(A1=300, A2=3, A3=5, A4=4, A5=4)
# Error 2: bad mask shape
params_multi2@mask <- list(
    X1=array(1, dim=c(5, 5)),
    X2=NULL,
    X3=NULL)
res <- checkCoupledMWCA(params_multi2)
expect_false(res$ok)
expect_true(any(grepl("dimension", res$errors, ignore.case=TRUE)))
expect_true(any(grepl("mask", res$errors, ignore.case=TRUE)))
expect_true(length(res$errors) >= 2)
