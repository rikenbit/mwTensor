# ============================================================
# Setup: MWCA fit on a simple matrix
# ============================================================
set.seed(42)
X_mat <- matrix(runif(20*30), nrow=20, ncol=30)
params_mat <- new("MWCAParams",
    X=X_mat, mask=NULL, pseudocount=1e-10,
    algorithms=c("mySVD", "mySVD"),
    dims=c(5L, 5L), transpose=FALSE, viz=FALSE, figdir=NULL)
fit_mat <- MWCA(params_mat)

# ============================================================
# 1. Basic refinement of MWCA factor
# ============================================================
ref <- refineFactor(fit_mat, 1L, algorithm="mySVD", dim=2L)
expect_true(is(ref, "RefinedFactor"))
expect_equal(ref@source_factor_name, "1")
expect_equal(ref@algorithm, "mySVD")
expect_equal(ref@dim, 2L)

# source_factor is factors[[1]]: 5 x 20
expect_equal(dim(ref@source_factor), c(5L, 20L))
# sub_factors: t(U) = 2 x 20
expect_equal(dim(ref@sub_factors), c(2L, 20L))
# coef: t(V) = 5 x 2
expect_equal(dim(ref@coef), c(5L, 2L))

# ============================================================
# 2. Approximation relation: A ~ coef %*% sub_factors
# ============================================================
approx_error <- max(abs(ref@source_factor - ref@coef %*% ref@sub_factors))
# SVD-based refinement with dim=2 on a rank-5 factor won't be exact,
# but for a random matrix it should be a reasonable approximation
expect_true(is.finite(approx_error))

# With dim = k (full rank), the approximation should be near-exact
ref_full <- refineFactor(fit_mat, 1L, algorithm="mySVD", dim=5L)
approx_error_full <- max(abs(ref_full@source_factor -
    ref_full@coef %*% ref_full@sub_factors))
expect_true(approx_error_full < 1e-8)

# ============================================================
# 3. Refinement with different algorithms
# ============================================================
ref_nmf <- refineFactor(fit_mat, 1L, algorithm="myNMF", dim=3L)
expect_equal(ref_nmf@algorithm, "myNMF")
expect_equal(ref_nmf@dim, 3L)
expect_equal(dim(ref_nmf@sub_factors), c(3L, 20L))
expect_equal(dim(ref_nmf@coef), c(5L, 3L))

# ============================================================
# 4. Refinement of second factor
# ============================================================
ref2 <- refineFactor(fit_mat, 2L, algorithm="mySVD", dim=2L)
expect_equal(dim(ref2@source_factor), c(5L, 30L))
expect_equal(dim(ref2@sub_factors), c(2L, 30L))
expect_equal(dim(ref2@coef), c(5L, 2L))

# ============================================================
# 5. Invalid factor index -> error
# ============================================================
expect_error(refineFactor(fit_mat, 0L, algorithm="mySVD", dim=2L),
    "out of range")
expect_error(refineFactor(fit_mat, 99L, algorithm="mySVD", dim=2L),
    "out of range")

# ============================================================
# 6. Invalid algorithm -> error
# ============================================================
expect_error(refineFactor(fit_mat, 1L, algorithm="noSuchAlgo", dim=2L),
    "not a built-in")

# ============================================================
# 7. dim too large -> error
# ============================================================
expect_error(refineFactor(fit_mat, 1L, algorithm="mySVD", dim=6L),
    "exceeds")

# ============================================================
# Setup: CoupledMWCA fit
# ============================================================
Xs <- mwTensor::toyModel("coupled_CP_Easy")
params_coupled <- new("CoupledMWCAParams",
    Xs=Xs,
    mask=list(X1=NULL, X2=NULL, X3=NULL),
    pseudocount=1E-10,
    weights=list(X1=1, X2=1, X3=1),
    common_model=list(
        X1=list(I1="A1", I2="A2"),
        X2=list(I2="A2", I3="A3", I4="A4"),
        X3=list(I4="A4", I5="A5")),
    common_initial=list(A1=NULL, A2=NULL, A3=NULL, A4=NULL, A5=NULL),
    common_algorithms=list(A1="mySVD", A2="mySVD", A3="mySVD",
        A4="mySVD", A5="mySVD"),
    common_iteration=list(A1=3, A2=3, A3=3, A4=3, A5=3),
    common_decomp=list(A1=TRUE, A2=TRUE, A3=TRUE, A4=TRUE, A5=TRUE),
    common_fix=list(A1=FALSE, A2=FALSE, A3=FALSE, A4=FALSE, A5=FALSE),
    common_dims=list(A1=3, A2=3, A3=3, A4=3, A5=3),
    common_transpose=list(A1=FALSE, A2=FALSE, A3=FALSE, A4=FALSE, A5=FALSE),
    common_coretype="Tucker",
    specific_model=list(
        X1=list(J1="B1", J2="B2"),
        X2=list(J3="B3", J4="B4", J5="B5"),
        X3=list(J6="B6", J7="B7")),
    specific_initial=list(B1=NULL, B2=NULL, B3=NULL, B4=NULL, B5=NULL,
        B6=NULL, B7=NULL),
    specific_algorithms=list(B1="mySVD", B2="mySVD", B3="mySVD",
        B4="mySVD", B5="mySVD", B6="mySVD", B7="mySVD"),
    specific_iteration=list(B1=3, B2=3, B3=3, B4=3, B5=3, B6=3, B7=3),
    specific_decomp=list(B1=TRUE, B2=TRUE, B3=TRUE, B4=TRUE, B5=TRUE,
        B6=TRUE, B7=TRUE),
    specific_fix=list(B1=FALSE, B2=FALSE, B3=FALSE, B4=FALSE, B5=FALSE,
        B6=FALSE, B7=FALSE),
    specific_dims=list(B1=2, B2=2, B3=2, B4=2, B5=2, B6=2, B7=2),
    specific_transpose=list(B1=FALSE, B2=FALSE, B3=FALSE, B4=FALSE,
        B5=FALSE, B6=FALSE, B7=FALSE),
    specific_coretype="Tucker",
    specific=FALSE, thr=1e-10, viz=FALSE, figdir=NULL, verbose=FALSE)
fit_coupled <- CoupledMWCA(params_coupled)

# ============================================================
# 8. Coupled: refine a common factor by name
# ============================================================
ref_A2 <- refineFactor(fit_coupled, "A2", algorithm="mySVD", dim=2L)
expect_true(is(ref_A2, "RefinedFactor"))
expect_equal(ref_A2@source_factor_name, "A2")
expect_equal(dim(ref_A2@source_factor), c(3L, 30L))
expect_equal(dim(ref_A2@sub_factors), c(2L, 30L))
expect_equal(dim(ref_A2@coef), c(3L, 2L))

# ============================================================
# 9. Coupled: nonexistent factor name -> error
# ============================================================
expect_error(refineFactor(fit_coupled, "GHOST", algorithm="mySVD", dim=2L),
    "not found")

# ============================================================
# 10. Source object is preserved in result
# ============================================================
expect_true(is(ref_A2@source_object, "CoupledMWCAResult"))
expect_true(is(ref@source_object, "MWCAResult"))

# ============================================================
# 11. dim=1 edge case works
# ============================================================
ref_dim1 <- refineFactor(fit_mat, 1L, algorithm="mySVD", dim=1L)
expect_equal(dim(ref_dim1@sub_factors), c(1L, 20L))
expect_equal(dim(ref_dim1@coef), c(5L, 1L))

# ============================================================
# 12. Non-character algorithm (numeric) -> error
# ============================================================
expect_error(refineFactor(fit_mat, 1L, algorithm=42, dim=2L))

# ============================================================
# 13. Invalid fit object -> error
# ============================================================
expect_error(refineFactor("not_a_fit", 1L, algorithm="mySVD", dim=2L),
    "MWCAResult or CoupledMWCAResult")

# ============================================================
# 14. Coupled: factor from specific_factors is accessible
# ============================================================
# fit_coupled was set up with specific=FALSE, so specific_factors
# are empty. Test that requesting a non-existent specific factor
# gives a clear error.
expect_error(refineFactor(fit_coupled, "B1", algorithm="mySVD", dim=2L),
    "not found")
