Xs <- mwTensor::toyModel("coupled_CP_Easy")

params <- new("CoupledMWCAParams",
    # 1. Data-wise setting
    Xs=Xs,
    mask=list(X1=NULL, X2=NULL, X3=NULL),
    weights=list(X1=1, X2=1, X3=1),
    # 2. Common Model setting
    common_model=list(
        X1=list(I1="A1", I2="A2"),
        X2=list(I2="A2", I3="A3", I4="A4"),
        X3=list(I4="A4", I5="A5")),
    # 3. Common Factor matrix-wise setting
    common_initial=list(A1=NULL, A2=NULL, A3=NULL, A4=NULL, A5=NULL),
    common_algorithms=list(A1="mySVD", A2="myALS_SVD", A3="myNMF",
        A4="myICA", A5="myCX"),
    common_iteration=list(A1=1, A2=10, A3=10, A4=10, A5=10),
    common_decomp=list(A1=TRUE, A2=TRUE, A3=TRUE, A4=TRUE, A5=TRUE),
    common_fix=list(A1=FALSE, A2=FALSE, A3=FALSE, A4=FALSE, A5=FALSE),
    common_dims=list(A1=3, A2=3, A3=5, A4=4, A5=4),
    common_transpose=list(A1=FALSE, A2=FALSE, A3=FALSE, A4=FALSE, A5=FALSE),
    common_coretype="Tucker",
    # 4. Specific Model setting
    specific_model=list(
        X1=list(J1="B1", J2="B2"),
        X2=list(J3="B3", J4="B4", J5="B5"),
        X3=list(J6="B6", J7="B7")),
    # 5. Specific Factor matrix-wise setting
    specific_initial=list(B1=NULL, B2=NULL, B3=NULL, B4=NULL, B5=NULL,
        B6=NULL, B7=NULL),
    specific_algorithms=list(B1="mySVD", B2="myALS_SVD", B3="myALS_SVD",
        B4="myNMF", B5="myICA", B6="myICA", B7="myCX"),
    specific_iteration=list(B1=1, B2=10, B3=10, B4=10, B5=10, B6=10, B7=10),
    specific_decomp=list(B1=TRUE, B2=TRUE, B3=TRUE, B4=TRUE, B5=TRUE,
        B6=TRUE, B7=TRUE),
    specific_fix=list(B1=FALSE, B2=FALSE, B3=FALSE, B4=FALSE, B5=FALSE,
        B6=FALSE, B7=FALSE),
    specific_dims=list(B1=1, B2=1, B3=1, B4=1, B5=1, B6=1, B7=1),
    specific_transpose=list(B1=FALSE, B2=FALSE, B3=FALSE, B4=FALSE,
        B5=FALSE, B6=FALSE, B7=FALSE),
    specific_coretype="Tucker",
    # 6. Other option
    specific=TRUE,
    thr=1e-10,
    viz=FALSE,
    figdir=NULL,
    verbose=FALSE)

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
expect_equal(dim(out@common_factors[[1]]), c(3, 20))
expect_equal(dim(out@common_factors[[2]]), c(3, 30))
expect_equal(dim(out@common_factors[[3]]), c(5, 30))
expect_equal(dim(out@common_factors[[4]]), c(4, 30))
expect_equal(dim(out@common_factors[[5]]), c(4, 25))
expect_equal(dim(out@common_cores[[1]]), c(3,3))
expect_equal(dim(out@common_cores[[2]]), c(3,5,4))
expect_equal(dim(out@common_cores[[3]]), c(4,4))
# Specific Factor Matrices
expect_equal(length(out@specific_model), 3)
expect_equal(length(out@specific_initial), 7)
expect_equal(length(out@specific_algorithms), 7)
expect_equal(length(out@specific_iteration), 7)
expect_equal(length(out@specific_decomp), 7)
expect_equal(length(out@specific_fix), 7)
expect_equal(length(out@specific_dims), 7)
expect_equal(length(out@specific_transpose), 7)
expect_identical(out@specific_coretype, "Tucker")
expect_equal(dim(out@specific_factors[[1]]), c(1,20))
expect_equal(dim(out@specific_factors[[2]]), c(1,30))
expect_equal(dim(out@specific_factors[[3]]), c(1,30))
expect_equal(dim(out@specific_factors[[4]]), c(1,30))
expect_equal(dim(out@specific_factors[[5]]), c(1,30))
expect_equal(dim(out@specific_factors[[6]]), c(1,30))
expect_equal(dim(out@specific_factors[[7]]), c(1,25))
expect_equal(dim(out@specific_cores[[1]]), c(1,1))
expect_equal(dim(out@specific_cores[[2]]), c(1,1,1))
expect_equal(dim(out@specific_cores[[3]]), c(1,1))
# Other option
expect_identical(out@specific, TRUE)
expect_identical(out@thr, 1e-10)
expect_equal(out@viz, FALSE)
expect_equal(out@figdir, NULL)
expect_equal(out@verbose, FALSE)
# Iteration
expect_true(is.numeric(out@rec_error))
expect_true(is.numeric(out@train_error))
expect_true(is.numeric(out@test_error))
expect_true(is.numeric(out@rel_change))

# Test Xs
Xs_dummy <- Xs
Xs_dummy[[1]][1,1] <- NA
params_dummy <- params
params_dummy@Xs <- Xs_dummy
expect_error(CoupledMWCA(params_dummy))

Xs_dummy[[1]][1,1] <- NaN
params_dummy <- params
params_dummy@Xs <- Xs_dummy
expect_error(CoupledMWCA(params_dummy))

Xs_dummy[[1]][1,1] <- Inf
params_dummy <- params
params_dummy@Xs <- Xs_dummy
expect_error(CoupledMWCA(params_dummy))

# Test mask
Ms <- Xs
Ms[[1]][] <- 1
Ms[[2]][] <- 1
Ms[[3]][] <- 1

params_dummy <- params
params_dummy@mask <- Ms
expect_error(expect_error(CoupledMWCA(params_dummy)))

Ms[[1]][1,1] <- 2
params_dummy <- params
params_dummy@mask <- Ms
expect_error(CoupledMWCA(params_dummy))

Ms[[1]][1,1] <- NA
params_dummy <- params
params_dummy@mask <- Ms
expect_error(CoupledMWCA(params_dummy))

Ms[[1]][1,1] <- Inf
params_dummy <- params
params_dummy@mask <- Ms
expect_error(CoupledMWCA(params_dummy))

# Test weights
params_dummy <- params
params_dummy@weights <- list(X1=1, X3=1)
expect_error(CoupledMWCA(params_dummy))

# Test model
params_dummy <- params
params_dummy@specific_model <- list(
            X1=list(J1="B1", J2="B2"),
            X2=list(J2="B2", J3="B2", J4="B4"),
            X3=list(J4="B4", J5="B5"))
expect_error(CoupledMWCA(params_dummy))

# Test initial
B1 <- mwTensor:::.randMat(1, 20)
B2 <- mwTensor:::.randMat(1, 30)
B2_dummy <- mwTensor:::.randMat(2, 30)
B3 <- mwTensor:::.randMat(1, 30)
B4 <- mwTensor:::.randMat(1, 30)
B5 <- mwTensor:::.randMat(1, 30)
B6 <- mwTensor:::.randMat(1, 30)
B7 <- mwTensor:::.randMat(1, 25)

params_dummy <- params
params_dummy@specific_initial <- list(B1=B1, B2=B2_dummy,
    B3=B3, B4=B4, B5=B5, B6=B6, B7=B7)
expect_error(CoupledMWCA(params_dummy))

# Test algorithms
params_dummy <- params
params_dummy@specific_algorithms <- list(B1="mySVDD", B2="myALS_SVD",
    B3="myNMF", B4="myICA", B5="myCX", B6="myCX", B7="myCX")
expect_error(CoupledMWCA(params_dummy))

params_dummy <- params
params_dummy@specific_algorithms <- list(B1="mySVDD", B2="myALS_SVD",
    B3="myNMF", B4="myICA", B5="myCX", B6="myCX")
expect_error(CoupledMWCA(params_dummy))

# Test iteration
params_dummy <- params
params_dummy@specific_iteration <- list(B1=1.1, B2=2, B3=3, B4=4,
    B5=5, B6=5, B7=5)
expect_error(CoupledMWCA(params_dummy))

# Test initial/fix
params_dummy <- params
params_dummy@specific_initial <- list(B1=B1, B2=B2, B3=B3, B4=B4,
    B5=B5, B6=B6, B7=B7)
params_dummy@specific_fix <- list(B1=TRUE, B2=FALSE, B3=TRUE, B4=FALSE,
        B5=TRUE, B6=FALSE, B7=TRUE)
out_dummy <- CoupledMWCA(params_dummy)

expect_equal(out_dummy@specific_factors$B1, B1)
expect_false(identical(out_dummy@specific_factors$B2, B2))
expect_equal(out_dummy@specific_factors$B3, B3)
expect_false(identical(out_dummy@specific_factors$B4, B4))
expect_equal(out_dummy@specific_factors$B5, B5)
expect_false(identical(out_dummy@specific_factors$B6, B6))
expect_equal(out_dummy@specific_factors$B7, B7)

# Test decomp
params_dummy <- params
params_dummy@specific_initial <- list(B1=B1, B2=B2, B3=B3, B4=B4,
    B5=B5, B6=B6, B7=B7)
params_dummy@specific_decomp <- list(B1=FALSE, B2=TRUE, B3=FALSE, B4=TRUE,
        B5=FALSE, B6=TRUE, B7=FALSE)
out_dummy <- CoupledMWCA(params_dummy)
expect_equal(diag(out_dummy@specific_factors$B1), 1)
expect_equal(diag(out_dummy@specific_factors$B3), 1)
expect_equal(diag(out_dummy@specific_factors$B5), 1)
expect_equal(diag(out_dummy@specific_factors$B7), 1)

# Test dims
params_dummy <- params
params_dummy@specific_dims <- list(B1=300, B2=3, B3=5, B4=4, B5=4, B6=4, B7=4)
expect_error(CoupledMWCA(params_dummy))

# Test transpose
params_dummy <- params
params_dummy@specific_transpose <- list(B1=FALSE, B2=TRUE, B3=FALSE, B4=TRUE,
        B5=FALSE, B6=TRUE, B7=FALSE)
expect_error(expect_error(CoupledMWCA(params_dummy)))

params_dummy <- params
params_dummy@specific_transpose <- list(B1="FALSE", B2=TRUE, B3=FALSE, B4=TRUE,
        B5=FALSE, B6=TRUE, B7=FALSE)
expect_error(CoupledMWCA(params_dummy))

# Test coretype
params_dummy <- params
params_dummy@specific_coretype <- "TUCKER"
expect_error(CoupledMWCA(params_dummy))

# Test thr
params_dummy <- params
params_dummy@thr <- 1E+100
expect_equal(length(CoupledMWCA(params_dummy)@rel_change), 2)

# Test CP
.diag <- function(out){
    num_modes <- mwTensor:::.ndim(out@data)
    min.s <- min(dim(out@data))
    tmp <- rep(0, min.s)
    cmd <- paste0("for(i in seq_len(min.s)){",
        "tmp[i] <- out@data[",
            paste(rep("i", length=num_modes), collapse=","), "]}")
    eval(parse(text=cmd))
    tmp
}

.nonDiagonal <- function(X, k=1){
    allels <- unique(as.vector(X@specific_cores[[k]]@data))
    diagels <- unique(.diag(X@specific_cores[[k]]))
    setdiff(allels, diagels)
}

params_cp <- params
params_cp@specific_dims <- list(B1=3, B2=3, B3=3, B4=3, B5=3, B6=3, B7=3)
params_cp@specific_coretype <- "CP"
res_cp <- CoupledMWCA(params_cp)

expect_equal(.nonDiagonal(res_cp, 1), 0)
expect_equal(.nonDiagonal(res_cp, 2), 0)
expect_equal(.nonDiagonal(res_cp, 3), 0)
