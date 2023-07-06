# Xs <- mwTensor::toyModel("coupled_Complex_Easy")
Xs <- mwTensor::toyModel("coupled_Complex_Hard")
Xs[[3]] <- Xs[[3]] + array(rbinom(20*23*24,1000,0.1), dim=c(20,23,24))

# X1
A1 <- mwTensor:::.randMat(5, 15)
A2 <- mwTensor:::.randMat(5, 20)
A3 <- mwTensor:::.randMat(5, 25)
A4 <- mwTensor:::.randMat(5, 30)
# X2
A5 <- mwTensor:::.randMat(5, 15)
A6 <- mwTensor:::.randMat(5, 21)
A7 <- mwTensor:::.randMat(5, 22)
# X3
A8 <- mwTensor:::.randMat(5, 20)
A9 <- mwTensor:::.randMat(5, 23)
A10 <- mwTensor:::.randMat(5, 24)
# X4
A11 <- mwTensor:::.randMat(5, 25)
A12 <- mwTensor:::.randMat(5, 25)
A13 <- mwTensor:::.randMat(5, 26)
# X5
A14 <- mwTensor:::.randMat(5, 30)
A15 <- mwTensor:::.randMat(5, 27)
A16 <- mwTensor:::.randMat(5, 28)
# X6
A17 <- mwTensor:::.randMat(5, 21)
A18 <- mwTensor:::.randMat(5, 11)
# X7
A19 <- mwTensor:::.randMat(5, 22)
A20 <- mwTensor:::.randMat(5, 12)
# X8
A21 <- mwTensor:::.randMat(5, 23)
A22 <- mwTensor:::.randMat(5, 13)
# X9
A23 <- mwTensor:::.randMat(5, 24)
A24 <- mwTensor:::.randMat(5, 14)
# X10
A25 <- mwTensor:::.randMat(5, 25)
A26 <- mwTensor:::.randMat(5, 15)
# X11
A27 <- mwTensor:::.randMat(5, 26)
A28 <- mwTensor:::.randMat(5, 16)
# X12
A29 <- mwTensor:::.randMat(5, 27)
A30 <- mwTensor:::.randMat(5, 17)
# X13
A31 <- mwTensor:::.randMat(5, 28)
A32 <- mwTensor:::.randMat(5, 18)

# # X1
# A1 <- .randMat(5, 15)
# A2 <- .randMat(5, 20)
# A3 <- .randMat(5, 25)
# A4 <- .randMat(5, 30)
# # X2
# A5 <- .randMat(5, 15)
# A6 <- .randMat(5, 21)
# A7 <- .randMat(5, 22)
# # X3
# A8 <- .randMat(5, 20)
# A9 <- .randMat(5, 23)
# A10 <- .randMat(5, 24)
# # X4
# A11 <- .randMat(5, 25)
# A12 <- .randMat(5, 25)
# A13 <- .randMat(5, 26)
# # X5
# A14 <- .randMat(5, 30)
# A15 <- .randMat(5, 27)
# A16 <- .randMat(5, 28)
# # X6
# A17 <- .randMat(5, 21)
# A18 <- .randMat(5, 11)
# # X7
# A19 <- .randMat(5, 22)
# A20 <- .randMat(5, 12)
# # X8
# A21 <- .randMat(5, 23)
# A22 <- .randMat(5, 13)
# # X9
# A23 <- .randMat(5, 24)
# A24 <- .randMat(5, 14)
# # X10
# A25 <- .randMat(5, 25)
# A26 <- .randMat(5, 15)
# # X11
# A27 <- .randMat(5, 26)
# A28 <- .randMat(5, 16)
# # X12
# A29 <- .randMat(5, 27)
# A30 <- .randMat(5, 17)
# # X13
# A31 <- .randMat(5, 28)
# A32 <- .randMat(5, 18)

params <- new("CoupledMWCAParams",
    # Data-wise setting
    Xs=Xs,
    mask=list(X1=NULL, X2=NULL, X3=NULL, X4=NULL, X5=NULL, X6=NULL,
        X7=NULL, X8=NULL, X9=NULL, X10=NULL, X11=NULL,
        X12=NULL, X13=NULL),
    pseudocount=1E-10,
    weights=list(X1=1, X2=1, X3=1, X4=1, X5=1, X6=1,
        X7=1, X8=1, X9=1, X10=1, X11=1,
        X12=1, X13=1),
    # Common Factor Matrices
    common_model=list(X1=list(I1="A1", I2="A2", I3="A3", I4="A4"),
        X2=list(I5="A5", I6="A6", I7="A7"),
        X3=list(I8="A8", I9="A9", I10="A10"),
        X4=list(I11="A11", I12="A12", I13="A13"),
        X5=list(I14="A14", I15="A15", I16="A16"),
        X6=list(I17="A17", I18="A18"),
        X7=list(I19="A19", I20="A20"),
        X8=list(I21="A21", I22="A22"),
        X9=list(I23="A23", I24="A24"),
        X10=list(I25="A25", I126="A26"),
        X11=list(I27="A27", I28="A28"),
        X12=list(I29="A29", I30="A30"),
        X13=list(I31="A31", I32="A32")),
    common_initial=list(A1=A1, A2=A2, A3=A3, A4=A4, A5=A5,
        A6=A6, A7=A7, A8=A8, A9=A9, A10=A10,
        A11=A11, A12=A12, A13=A13, A14=A14, A15=A15,
        A16=A16, A17=A17, A18=A18, A19=A19, A20=A20,
        A21=A21, A22=A22, A23=A23, A24=A24, A25=A25,
        A26=A26, A27=A27, A28=A28, A29=A29, A30=A30,
        A31=A31, A32=A32),
    common_algorithms=list(A1="myNMF", A2="myNMF", A3="myNMF", A4="myNMF", A5="myNMF",
        A6="myNMF", A7="myNMF", A8="myNMF", A9="myNMF", A10="myNMF",
        A11="myNMF", A12="myNMF", A13="myNMF", A14="myNMF", A15="myNMF",
        A16="myNMF", A17="myNMF", A18="myNMF", A19="myNMF", A20="myNMF",
        A21="myNMF", A22="myNMF", A23="myNMF", A24="myNMF", A25="myNMF",
        A26="myNMF", A27="myNMF", A28="myNMF", A29="myNMF", A30="myNMF",
        A31="myNMF", A32="myNMF"),
    common_iteration=list(A1=20, A2=20, A3=20, A4=20, A5=20,
        A6=20, A7=20, A8=20, A9=20, A10=20,
        A11=20, A12=20, A13=20, A14=20, A15=20,
        A16=20, A17=20, A18=20, A19=20, A20=20,
        A21=20, A22=20, A23=20, A24=20, A25=20,
        A26=20, A27=20, A28=20, A29=20, A30=20,
        A31=20, A32=20),
    common_decomp=list(A1=TRUE, A2=TRUE, A3=TRUE, A4=TRUE, A5=TRUE,
        A6=TRUE, A7=TRUE, A8=TRUE, A9=TRUE, A10=TRUE,
        A11=TRUE, A12=TRUE, A13=TRUE, A14=TRUE, A15=TRUE,
        A16=TRUE, A17=TRUE, A18=TRUE, A19=TRUE, A20=TRUE,
        A21=TRUE, A22=TRUE, A23=TRUE, A24=TRUE, A25=TRUE,
        A26=TRUE, A27=TRUE, A28=TRUE, A29=TRUE, A30=TRUE,
        A31=TRUE, A32=TRUE),
    common_fix=list(A1=FALSE, A2=FALSE, A3=FALSE, A4=FALSE, A5=FALSE,
        A6=FALSE, A7=FALSE, A8=FALSE, A9=FALSE, A10=FALSE,
        A11=FALSE, A12=FALSE, A13=FALSE, A14=FALSE, A15=FALSE,
        A16=FALSE, A17=FALSE, A18=FALSE, A19=FALSE, A20=FALSE,
        A21=FALSE, A22=FALSE, A23=FALSE, A24=FALSE, A25=FALSE,
        A26=FALSE, A27=FALSE, A28=FALSE, A29=FALSE, A30=FALSE,
        A31=FALSE, A32=FALSE),
    common_dims=list(A1=5, A2=5, A3=5, A4=5, A5=5,
        A6=5, A7=5, A8=5, A9=5, A10=5,
        A11=5, A12=5, A13=5, A14=5, A15=5,
        A16=5, A17=5, A18=5, A19=5, A20=5,
        A21=5, A22=5, A23=5, A24=5, A25=5,
        A26=5, A27=5, A28=5, A29=5, A30=5,
        A31=5, A32=5),
    common_transpose=list(A1=FALSE, A2=FALSE, A3=FALSE, A4=FALSE, A5=FALSE,
        A6=FALSE, A7=FALSE, A8=FALSE, A9=FALSE, A10=FALSE,
        A11=FALSE, A12=FALSE, A13=FALSE, A14=FALSE, A15=FALSE,
        A16=FALSE, A17=FALSE, A18=FALSE, A19=FALSE, A20=FALSE,
        A21=FALSE, A22=FALSE, A23=FALSE, A24=FALSE, A25=FALSE,
        A26=FALSE, A27=FALSE, A28=FALSE, A29=FALSE, A30=FALSE,
        A31=FALSE, A32=FALSE),
    common_coretype="Tucker",
    # Other option
    specific=FALSE,
    thr=1e-10,
    viz=TRUE,
    # figdir=".",
    verbose=TRUE)

out <- CoupledMWCA(params)
expect_equal(is(out), "CoupledMWCAResult")

rec <- mwTensor:::.recTensors(out@common_cores, out@common_factors,
    out@common_model)
expect_equal(length(rec), 13)
