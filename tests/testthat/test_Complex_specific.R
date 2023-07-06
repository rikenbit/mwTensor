# Xs <- mwTensor::toyModel("coupled_Complex_Easy")
Xs <- mwTensor::toyModel("coupled_Complex_Hard")
Xs[[3]] <- Xs[[3]] + array(rbinom(20*23*24,1000,0.1), dim=c(20,23,24))

A1 <- mwTensor:::.randMat(7, 15)
A2 <- mwTensor:::.randMat(7, 20)
A3 <- mwTensor:::.randMat(7, 25)
A4 <- mwTensor:::.randMat(7, 30)
A5 <- mwTensor:::.randMat(7, 21)
A6 <- mwTensor:::.randMat(7, 22)
A7 <- mwTensor:::.randMat(7, 23)
A8 <- mwTensor:::.randMat(7, 24)
A9 <- mwTensor:::.randMat(7, 25)
A10 <- mwTensor:::.randMat(7, 26)
A11 <- mwTensor:::.randMat(7, 27)
A12 <- mwTensor:::.randMat(7, 28)
A13 <- mwTensor:::.randMat(7, 11)
A14 <- mwTensor:::.randMat(7, 12)
A15 <- mwTensor:::.randMat(7, 13)
A16 <- mwTensor:::.randMat(7, 14)
A17 <- mwTensor:::.randMat(7, 15)
A18 <- mwTensor:::.randMat(7, 16)
A19 <- mwTensor:::.randMat(7, 17)
A20 <- mwTensor:::.randMat(7, 18)

B1 <- mwTensor:::.randMat(2, 15)
B2 <- mwTensor:::.randMat(2, 20)
B3 <- mwTensor:::.randMat(2, 25)
B4 <- mwTensor:::.randMat(2, 30)
B5 <- mwTensor:::.randMat(2, 15)
B6 <- mwTensor:::.randMat(2, 21)
B7 <- mwTensor:::.randMat(2, 22)
B8 <- mwTensor:::.randMat(2, 20)
B9 <- mwTensor:::.randMat(2, 23)
B10 <- mwTensor:::.randMat(2, 24)
B11 <- mwTensor:::.randMat(2, 25)
B12 <- mwTensor:::.randMat(2, 25)
B13 <- mwTensor:::.randMat(2, 26)
B14 <- mwTensor:::.randMat(2, 30)
B15 <- mwTensor:::.randMat(2, 27)
B16 <- mwTensor:::.randMat(2, 28)
B17 <- mwTensor:::.randMat(2, 21)
B18 <- mwTensor:::.randMat(2, 11)
B19 <- mwTensor:::.randMat(2, 22)
B20 <- mwTensor:::.randMat(2, 12)
B21 <- mwTensor:::.randMat(2, 23)
B22 <- mwTensor:::.randMat(2, 13)
B23 <- mwTensor:::.randMat(2, 24)
B24 <- mwTensor:::.randMat(2, 14)
B25 <- mwTensor:::.randMat(2, 25)
B26 <- mwTensor:::.randMat(2, 15)
B27 <- mwTensor:::.randMat(2, 26)
B28 <- mwTensor:::.randMat(2, 16)
B29 <- mwTensor:::.randMat(2, 27)
B30 <- mwTensor:::.randMat(2, 17)
B31 <- mwTensor:::.randMat(2, 28)
B32 <- mwTensor:::.randMat(2, 18)

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
        X2=list(I1="A1", I5="A5", I6="A6"),
        X3=list(I2="A2", I7="A7", I8="A8"),
        X4=list(I3="A3", I9="A9", I10="A10"),
        X5=list(I4="A4", I11="A11", I12="A12"),
        X6=list(I5="A5", I13="A13"),
        X7=list(I6="A6", I14="A14"),
        X8=list(I7="A7", I15="A15"),
        X9=list(I8="A8", I16="A16"),
        X10=list(I9="A9", I17="A17"),
        X11=list(I10="A10", I18="A18"),
        X12=list(I11="A11", I19="A19"),
        X13=list(I12="A12", I20="A20")),
    common_initial=list(A1=A1, A2=A2, A3=A3, A4=A4, A5=A5,
        A6=A6, A7=A7, A8=A8, A9=A9, A10=A10,
        A11=A11, A12=A12, A13=A13, A14=A14, A15=A15,
        A16=A16, A17=A17, A18=A18, A19=A19, A20=A20),
    common_algorithms=list(A1="myNMF", A2="myNMF", A3="myNMF", A4="myNMF", A5="myNMF",
        A6="myNMF", A7="myNMF", A8="myNMF", A9="myNMF", A10="myNMF",
        A11="myNMF", A12="myNMF", A13="myNMF", A14="myNMF", A15="myNMF",
        A16="myNMF", A17="myNMF", A18="myNMF", A19="myNMF", A20="myNMF"),
    common_iteration=list(A1=20, A2=20, A3=20, A4=20, A5=20,
        A6=20, A7=20, A8=20, A9=20, A10=20,
        A11=20, A12=20, A13=20, A14=20, A15=20,
        A16=20, A17=20, A18=20, A19=20, A20=20),
    common_decomp=list(A1=TRUE, A2=TRUE, A3=TRUE, A4=TRUE, A5=TRUE,
        A6=TRUE, A7=TRUE, A8=TRUE, A9=TRUE, A10=TRUE,
        A11=TRUE, A12=TRUE, A13=TRUE, A14=TRUE, A15=TRUE,
        A16=TRUE, A17=TRUE, A18=TRUE, A19=TRUE, A20=TRUE),
    common_fix=list(A1=FALSE, A2=FALSE, A3=FALSE, A4=FALSE, A5=FALSE,
        A6=FALSE, A7=FALSE, A8=FALSE, A9=FALSE, A10=FALSE,
        A11=FALSE, A12=FALSE, A13=FALSE, A14=FALSE, A15=FALSE,
        A16=FALSE, A17=FALSE, A18=FALSE, A19=FALSE, A20=FALSE),
    common_dims=list(A1=7, A2=7, A3=7, A4=7, A5=7,
        A6=7, A7=7, A8=7, A9=7, A10=7,
        A11=7, A12=7, A13=7, A14=7, A15=7,
        A16=7, A17=7, A18=7, A19=7, A20=7),
    common_transpose=list(A1=FALSE, A2=FALSE, A3=FALSE, A4=FALSE, A5=FALSE,
        A6=FALSE, A7=FALSE, A8=FALSE, A9=FALSE, A10=FALSE,
        A11=FALSE, A12=FALSE, A13=FALSE, A14=FALSE, A15=FALSE,
        A16=FALSE, A17=FALSE, A18=FALSE, A19=FALSE, A20=FALSE),
    common_coretype="Tucker",
    # Specific Factor Matrices
    specific_model=list(X1=list(J1="B1", J2="B2", J3="B3", J4="B4"),
        X2=list(J5="B5", J6="B6", J7="B7"),
        X3=list(J8="B8", J9="B9", J10="B10"),
        X4=list(J11="B11", J12="B12", J13="B13"),
        X5=list(J14="B14", J15="B15", J16="B16"),
        X6=list(J17="B17", J18="B18"),
        X7=list(J19="B19", J20="B20"),
        X8=list(J21="B21", J22="B22"),
        X9=list(J23="B23", J24="B24"),
        X10=list(J25="B25", J26="B26"),
        X11=list(J27="B27", J28="B28"),
        X12=list(J29="B29", J30="B30"),
        X13=list(J31="B31", J32="B32")),
    specific_initial=list(B1=B1, B2=B2, B3=B3, B4=B4, B5=B5,
        B6=B6, B7=B7, B8=B8, B9=B9, B10=B10,
        B11=B11, B12=B12, B13=B13, B14=B14, B15=B15,
        B16=B16, B17=B17, B18=B18, B19=B19, B20=B20,
        B21=B21, B22=B22, B23=B23, B24=B24, B25=B25,
        B26=B26, B27=B27, B28=B28, B29=B29, B30=B30,
        B31=B31, B32=B32),
    specific_algorithms=list(
        B1="myNMF", B2="myNMF", B3="myNMF", B4="myNMF", B5="myNMF",
        B6="myNMF", B7="myNMF", B8="myNMF", B9="myNMF", B10="myNMF",
        B11="myNMF", B12="myNMF", B13="myNMF", B14="myNMF", B15="myNMF",
        B16="myNMF", B17="myNMF", B18="myNMF", B19="myNMF", B20="myNMF",
        B21="myNMF", B22="myNMF", B23="myNMF", B24="myNMF", B25="myNMF",
        B26="myNMF", B27="myNMF", B28="myNMF", B29="myNMF", B30="myNMF",
        B31="myNMF", B32="myNMF"),
    specific_iteration=list(B1=20, B2=20, B3=20, B4=20, B5=20,
        B6=20, B7=20, B8=20, B9=20, B10=20,
        B11=20, B12=20, B13=20, B14=20, B15=20,
        B16=20, B17=20, B18=20, B19=20, B20=20,
        B21=20, B22=20, B23=20, B24=20, B25=20,
        B26=20, B27=20, B28=20, B29=20, B30=20,
        B31=20, B32=20),
    specific_decomp=list(B1=TRUE, B2=TRUE, B3=TRUE, B4=TRUE, B5=TRUE,
        B6=TRUE, B7=TRUE, B8=TRUE, B9=TRUE, B10=TRUE,
        B11=TRUE, B12=TRUE, B13=TRUE, B14=TRUE, B15=TRUE,
        B16=TRUE, B17=TRUE, B18=TRUE, B19=TRUE, B20=TRUE,
        B21=TRUE, B22=TRUE, B23=TRUE, B24=TRUE, B25=TRUE,
        B26=TRUE, B27=TRUE, B28=TRUE, B29=TRUE, B30=TRUE,
        B31=TRUE, B32=TRUE),
    specific_fix=list(B1=FALSE, B2=FALSE, B3=FALSE, B4=FALSE, B5=FALSE,
        B6=FALSE, B7=FALSE, B8=FALSE, B9=FALSE, B10=FALSE,
        B11=FALSE, B12=FALSE, B13=FALSE, B14=FALSE, B15=FALSE,
        B16=FALSE, B17=FALSE, B18=FALSE, B19=FALSE, B20=FALSE,
        B21=FALSE, B22=FALSE, B23=FALSE, B24=FALSE, B25=FALSE,
        B26=FALSE, B27=FALSE, B28=FALSE, B29=FALSE, B30=FALSE,
        B31=FALSE, B32=FALSE),
    specific_dims=list(B1=2, B2=2, B3=2, B4=2, B5=2,
        B6=2, B7=2, B8=2, B9=2, B10=2,
        B11=2, B12=2, B13=2, B14=2, B15=2,
        B16=2, B17=2, B18=2, B19=2, B20=2,
        B21=2, B22=2, B23=2, B24=2, B25=2,
        B26=2, B27=2, B28=2, B29=2, B30=2,
        B31=2, B32=2),
    specific_transpose=list(B1=FALSE, B2=FALSE, B3=FALSE, B4=FALSE, B5=FALSE,
        B6=FALSE, B7=FALSE, B8=FALSE, B9=FALSE, B10=FALSE,
        B11=FALSE, B12=FALSE, B13=FALSE, B14=FALSE, B15=FALSE,
        B16=FALSE, B17=FALSE, B18=FALSE, B19=FALSE, B20=FALSE,
        B21=FALSE, B22=FALSE, B23=FALSE, B24=FALSE, B25=FALSE,
        B26=FALSE, B27=FALSE, B28=FALSE, B29=FALSE, B30=FALSE,
        B31=FALSE, B32=FALSE),
    specific_coretype="Tucker",
    # Other option
    specific=TRUE,
    thr=1e-10,
    viz=TRUE,
    # figdir=".",
    verbose=TRUE)

out <- CoupledMWCA(params)
expect_equal(is(out), "CoupledMWCAResult")

rec <- mwTensor:::.recTensors(out@common_cores, out@common_factors,
    out@common_model)
expect_equal(length(rec), 13)
