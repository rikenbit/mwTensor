## Class to holds parameters that will be passed on to MWCA
setClass("MWCAParams",
    representation=representation(
        X="array",
        mask="ANY",
        algorithms="character",
        dims="numeric",
        transpose="logical",
        viz="logical",
        figdir="ANY"
    ),
    prototype=prototype(
        X=array(runif(4*5*6), dim=c(4,5,6)),
        mask=NULL,
        algorithms=c("myALS_SVD", "myNMF", "myICA"),
        dims=c(2, 3, 4),
        transpose=FALSE,
        viz=FALSE,
        figdir=NULL
    )
)

## Class to holds parameters that will be passed on to Coupled MWCA
setClass("CoupledMWCAParams",
    representation=representation(
        # 1. Data-wise setting
        Xs="list",
        mask="list",
        weights="list",
        # 2. Common Model setting
        common_model="list",
        # 3. Common Factor matrix-wise setting
        common_initial="list",
        common_algorithms="list",
        common_iteration="list",
        common_decomp="list",
        common_fix="list",
        common_dims="list",
        common_transpose="list",
        common_coretype="character",
        # 4. Specific Model setting
        specific_model="list",
        # 5. Specific Factor matrix-wise setting
        specific_initial="list",
        specific_algorithms="list",
        specific_iteration="list",
        specific_decomp="list",
        specific_fix="list",
        specific_dims="list",
        specific_transpose="list",
        specific_coretype="character",
        # 6. Other option
        specific="logical",
        thr="numeric",
        viz="logical",
        figdir="ANY",
        verbose="logical"
    ),
    prototype=prototype(
        # 1. Data-wise setting
        Xs=list(
            X1=array(runif(7*4), dim=c(7,4)),
            X2=array(runif(4*5*6), dim=c(4,5,6)),
            X3=array(runif(6*8), dim=c(6,8))),
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
        specific_decomp=list(B1=TRUE, B2=TRUE, B3=TRUE, B4=TRUE, B5=TRUE, B6=TRUE, B7=TRUE),
        specific_fix=list(B1=FALSE, B2=FALSE, B3=FALSE, B4=FALSE, B5=FALSE, B6=FALSE, B7=FALSE),
        specific_dims=list(B1=1, B2=1, B3=1, B4=1, B5=1, B6=1, B7=1),
        specific_transpose=list(B1=FALSE, B2=FALSE, B3=FALSE, B4=FALSE, B5=FALSE, B6=FALSE, B7=FALSE),
        specific_coretype="Tucker",
        # 6. Other option
        specific=FALSE,
        thr=1e-10,
        viz=FALSE,
        figdir=NULL,
        verbose=FALSE
    )
)

## Class to holds MWCA results for plot
setClass("MWCAResult",
    representation=representation(
        algorithms="character",
        dims="numeric",
        transpose="logical",
        viz="logical",
        figdir="ANY",
        factors="list",
        core="array",
        rec_error="numeric",
        train_error="numeric",
        test_error="numeric"
    )
)

## Class to holds Coupled MWCA results for plot
setClass("CoupledMWCAResult",
    representation=representation(
        # Data-wise setting
        weights="list",
        # Common Factor Matrices
        common_model="list",
        common_initial="list",
        common_algorithms="list",
        common_iteration="list",
        common_decomp="list",
        common_fix="list",
        common_dims="list",
        common_transpose="list",
        common_coretype="character",
        common_factors="list",
        common_cores="list",
        # Specific Factor Matrices
        specific_model="list",
        specific_initial="list",
        specific_algorithms="list",
        specific_iteration="list",
        specific_decomp="list",
        specific_fix="list",
        specific_dims="list",
        specific_transpose="list",
        specific_coretype="character",
        specific_factors="list",
        specific_cores="list",
        # Other option
        specific="logical",
        thr="numeric",
        viz="logical",
        figdir="ANY",
        verbose="logical",
        # Iteration
        rec_error="numeric",
        train_error="numeric",
        test_error="numeric",
        rel_change="numeric"
    )
)
