## Class to holds parameters that will be passed on to MWCA
setClass("MWCAParams",
    representation=representation(
        X="array",
        mask="ANY",
        algorithms="character",
        decomp="logical",
        fix="logical",
        dims="numeric",
        transpose="logical",
        viz="logical",
        figdir="ANY"
    ),
    prototype=prototype(
        X=array(runif(4*5*6), dim=c(4,5,6)),
        mask=NULL,
        algorithms=c("myALS_SVD", "myNMF", "myICA"),
        decomp=c(TRUE, TRUE, TRUE),
        fix=c(FALSE, FALSE, FALSE),
        dims=c(2, 3, 4),
        transpose=FALSE,
        viz=FALSE,
        figdir=NULL
    )
)

## Class to holds parameters that will be passed on to Coupled MWCA
setClass("CoupledMWCAParams",
    representation=representation(
        Xs="list",
        dimnames="list",
        mask="list",
        weights="list",
        initial="list",
        algorithms="list",
        iteration="list",
        decomp="list",
        fix="list",
        dims="list",
        transpose="logical",
        thr="numeric",
        viz="logical",
        figdir="ANY",
        verbose="logical"
    ),
    prototype=prototype(
        # Data matrix-wise setting
        Xs=list(
            X1=array(runif(7*4), dim=c(7,4)),
            X2=array(runif(4*5*6), dim=c(4,5,6)),
            X3=array(runif(6*8), dim=c(6,8))),
        dimnames=list(
            X1=c("I1", "I2"),
            X2=c("I2", "I3", "I4"),
            X3=c("I4", "I5")),
        mask=list(
            X1=NULL,
            X2=NULL,
            X3=NULL),
        weights=list(
            X1=1,
            X2=1,
            X3=1),
        # Factor matrix-wise setting
        initial=list(
            X1=list(A1=NULL, A2=NULL),
            X2=list(A2=NULL, A3=NULL, A4=NULL),
            X3=list(A4=NULL, A5=NULL)),
        algorithms=list(
            X1=list(A1="mySVD", A2="myALS_SVD"),
            X2=list(A2="myALS_SVD", A3="myNMF", A4="myICA"),
            X3=list(A4="myICA", A5="myCX")),
        iteration=list(
            X1=c(A1=1, A2=10),
            X2=c(A2=10, A3=10, A4=10),
            X3=c(A4=10, A5=10)),
        decomp=list(
            X1=c(A1=TRUE, A2=TRUE),
            X2=c(A2=TRUE, A3=TRUE, A4=TRUE),
            X3=c(A4=TRUE, A5=TRUE)),
        fix=list(
            X1=c(A1=FALSE, A2=FALSE),
            X2=c(A2=FALSE, A3=FALSE, A4=FALSE),
            X3=c(A4=FALSE, A5=FALSE)),
        dims=list(
            X1=c(A1=3, A2=3),
            X2=c(A2=2, A3=5, A4=4),
            X3=c(A4=4, A5=4)),
        transpose=FALSE,
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
        decomp="logical",
        fix="logical",
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
        dimnames="list",
        weights="list",
        initial="list",
        algorithms="list",
        decomp="list",
        fix="list",
        dims="list",
        transpose="logical",
        viz="logical",
        figdir="ANY",
        verbose="logical",
        factors="list",
        cores="list",
        rec_error="numeric",
        train_error="numeric",
        test_error="numeric",
        rel_change="numeric"
    )
)
