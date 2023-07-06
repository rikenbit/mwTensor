defaultMWCAParams <- function(X){
    stopifnot(is.array(X))
    # Default Parameters
    new("MWCAParams",
        X=X,
        mask=NULL,
        pseudocount=.Machine$double.eps,
        algorithms=rep("mySVD", length=length(dim(X))),
        dims=sapply(dim(X), function(x){min(x, 2)}),
        transpose=FALSE,
        viz=FALSE,
        figdir=NULL)
}