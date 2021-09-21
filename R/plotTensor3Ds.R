plotTensor3Ds <- function(Xs){
    num_Xs <- length(Xs)
    if(num_Xs > 30){
        message("Cannot visualized because of too many datasets")
    }else{
        layout(.layoutMat(num_Xs))
        for(n in seq(num_Xs)){
            dimX <- length(dim(Xs[[n]]))
            if(dimX == 2){
                if(is.array(Xs[[n]])){
                    plotTensor2D(as.tensor(Xs[[n]]),
                    	method="sd", sign="positive", thr=1.5)
            	}else{
	                plotTensor2D(Xs[[n]],
                    	method="sd", sign="positive", thr=1.5)
	            }
            }
            if(dimX == 3){
                if(is.array(Xs[[n]])){
                    plotTensor3D(as.tensor(Xs[[n]]),
                    	method="sd", sign="positive", thr=1.5)
                }else{
	                plotTensor3D(Xs[[n]],
                    	method="sd", sign="positive", thr=1.5)
                }
            }
            if(dimX >= 4){
                .SinglePlot(paste0(dimX, "th-order Tensor"))
            }
        }
    }
}

.layoutMat <- function(n){
	if(n <= 5){
		t(seq(n))
	}else{
		out <- matrix(0, ceiling(n/5), ncol=5)
		out <- t(out)
		out[seq(n)] <- seq(n)
		t(out)
	}
}

.SinglePlot <- function(x){
    plot(1, 1, col="white", ann=FALSE, xaxt="n", yaxt="n", axes=FALSE)
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    par(ps=30)
    text(1, 1, x, col="red")
}

.figheight <- function(n){
    500 * ceiling(n/5)
}