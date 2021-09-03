# CP type
.coupled_CP_Easy <- function(){
    X1 <- matrix(rpois(20*30, lambda=1), nrow=20, ncol=30)
    X2 <- nnTensor::toyModel("CP")@data
    X3 <- matrix(rpois(30*25, lambda=1), nrow=30, ncol=25)

    X1[1:5, 1:5] <- rpois(5*5, lambda=100)
    X1[6:10, 10:14] <- rpois(5*5, lambda=100)
    X1[11:15, 18:22] <- rpois(5*5, lambda=100)
    X1[16:20, 26:30] <- rpois(5*5, lambda=100)

    X3[1:5, 1:5] <- rpois(5*5, lambda=100)
    X3[10:14, 6:10] <- rpois(5*5, lambda=100)
    X3[18:22, 11:15] <- rpois(5*5, lambda=100)
    X3[26:30, 16:20] <- rpois(5*5, lambda=100)

    dimnames(X1) <- list(I1=seq(20), I2=seq(30))
    dimnames(X2) <- list(I2=seq(30), I3=seq(30), I4=seq(30))
    dimnames(X3) <- list(I4=seq(30), I5=seq(25))

    list(X1=X1, X2=X2, X3=X3)
}

.coupled_CP_Hard <- function(){
    X1 <- matrix(rpois(20*30, lambda=1), nrow=20, ncol=30)
    X2 <- nnTensor::toyModel("CP")@data
    X3 <- matrix(rpois(30*25, lambda=1), nrow=30, ncol=25)

    X1[1:5, 1:5] <- rpois(5*5, lambda=100)
    X1[6:10, 10:14] <- rpois(5*5, lambda=100)
    X1[11:15, 18:22] <- rpois(5*5, lambda=100)
    X1[16:20, 26:30] <- rpois(5*5, lambda=100)
    X1[1:10, 15:17] <- rpois(10*3, lambda=300) # X1 Specific

    X3[1:5, 1:5] <- rpois(5*5, lambda=100)
    X3[10:14, 6:10] <- rpois(5*5, lambda=100)
    X3[18:22, 11:15] <- rpois(5*5, lambda=100)
    X3[26:30, 16:20] <- rpois(5*5, lambda=100)
    X3[15:17, 21:25] <- rpois(3*5, lambda=300) # X3 Specific

    dimnames(X1) <- list(I1=seq(20), I2=seq(30))
    dimnames(X2) <- list(I2=seq(30), I3=seq(30), I4=seq(30))
    dimnames(X3) <- list(I4=seq(30), I5=seq(25))

    list(X1=X1, X2=X2, X3=X3)
}

# Tucker type
.coupled_Tucker_Easy <- function(){
    X1 <- matrix(rpois(30*50, lambda=1), nrow=30, ncol=50)
    X2 <- nnTensor::toyModel("Tucker")@data
    X3 <- matrix(rpois(50*25, lambda=1), nrow=50, ncol=25)

    X1[1:5, 1:5] <- rpois(5*5, lambda=100)
    X1[6:10, 10:15] <- rpois(5*6, lambda=100)
    X1[11:15, 30:35] <- rpois(5*6, lambda=100)
    X1[16:20, 46:50] <- rpois(5*5, lambda=100)

    X3[1:5, 1:5] <- rpois(5*5, lambda=100)
    X3[16:20, 6:10] <- rpois(5*5, lambda=100)
    X3[30:35, 11:15] <- rpois(6*5, lambda=100)
    X3[46:50, 16:20] <- rpois(5*5, lambda=100)

    dimnames(X1) <- list(I1=seq(30), I2=seq(50))
    dimnames(X2) <- list(I2=seq(50), I3=seq(50), I4=seq(50))
    dimnames(X3) <- list(I4=seq(50), I5=seq(25))

    list(X1=X1, X2=X2, X3=X3)
}

.coupled_Tucker_Hard <- function(){
    X1 <- matrix(rpois(30*50, lambda=1), nrow=30, ncol=50)
    X2 <- nnTensor::toyModel("Tucker")@data
    X3 <- matrix(rpois(50*25, lambda=1), nrow=50, ncol=25)

    X1[1:5, 1:5] <- rpois(5*5, lambda=100)
    X1[6:10, 10:15] <- rpois(5*6, lambda=100)
    X1[11:15, 30:35] <- rpois(5*6, lambda=100)
    X1[16:20, 46:50] <- rpois(5*5, lambda=100)
    X1[20:30, 16:20] <- rpois(11*5, lambda=300) # X1 Specific

    X3[1:5, 1:5] <- rpois(5*5, lambda=100)
    X3[16:20, 6:10] <- rpois(5*5, lambda=100)
    X3[30:35, 11:15] <- rpois(6*5, lambda=100)
    X3[46:50, 16:20] <- rpois(5*5, lambda=100)
    X3[6:10, 21:22] <- rpois(5*2, lambda=300) # X3 Specific
    X3[11:15, 23:25] <- rpois(5*3, lambda=300) # X3 Specific

    dimnames(X1) <- list(I1=seq(30), I2=seq(50))
    dimnames(X2) <- list(I2=seq(50), I3=seq(50), I4=seq(50))
    dimnames(X3) <- list(I4=seq(50), I5=seq(25))

    list(X1=X1, X2=X2, X3=X3)
}

.coupled_Complex <- function(){
    X1 <- array(rpois(15*20*25*30, lambda=1), dim=c(15,20,25,30))

    X2 <- array(rpois(15*21*22, lambda=1), dim=c(15,21,22))
    X3 <- array(rpois(20*23*24, lambda=1), dim=c(20,23,24))
    X4 <- array(rpois(25*25*26, lambda=1), dim=c(25,25,26))
    X5 <- array(rpois(30*27*28, lambda=1), dim=c(30,27,28))

    X6 <- array(rpois(21*11, lambda=1), dim=c(21,11))
    X7 <- array(rpois(22*12, lambda=1), dim=c(22,12))
    X8 <- array(rpois(23*13, lambda=1), dim=c(23,13))
    X9 <- array(rpois(24*14, lambda=1), dim=c(24,14))

    X10 <- array(rpois(25*15, lambda=1), dim=c(25,15))
    X11 <- array(rpois(26*16, lambda=1), dim=c(26,16))
    X12 <- array(rpois(27*17, lambda=1), dim=c(27,17))
    X13 <- array(rpois(28*18, lambda=1), dim=c(28,18))

    X1[1:5,1:5,1:5,1:5] <- rpois(5*5*5*5, lambda=50)

    X2[1:5,1:5,1:5] <- rpois(5*5*5, lambda=15)
    X3[1:5,1:5,1:5] <- rpois(5*5*5, lambda=20)
    X4[1:5,1:5,1:5] <- rpois(5*5*5, lambda=30)
    X5[1:5,1:5,1:5] <- rpois(5*5*5, lambda=35)

    X6[1:5,1:5] <- rpois(5*5, lambda=50)
    X7[1:5,1:5] <- rpois(5*5, lambda=50)
    X8[1:5,1:5] <- rpois(5*5, lambda=50)
    X9[1:5,1:5] <- rpois(5*5, lambda=50)
    X10[1:5,1:5] <- rpois(5*5, lambda=50)
    X11[1:5,1:5] <- rpois(5*5, lambda=50)
    X12[1:5,1:5] <- rpois(5*5, lambda=50)
    X13[1:5,1:5] <- rpois(5*5, lambda=50)

    dimnames(X1) <- list(I1=seq(15), I2=seq(20), I3=seq(25), I4=seq(30))

    dimnames(X2) <- list(I1=seq(15), I5=seq(21), I6=seq(22))
    dimnames(X3) <- list(I2=seq(20), I7=seq(23), I8=seq(24))
    dimnames(X4) <- list(I3=seq(25), I9=seq(25), I10=seq(26))
    dimnames(X5) <- list(I4=seq(30), I11=seq(27), I12=seq(28))

    dimnames(X6) <- list(I5=seq(21), I13=seq(11))
    dimnames(X7) <- list(I6=seq(22), I14=seq(12))
    dimnames(X8) <- list(I7=seq(23), I15=seq(13))
    dimnames(X9) <- list(I8=seq(24), I16=seq(14))
    dimnames(X10) <- list(I9=seq(25), I17=seq(15))
    dimnames(X11) <- list(I10=seq(26), I18=seq(16))
    dimnames(X12) <- list(I11=seq(27), I19=seq(17))
    dimnames(X13) <- list(I12=seq(28), I20=seq(18))

    list(X1=X1, X2=X2, X3=X3, X4=X4, X5=X5,
        X6=X6, X7=X7, X8=X8, X9=X9, X10=X10, X11=X11, X12=X12, X13=X13)
}

.flist <- list(
    coupled_CP_Easy = .coupled_CP_Easy,
    coupled_CP_Hard = .coupled_CP_Hard,
    coupled_Tucker_Easy = .coupled_Tucker_Easy,
    coupled_Tucker_Hard = .coupled_Tucker_Hard,
    coupled_Complex = .coupled_Complex
)

toyModel <- function(model = "coupled_CP_Easy", seeds=123){
    set.seed(seeds)
    out <- .flist[[model]]()
    set.seed(NULL)
    out
}
