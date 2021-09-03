# Xs <- toyModel("coupled_CP_Easy")
# Xs <- toyModel("coupled_CP_Hard")
# Xs <- toyModel("coupled_Tucker_Easy")
Xs <- toyModel("coupled_Tucker_Hard")

params <- new("CoupledMWCAParams",
    Xs=Xs,
    dims=list(
        X1=c(A1=1, A2=1),
        X2=c(A2=1, A3=4, A4=4),
        X3=c(A4=4, A5=5)),
    algorithms=list(
    	X1=c(A1="mySVD", A2="mySVD"),
    	X2=c(A2="mySVD", A3="mySVD", A4="mySVD"),
    	X3=c(A4="mySVD", A5="mySVD")),
    iteration=list(
    	X1=c(A1=10, A2=10),
    	X2=c(A2=10, A3=10, A4=10),
    	X3=c(A4=10, A5=10)),
    viz=TRUE,
    transpose=FALSE,
    verbose=TRUE)

out <- CoupledMWCA(params)
rec <- .recTensors(Ss=out@cores, As=out@factors)

# layout(t(1:3))
# image.plot(rec[[1]]@data)
# plotTensor3D(as.tensor(rec[[2]]@data))
# image.plot(rec[[3]]@data)
