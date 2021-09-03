.TensorNetwork <- function(objects){
    if(class(objects) == "MWCAParams"){
        .TensorNetwork_MWCAParams(objects)
    }
    if(class(objects) == "CoupledMWCAParams"){
        .TensorNetwork_CoupledMWCAParams(objects)
    }
    if(class(objects) == "MWCAResult"){
        .TensorNetwork_MWCAResult(objects)
    }
    if(class(objects) == "CoupledMWCAResult"){
        .TensorNetwork_MWCAResult(objects)
    }
}

.TensorNetwork_MWCAParams <- function(objects){

}

.TensorNetwork_CoupledMWCAParams <- function(objects){
    # X1,X2,X3,...
    Xnames <- names(objects)
    # I1,I2,...
    Inames <- lapply(objects, function(x){names(dimnames(x))})
    # 2,3,2,...
    Degree <- unlist(lapply(Inames, length))
    # Adjacency matrix
    Adjacency <- matrix(0, nrow=length(Xnames), ncol=length(Xnames))
    rownames(Adjacency) <- Xnames
    colnames(Adjacency) <- Xnames
    for(i in seq_along(Xnames)){
        for(i in seq_along(Xnames)){
            Adjacency[i, j] <- .commonIndex(Inames[[i]], Inames[[j]])
        }
    }
    diag(Adjacency) <- 0
    # igraph object
    g <- graph.adjacency(Adjacency, mode="undirected", weighted=NULL)
    # Setting
    V(g)$weight <- Degree
    plot(g,
        vertex.size=25,
        vertex.shape="rectangle",
        vertex.label.color="white",
        vertex.label.cex = 2,
        vertex.label.dist = 0,
        vertex.label.family = "Helvetica",
        vertex.label.font = 2,
        vertex.frame.color="white",
        vertex.color=.degreeColor(V(g)$weight),
        edge.width=3,
        edge.color="gray80",
        edge.label=E(g)$label,
        edge.label.x=c(0.6, 0.2, -0.5),
        edge.label.y=c(0.6, 0.2, -0.5),
        edge.label.cex=3,
        layout=layout_as_tree)
}

.edgeList <- function(Inames){

}


# .dark2 <- c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E",
    # "#E6AB02", "#A6761D", "#666666")
# .colrs <- c("gray50", "tomato", "gold")
# .set1 <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
#     "#FFFF33", "#A65628", "#F781BF", "#999999")
# .mydark <- c("#c18e96", "#c48944", "#012356", "#010920")
.mydark <- c("#BB86FC", "#3700B3", "#03DAC6", "#CF6679", "#121212")

.degreeColor <- function(x){
    .mydark[unlist(x)]
}


.commonIndex <- function(x, y){
    if(length(intersect(x, y)) != 0){
        1
    }else{
        0
    }
}


.TensorNetwork_MWCAResult <- function(objects){

}

.TensorNetwork_CoupledMWCAResult <- function(objects){
    # X1,X2,X3,...
    names(objects@dimnames)
    # I1,I2,I3,...
    objects@dimnames
    # A1,A2,A3,...
    objects@initial

}


