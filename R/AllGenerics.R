# MWCA
setGeneric("MWCA", function(params) standardGeneric("MWCA"))
setMethod("MWCA",
    signature(params="MWCAParams"),
    function(params){
        .MWCA(params)})

# CoupledMWCA
setGeneric("CoupledMWCA", function(params) standardGeneric("CoupledMWCA"))
setMethod("CoupledMWCA",
    signature(params="CoupledMWCAParams"),
    function(params){
        .CoupledMWCA(params)})

# TensorNetwork
setGeneric("TensorNetwork", function(obj) standardGeneric("TensorNetwork"))
setMethod("TensorNetwork",
    signature(obj="MWCAParams"),
    function(obj){
        .TensorNetwork(obj)})

setMethod("TensorNetwork",
    signature(obj="CoupledMWCAParams"),
    function(obj){
        .TensorNetwork(obj)})

setMethod("TensorNetwork",
    signature(obj="MWCAResult"),
    function(obj){
        .TensorNetwork(obj)})

setMethod("TensorNetwork",
    signature(obj="CoupledMWCAResult"),
    function(obj){
        .TensorNetwork(obj)})
