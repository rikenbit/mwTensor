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
