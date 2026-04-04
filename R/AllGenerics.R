# MWCA
setGeneric("MWCA", function(params) standardGeneric("MWCA"))
setMethod("MWCA",
    signature(params="MWCAParams"),
    function(params){
        .MWCA(params)})

# CoupledMWCA
setGeneric("CoupledMWCA", function(params) standardGeneric("CoupledMWCA"))

# checkCoupledMWCA
setGeneric("checkCoupledMWCA", function(params) standardGeneric("checkCoupledMWCA"))

# initCoupledMWCA
setGeneric("initCoupledMWCA",
    function(params, seed=NULL, init_policy="random")
        standardGeneric("initCoupledMWCA"))
setMethod("CoupledMWCA",
    signature(params="CoupledMWCAParams"),
    function(params){
        .CoupledMWCA(params)})

setMethod("CoupledMWCA",
    signature(params="CoupledMWCAInit"),
    function(params){
        .CoupledMWCA(.injectInit(params))
    })
