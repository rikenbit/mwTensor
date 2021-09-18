library("mwTensor")
library("MASS")
library("rTensor")
library("nnTensor")
library("ccTensor")
library("ica")
library("igraph")
library("testthat")

options(testthat.use_colours = FALSE)

# source("../R/mySVD.R") # comment out
# source("../R/myALS_SVD.R") # comment out
# source("../R/myNMF.R") # comment out
# source("../R/myICA.R") # comment out
# source("../R/myCX.R") # comment out
# source("../R/AllClasses.R") # comment out
# source("../R/AllGenerics.R") # comment out
# source("../R/mwTensor-internal.R") # comment out
# source("../R/MWCA_Check.R") # comment out
# source("../R/MWCA_Initialization.R") # comment out
# source("../R/MWCA.R") # comment out
# source("../R/CoupledMWCA_Check_common.R") # comment out
# source("../R/CoupledMWCA_Check_specific.R") # comment out
# source("../R/CoupledMWCA_Check_other.R") # comment out
# source("../R/CoupledMWCA_Initialization.R") # comment out
# source("../R/CoupledMWCA.R") # comment out
# source("../R/toyModel.R") # comment out
# source("../R/plotTensor3Ds.R") # comment out

# test_file
test_file("testthat/test_mySVD.R")
test_file("testthat/test_myALS_SVD.R")
test_file("testthat/test_myNMF.R")
test_file("testthat/test_myICA.R")
test_file("testthat/test_myCX.R")
test_file("testthat/test_toyModel.R")
test_file("testthat/test_MWCA.R")
test_file("testthat/test_CoupledMWCA_common.R")
test_file("testthat/test_CoupledMWCA_specific.R")
# test_file("testthat/test_Complex_common.R") # comment out
# test_file("testthat/test_Complex_specific.R") # comment out
# test_file("testthat/test_Complex_eachData.R") # comment out
