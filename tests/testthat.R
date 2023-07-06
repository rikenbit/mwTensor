library("mwTensor")
library("MASS")
library("rTensor")
library("nnTensor")
library("ccTensor")
library("iTensor")
library("igraph")
library("testthat")

options(testthat.use_colours = FALSE)

# test_file
test_file("testthat/test_mySVD.R")
test_file("testthat/test_myALS_SVD.R")
test_file("testthat/test_myNMF.R")
test_file("testthat/test_myICA.R")
test_file("testthat/test_myCX.R")
test_file("testthat/test_toyModel.R")
test_file("testthat/test_MWCA.R")
test_file("testthat/test_defaultMWCAParams.R")
test_file("testthat/test_defaultCoupledMWCAParams.R")
test_file("testthat/test_CoupledMWCA_common.R")
test_file("testthat/test_CoupledMWCA_specific.R")
# test_file("testthat/test_Complex_common.R") # comment out
# test_file("testthat/test_Complex_specific.R") # comment out
# test_file("testthat/test_Complex_eachData.R") # comment out
