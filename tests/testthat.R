library("MASS")
library("rTensor")
library("nnTensor")
library("ccTensor")
library("ica")
library("testthat")

options(testthat.use_colours = FALSE)

source("../R/mySVD.R") # comment out
source("../R/myNMF.R") # comment out
source("../R/myICA.R") # comment out
source("../R/myCX.R") # comment out
source("../R/AllClasses.R") # comment out
source("../R/AllGenerics.R") # comment out
source("../R/mwTensor-internal.R") # comment out
source("../R/MWCA.R") # comment out
source("../R/CoupledMWCA.R") # comment out
source("../R/toyModel.R") # comment out
source("../R/TensorNetwork.R") # comment out

test_file("testthat/test_mySVD.R")
test_file("testthat/test_myNMF.R")
test_file("testthat/test_myICA.R")
test_file("testthat/test_myCX.R")
test_file("testthat/test_MWCA.R")
test_file("testthat/test_CoupledMWCA.R")
test_file("testthat/test_toyModel.R")
test_file("testthat/test_TensorNetwork.R")
