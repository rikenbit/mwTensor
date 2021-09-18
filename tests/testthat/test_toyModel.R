Xs1 <- mwTensor::toyModel("coupled_CP_Easy")
expect_equal(length(Xs1), 3)

Xs2 <- mwTensor::toyModel("coupled_CP_Hard")
expect_equal(length(Xs2), 3)

Xs3 <- mwTensor::toyModel("coupled_Tucker_Easy")
expect_equal(length(Xs3), 3)

Xs4 <- mwTensor::toyModel("coupled_Tucker_Hard")
expect_equal(length(Xs4), 3)

Xs5 <- mwTensor::toyModel("coupled_Complex_Easy")
expect_equal(length(Xs5), 13)

Xs6 <- mwTensor::toyModel("coupled_Complex_Hard")
expect_equal(length(Xs6), 13)
