Xs1 <- toyModel("coupled_CP_Easy")
expect_equal(length(Xs1), 3)

Xs2 <- toyModel("coupled_CP_Hard")
expect_equal(length(Xs2), 3)

Xs3 <- toyModel("coupled_Tucker_Easy")
expect_equal(length(Xs3), 3)

Xs4 <- toyModel("coupled_Tucker_Hard")
expect_equal(length(Xs4), 3)

Xs5 <- toyModel("coupled_Complex_Easy")
expect_equal(length(Xs5), 13)

Xs6 <- toyModel("coupled_Complex_Hard")
expect_equal(length(Xs6), 13)
