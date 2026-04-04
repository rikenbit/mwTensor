# ============================================================
# Test data
# ============================================================
Xs <- mwTensor::toyModel("coupled_CP_Easy")

# -----------------------------------------------------------
# Helper: build a valid 3-block coupled program (matches Xs)
# -----------------------------------------------------------
make_valid_program <- function(){
    blocks <- list(
        X1=MWCAProgramBlock(
            modes=c("I1","I2"),
            factor_map=c(I1="A1", I2="A2"),
            type="common"),
        X2=MWCAProgramBlock(
            modes=c("I2","I3","I4"),
            factor_map=c(I2="A2", I3="A3", I4="A4"),
            type="common"),
        X3=MWCAProgramBlock(
            modes=c("I4","I5"),
            factor_map=c(I4="A4", I5="A5"),
            type="common"))
    factors <- list(
        A1=MWCAProgramFactor(mode="I1", dim=3, algorithm="mySVD"),
        A2=MWCAProgramFactor(mode="I2", dim=3, algorithm="mySVD"),
        A3=MWCAProgramFactor(mode="I3", dim=5, algorithm="myNMF"),
        A4=MWCAProgramFactor(mode="I4", dim=4, algorithm="myICA"),
        A5=MWCAProgramFactor(mode="I5", dim=4, algorithm="myCX"))
    MWCAProgram(blocks=blocks, factors=factors)
}

# ============================================================
# 1. Valid simple program (no refinement)
# ============================================================
prog <- make_valid_program()
expect_true(inherits(prog, "MWCAProgram"))
expect_equal(length(prog$blocks), 3)
expect_equal(length(prog$factors), 5)
expect_equal(length(prog$refinements), 0)

v <- validateMWCAProgram(prog)
expect_true(v$ok)
expect_equal(length(v$errors), 0)

# ============================================================
# 2. Print and summary don't error
# ============================================================
expect_output(print(prog), "MWCAProgram")
expect_output(summary(prog), "Program is valid")

# ============================================================
# 3. Valid one-step factor refinement
# ============================================================
prog_ref <- MWCAProgram(
    blocks=prog$blocks,
    factors=prog$factors,
    refinements=list(
        R1=MWCAProgramRefinement(
            source_factor="A2", algorithm="myNMF", dim=2)))
v_ref <- validateMWCAProgram(prog_ref)
expect_true(v_ref$ok)

# ============================================================
# 4. Recursion depth > 1 is rejected
# ============================================================
prog_deep <- MWCAProgram(
    blocks=prog$blocks,
    factors=prog$factors,
    refinements=list(
        R1=MWCAProgramRefinement(source_factor="A2", algorithm="myNMF", dim=2),
        R2=MWCAProgramRefinement(source_factor="R1", algorithm="mySVD", dim=2)))
# R2 targets R1 which is a refinement output -> depth > 1
v_deep <- validateMWCAProgram(prog_deep)
expect_false(v_deep$ok)
expect_true(any(grepl("depth > 1", v_deep$errors)))

# ============================================================
# 5. Reference to undefined factor
# ============================================================
prog_bad_ref <- MWCAProgram(
    blocks=list(
        X1=MWCAProgramBlock(
            modes=c("I1","I2"),
            factor_map=c(I1="A1", I2="NONEXISTENT"),
            type="common")),
    factors=list(
        A1=MWCAProgramFactor(mode="I1", dim=3)))
v_bad <- validateMWCAProgram(prog_bad_ref)
expect_false(v_bad$ok)
expect_true(any(grepl("NONEXISTENT", v_bad$errors)))

# ============================================================
# 6. Refinement references undefined factor
# ============================================================
prog_bad_refinement <- MWCAProgram(
    blocks=prog$blocks,
    factors=prog$factors,
    refinements=list(
        R1=MWCAProgramRefinement(
            source_factor="GHOST", algorithm="mySVD", dim=2)))
v_bad2 <- validateMWCAProgram(prog_bad_refinement)
expect_false(v_bad2$ok)
expect_true(any(grepl("GHOST", v_bad2$errors)))

# ============================================================
# 7. Unmapped mode in block is an error
# ============================================================
prog_unmapped <- MWCAProgram(
    blocks=list(
        X1=MWCAProgramBlock(
            modes=c("I1","I2","I3"),
            factor_map=c(I1="A1", I2="A2"),
            type="common")),
    factors=list(
        A1=MWCAProgramFactor(mode="I1", dim=3),
        A2=MWCAProgramFactor(mode="I2", dim=3)))
v_unmapped <- validateMWCAProgram(prog_unmapped)
expect_false(v_unmapped$ok)
expect_true(any(grepl("unmapped", v_unmapped$errors)))

# ============================================================
# 8. Factor status types are reflected correctly
# ============================================================
prog_status <- MWCAProgram(
    blocks=list(
        X1=MWCAProgramBlock(
            modes=c("I1","I2"),
            factor_map=c(I1="A1", I2="A2"),
            type="common")),
    factors=list(
        A1=MWCAProgramFactor(mode="I1", dim=3, status="fixed"),
        A2=MWCAProgramFactor(mode="I2", dim=3, status="frozen")))
v_status <- validateMWCAProgram(prog_status)
expect_true(v_status$ok)
expect_equal(prog_status$factors$A1$status, "fixed")
expect_equal(prog_status$factors$A2$status, "frozen")

# ============================================================
# 9. Compiler: simple program -> CoupledMWCAParams
# ============================================================
compiled <- compileMWCAProgram(prog, Xs)
expect_true(is(compiled, "CoupledMWCAParams"))
expect_equal(length(compiled@Xs), 3)
expect_equal(compiled@common_dims$A1, 3)
expect_equal(compiled@common_dims$A3, 5)
expect_equal(compiled@common_algorithms$A3, "myNMF")
expect_equal(compiled@common_algorithms$A4, "myICA")

# ============================================================
# 10. Compiler: fixed/frozen status maps correctly (multi-block)
# ============================================================
prog_status_multi <- MWCAProgram(
    blocks=list(
        X1=MWCAProgramBlock(
            modes=c("I1","I2"),
            factor_map=c(I1="A1", I2="A2"),
            type="common"),
        X2=MWCAProgramBlock(
            modes=c("I2","I3","I4"),
            factor_map=c(I2="A2", I3="A3", I4="A4"),
            type="common")),
    factors=list(
        A1=MWCAProgramFactor(mode="I1", dim=3, status="fixed"),
        A2=MWCAProgramFactor(mode="I2", dim=3, status="frozen"),
        A3=MWCAProgramFactor(mode="I3", dim=3, status="decomposed"),
        A4=MWCAProgramFactor(mode="I4", dim=3, status="decomposed")))
compiled_status <- compileMWCAProgram(prog_status_multi,
    list(X1=Xs$X1, X2=Xs$X2))
expect_true(is(compiled_status, "CoupledMWCAParams"))
expect_true(compiled_status@common_fix$A1)    # fixed -> fix=TRUE
expect_true(compiled_status@common_decomp$A1) # fixed -> decomp=TRUE
expect_false(compiled_status@common_decomp$A2) # frozen -> decomp=FALSE

# ============================================================
# 11. Compiler: single block -> MWCAParams
# ============================================================
prog_single <- MWCAProgram(
    blocks=list(
        X1=MWCAProgramBlock(
            modes=c("I1","I2"),
            factor_map=c(I1="A1", I2="A2"),
            type="common")),
    factors=list(
        A1=MWCAProgramFactor(mode="I1", dim=3),
        A2=MWCAProgramFactor(mode="I2", dim=5)))
compiled_single <- compileMWCAProgram(prog_single, list(X1=Xs$X1))
expect_true(is(compiled_single, "MWCAParams"))
expect_equal(compiled_single@dims, c(3L, 5L))
expect_equal(compiled_single@algorithms, c("mySVD", "mySVD"))

# ============================================================
# 12. Compiler: refuses invalid program
# ============================================================
expect_error(compileMWCAProgram(prog_bad_ref, list(X1=Xs$X1)),
    "Cannot compile invalid program")

# ============================================================
# 13. Compiler: refuses programs with refinements
# ============================================================
expect_error(compileMWCAProgram(prog_ref, Xs),
    "refinements is not yet supported")

# ============================================================
# 14. Compiler: missing Xs data errors
# ============================================================
expect_error(compileMWCAProgram(prog, Xs[1:2]),
    "missing data")

# ============================================================
# 15. Factor type vs block type mismatch -> warning
# ============================================================
prog_mismatch <- MWCAProgram(
    blocks=list(
        X1=MWCAProgramBlock(
            modes=c("I1","I2"),
            factor_map=c(I1="A1", I2="A2"),
            type="common")),
    factors=list(
        A1=MWCAProgramFactor(mode="I1", dim=3, type="common"),
        A2=MWCAProgramFactor(mode="I2", dim=3, type="specific")))
v_mismatch <- validateMWCAProgram(prog_mismatch)
expect_true(v_mismatch$ok) # warning, not error
expect_true(length(v_mismatch$warnings) > 0)
expect_true(any(grepl("type", v_mismatch$warnings)))

# ============================================================
# 16. Compiler: mixed common/specific blocks
# ============================================================
prog_mixed <- MWCAProgram(
    blocks=list(
        X1=MWCAProgramBlock(modes=c("I1","I2"),
            factor_map=c(I1="A1", I2="A2"), type="common"),
        X2=MWCAProgramBlock(modes=c("I2","I3","I4"),
            factor_map=c(I2="A2", I3="A3", I4="A4"), type="common"),
        X3=MWCAProgramBlock(modes=c("I4","I5"),
            factor_map=c(I4="B1", I5="B2"), type="specific")),
    factors=list(
        A1=MWCAProgramFactor(mode="I1", dim=3, type="common"),
        A2=MWCAProgramFactor(mode="I2", dim=3, type="common"),
        A3=MWCAProgramFactor(mode="I3", dim=3, type="common"),
        A4=MWCAProgramFactor(mode="I4", dim=3, type="common"),
        B1=MWCAProgramFactor(mode="I4", dim=2, type="specific"),
        B2=MWCAProgramFactor(mode="I5", dim=2, type="specific")))
compiled_mixed <- compileMWCAProgram(prog_mixed, Xs)
expect_true(is(compiled_mixed, "CoupledMWCAParams"))
# common_model must cover all Xs
expect_equal(sort(names(compiled_mixed@common_model)), c("X1","X2","X3"))
expect_equal(sort(names(compiled_mixed@specific_model)), c("X1","X2","X3"))
expect_true(compiled_mixed@specific)
# Should actually run
out_mixed <- CoupledMWCA(compiled_mixed)
expect_true(is(out_mixed, "CoupledMWCAResult"))

# ============================================================
# 17. executeMWCAProgram: no refinements
# ============================================================
res_exec <- executeMWCAProgram(prog, Xs)
expect_true(is(res_exec$fit, "CoupledMWCAResult"))
expect_equal(length(res_exec$refinements), 0)

# ============================================================
# 18. executeMWCAProgram: with refinement
# ============================================================
prog_with_ref <- MWCAProgram(
    blocks=prog$blocks,
    factors=prog$factors,
    refinements=list(
        R1=MWCAProgramRefinement(source_factor="A2",
            algorithm="mySVD", dim=2)))
res_ref <- executeMWCAProgram(prog_with_ref, Xs)
expect_true(is(res_ref$fit, "CoupledMWCAResult"))
expect_equal(length(res_ref$refinements), 1)
expect_true(is(res_ref$refinements$R1, "RefinedFactor"))
expect_equal(res_ref$refinements$R1@source_factor_name, "A2")
expect_equal(res_ref$refinements$R1@dim, 2L)

# ============================================================
# 19. Refinement name collides with factor name -> error
# ============================================================
prog_name_collision <- MWCAProgram(
    blocks=prog$blocks,
    factors=prog$factors,
    refinements=list(
        A2=MWCAProgramRefinement(source_factor="A3",
            algorithm="mySVD", dim=2)))
v_collision <- validateMWCAProgram(prog_name_collision)
expect_false(v_collision$ok)
expect_true(any(grepl("collide", v_collision$errors)))

# ============================================================
# 20. Duplicate refinements on same source factor -> warning
# ============================================================
prog_dup_ref <- MWCAProgram(
    blocks=prog$blocks,
    factors=prog$factors,
    refinements=list(
        R1=MWCAProgramRefinement(source_factor="A2",
            algorithm="mySVD", dim=2),
        R2=MWCAProgramRefinement(source_factor="A2",
            algorithm="myNMF", dim=3)))
v_dup <- validateMWCAProgram(prog_dup_ref)
expect_true(v_dup$ok)
expect_true(length(v_dup$warnings) > 0)
expect_true(any(grepl("Multiple refinements", v_dup$warnings)))

# ============================================================
# 21. Compiler: dummy factors are frozen (decomp=FALSE, dim=1)
# ============================================================
compiled_mixed_check <- compileMWCAProgram(prog_mixed, Xs)
# X3 is specific-only -> should have dummy common factors
x3_common <- compiled_mixed_check@common_model$X3
dummy_factor_names <- unlist(x3_common)
for(dfn in dummy_factor_names){
    expect_equal(compiled_mixed_check@common_dims[[dfn]], 1,
        info=paste("dummy common factor", dfn, "should have dim=1"))
    expect_false(compiled_mixed_check@common_decomp[[dfn]],
        info=paste("dummy common factor", dfn, "should have decomp=FALSE"))
}

# ============================================================
# 22. executeMWCAProgram: single block (no refinement)
# ============================================================
prog_single_exec <- MWCAProgram(
    blocks=list(
        X1=MWCAProgramBlock(
            modes=c("I1","I2"),
            factor_map=c(I1="A1", I2="A2"),
            type="common")),
    factors=list(
        A1=MWCAProgramFactor(mode="I1", dim=3),
        A2=MWCAProgramFactor(mode="I2", dim=5)))
res_single <- executeMWCAProgram(prog_single_exec, list(X1=Xs$X1))
expect_true(is(res_single$fit, "MWCAResult"))
expect_equal(length(res_single$refinements), 0)

# ============================================================
# 23. Refinement referencing undefined factor -> error
#     (already tested in #6 but with a more specific message check)
# ============================================================
prog_ref_undef <- MWCAProgram(
    blocks=prog$blocks,
    factors=prog$factors,
    refinements=list(
        R1=MWCAProgramRefinement(
            source_factor="MISSING_FACTOR",
            algorithm="mySVD", dim=2)))
v_ref_undef <- validateMWCAProgram(prog_ref_undef)
expect_false(v_ref_undef$ok)
expect_true(any(grepl("undefined factor", v_ref_undef$errors)))
expect_true(any(grepl("MISSING_FACTOR", v_ref_undef$errors)))

# ============================================================
# 24. executeMWCAProgram: refinement with dim too large
#     errors at runtime (propagated from refineFactor)
# ============================================================
prog_ref_bad_dim <- MWCAProgram(
    blocks=prog$blocks,
    factors=prog$factors,
    refinements=list(
        R1=MWCAProgramRefinement(source_factor="A2",
            algorithm="mySVD", dim=999)))
# Passes validation (no data dimensions to check at program level)
v_ref_bad_dim <- validateMWCAProgram(prog_ref_bad_dim)
expect_true(v_ref_bad_dim$ok)
# Fails at execution time when refineFactor checks dim vs factor shape
expect_error(executeMWCAProgram(prog_ref_bad_dim, Xs),
    "exceeds")
