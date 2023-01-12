## Class to holds parameters that will be passed on to MWCA
setClass("MWCAParams",
    representation=representation(
        X="array",
        mask="ANY",
        algorithms="character",
        dims="numeric",
        transpose="logical",
        viz="logical",
        figdir="ANY"
    )
)

## Class to holds parameters that will be passed on to Coupled MWCA
setClass("CoupledMWCAParams",
    representation=representation(
        # 1. Data-wise setting
        Xs="list",
        mask="list",
        weights="list",
        # 2. Common Model setting
        common_model="list",
        # 3. Common Factor matrix-wise setting
        common_initial="list",
        common_algorithms="list",
        common_iteration="list",
        common_decomp="list",
        common_fix="list",
        common_dims="list",
        common_transpose="list",
        common_coretype="character",
        # 4. Specific Model setting
        specific_model="list",
        # 5. Specific Factor matrix-wise setting
        specific_initial="list",
        specific_algorithms="list",
        specific_iteration="list",
        specific_decomp="list",
        specific_fix="list",
        specific_dims="list",
        specific_transpose="list",
        specific_coretype="character",
        # 6. Other option
        specific="logical",
        thr="numeric",
        viz="logical",
        figdir="ANY",
        verbose="logical"
    )
)

## Class to holds MWCA results for plot
setClass("MWCAResult",
    representation=representation(
        algorithms="character",
        dims="numeric",
        transpose="logical",
        viz="logical",
        figdir="ANY",
        factors="list",
        core="array",
        rec_error="numeric",
        train_error="numeric",
        test_error="numeric"
    )
)

## Class to holds Coupled MWCA results for plot
setClass("CoupledMWCAResult",
    representation=representation(
        # Data-wise setting
        weights="list",
        # Common Factor Matrices
        common_model="list",
        common_initial="list",
        common_algorithms="list",
        common_iteration="list",
        common_decomp="list",
        common_fix="list",
        common_dims="list",
        common_transpose="list",
        common_coretype="character",
        common_factors="list",
        common_cores="list",
        # Specific Factor Matrices
        specific_model="list",
        specific_initial="list",
        specific_algorithms="list",
        specific_iteration="list",
        specific_decomp="list",
        specific_fix="list",
        specific_dims="list",
        specific_transpose="list",
        specific_coretype="character",
        specific_factors="list",
        specific_cores="list",
        # Other option
        specific="logical",
        thr="numeric",
        viz="logical",
        figdir="ANY",
        verbose="logical",
        # Iteration
        rec_error="numeric",
        train_error="numeric",
        test_error="numeric",
        rel_change="numeric"
    )
)
