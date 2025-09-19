# ---------------------------------------------------------------------------- #
# Define Functions for Fitting Saturated Models
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Define code_satur_gimme_model_paths() ----
# ---------------------------------------------------------------------------- #

# Define function to create GIMME paths for saturated model where "ar = TRUE"
# and "VAR = TRUE" from example participant's data matrix

create_satur_gimme_paths <- function(dat_mat) {
  # Specify variables and design matrices
  
  vars_current <- colnames(dat_mat)
  vars_lagged <- paste0(vars_current, "lag")
  
  n_vars <- length(vars_current)
  
  current_mat           <- matrix(1, n_vars, n_vars,
                                  dimnames = list(vars_current, vars_current))
  lagged_mat            <- matrix(1, n_vars, n_vars,
                                  dimnames = list(vars_lagged,  vars_lagged))
  lagged_to_current_mat <- matrix(1, n_vars, n_vars,
                                  dimnames = list(vars_current, vars_lagged))

  # Specify variances for current variables (diagonal)
  
  variances_current <- paste0(vars_current, "~~", vars_current)
  
  # Specify intercepts for current variables
  
  intercepts_current <- paste0(vars_current, "~1")
  
  # Specify variances and covariances among lagged variables (diagonal and lower tri)
  
  idx <- as.data.frame(which(lower.tri(lagged_mat, diag = TRUE), arr.ind = TRUE))
  
  varcov_lagged <- apply(idx, 1, function(x) {
    paste0(vars_lagged[x["row"]], "~~", vars_lagged[x["col"]])
  })
  
  # Specify intercepts for lagged variables
  
  intercepts_lagged <- paste0(vars_lagged, "~1")
  
  # Specify lagged variables not predicted by current variables (entire matrix)
  
  current_to_lagged <- vector()
  
  for (var_lagged in vars_lagged) {
    current_to_lagged_for_var_lagged <- paste0(var_lagged, "~0*", vars_current)
    
    current_to_lagged <- c(current_to_lagged, current_to_lagged_for_var_lagged)
  }
  
  # Specify autoregressive paths (diagonal)
  
  autoreg <- paste(vars_current, vars_lagged, sep = "~")
  
  # Specify undirected contemporaneous relations among current variables (lower tri; among residuals?)
  
  idx <- as.data.frame(which(lower.tri(current_mat), arr.ind = TRUE))
  
  cov_current <- apply(idx, 1, function(x) {
    paste0(vars_current[x["row"]], "~~", vars_current[x["col"]])
  })
  
  # Specify cross-lagged relations (lower tri and upper tri)
  
  idx <- as.data.frame(which(lower.tri(lagged_to_current_mat) | upper.tri(lagged_to_current_mat), 
                             arr.ind = TRUE))
  
  cross_lagged <- apply(idx, 1, function(x) {
    paste0(vars_current[x["row"]], "~~", vars_lagged[x["col"]])
  })
  
  # Specify paths
  
  paths <- list(variances_current  = variances_current,
                intercepts_current = intercepts_current,
                varcov_lagged      = varcov_lagged,
                intercepts_lagged  = intercepts_lagged,
                current_to_lagged  = current_to_lagged,
                autoreg            = autoreg,
                cov_current        = cov_current,
                cross_lagged       = cross_lagged)
  
  paths$all <- unlist(paths, use.names = FALSE)
  
  # Include number of paths
  
  paths <- lapply(paths, function(x) {
    list(length = length(x),
         paths = x)
  })
  
  return(paths)
}

# ---------------------------------------------------------------------------- #
# Define create_lagged_vars_for_satur_gimme_model() ----
# ---------------------------------------------------------------------------- #

# Define function to create lagged variables (append "lag" to current variables) using GIMME method in "setupTransformData.R"
# - https://github.com/GatesLab/gimme/blob/a633a143108315941a2d09701ec7c204b4742087/R/setupTransformData.R#L131-L158

create_lagged_vars_for_satur_gimme_model <- function(dat_mat_ls) {
  dat_mat_ls <- lapply(dat_mat_ls, function(mat){
    first           <- mat[1:(nrow(mat) - 1), ]
    second          <- mat[2:(nrow(mat)), ]
    ts_lc           <- cbind(first, second)
    colnames(ts_lc) <- c(paste0(colnames(mat), "lag"), colnames(mat))
    
    ts_lc
  })
}

# ---------------------------------------------------------------------------- #
# Define setup_dat_for_testWeights() ----
# ---------------------------------------------------------------------------- #

# Define function to create limited version of "dat" for use in testWeights() below
# using GIMME's approach in "setup()"
# - https://github.com/GatesLab/gimme/blob/master/R/setup.R

setup_dat_for_testWeights <- function(data_file) {
  # Note: Simplified the following for our case (i.e., assuming that "exogenous",
  # "conv_vars", and "mult_vars" of "indSEM()" are NULL and that "data_file" already 
  # contains the lagged variables created via create_lagged_vars_for_satur_gimme_model()
  
  orig <- colnames(data_file)[!grepl("lag", colnames(data_file))]
  lagg <- paste0(orig, "lag")
  exog <- lagg
  endo <- setdiff(orig, exog)
  coln <- unique(c(lagg, endo))
  
  varLabels <- list(orig = orig,
                    lagg = lagg,
                    exog = exog,
                    endo = endo,
                    coln = coln)
  
  dat <- list("n_endog"   = length(varLabels$endo),
              "varLabels" = varLabels)
  
  return(dat)
}

# ---------------------------------------------------------------------------- #
# Define testWeights() ----
# ---------------------------------------------------------------------------- #

# Define testWeights() from "search.paths.ind.R" for use in fit_and_check_satur_model() below
# - https://github.com/GatesLab/gimme/blob/master/R/search.paths.ind.R

# TODO: What does this check exactly?





testWeights <- function(fit, dat) {
  ind_betas <- round(lavInspect(fit, "std")$beta, digits = 4)
  #added to ensure correct ordering in matrices
  ind_betas <- ind_betas[dat$varLabels$endo, ]                           # TODO: "endo" is original nonlagged variable names
  ind_betas <- ind_betas[, dat$varLabels$coln]                           # TODO: "coln" is lagged and nonlagged variable names
  test      <- any(Re(eigen(ind_betas[, 1:dat$n_endog])$values) >= 1) |  # TODO: "n_endog" is number of original variables
    any(Re(eigen(ind_betas[, (dat$n_endog + 1):(dat$n_endog * 2)])$values) >= 1)
  
  return(test)
}

# ---------------------------------------------------------------------------- #
# Define fit_and_check_satur_model() ----
# ---------------------------------------------------------------------------- #

# Define function to fit saturated idiographic VAR model and check convergence
# using GIMME's approaches in "fit.model()" and "search.paths.ind()"
# - https://github.com/GatesLab/gimme/blob/master/R/fit.model.R
# - https://github.com/GatesLab/gimme/blob/master/R/search.paths.ind.R

fit_and_check_satur_model <- function(data_file, syntax) {
  # Set up limited "dat" object for use in "testWeights()" below
  
  dat <- setup_dat_for_testWeights(data_file)
  
  # Fit model
  
  fit <- try(lavaan(syntax,
                    data            = data_file,
                    model.type      = "sem",
                    missing         = "fiml",
                    estimator       = "ml",
                    int.ov.free     = FALSE,
                    int.lv.free     = TRUE,
                    auto.fix.first  = TRUE,
                    auto.var        = TRUE,
                    auto.cov.lv.x   = TRUE,
                    auto.th         = TRUE,
                    auto.delta      = TRUE,
                    auto.cov.y      = FALSE,
                    auto.fix.single = TRUE,
                    warn            = FALSE))
  
  # Check for convergence if no error during model-fitting
  
  if (!inherits(fit, "try-error")){
    converge     <- lavaan::lavInspect(fit, "converged")
    zero_se      <- sum(lavInspect(fit, "se")$beta, na.rm = TRUE) == 0   # If all non-NA SEs are 0
    na_se        <- any(is.na(lavInspect(fit, what = "list")$se))        # If any SEs are NA
    test_weights <- testWeights(fit, dat)                                # TODO: Define meaning of this (TRUE is bad)
    
    if (converge & !na_se) { 
      indices <- fitMeasures(fit, c("chisq", "df", "pvalue", "rmsea", "srmr", "nnfi", "cfi"))
    } else {
      indices <- NULL
    }
    
    # TODO (check the whole convergence logic and simplify): Do additional checks
    
    if (converge & !zero_se & !test_weights){
      status1 <- "converged normally"
      nonconv <- FALSE
    } else {
      # TODO (what happens to "nonconv" and do we need it at all?): if no convergence or unstable
      if (!converge | zero_se | test_weights) {
        if (test_weights | zero_se) {
          status1 <- "unstable solution"
        } else if (!converge) {
          status1 <- "nonconvergence"
        }
      }
    }

  } else {
    # TODO: What about "na_se", "test_weights", and "status1" in this case?
    indices  <- NULL
    converge <- FALSE
    zero_se  <- TRUE
    nonconv  <- TRUE
  }
  
  # Return fit and convergence indicators in list
  
  results <- list(dat          = dat,
                  fit          = fit,
                  converge     = converge,
                  zero_se      = zero_se,
                  na_se        = na_se,
                  test_weights = test_weights,
                  indices      = indices,
                  status1      = status1)
  
  return(results)
}