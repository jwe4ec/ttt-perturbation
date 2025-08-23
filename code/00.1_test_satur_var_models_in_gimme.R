# ---------------------------------------------------------------------------- #
# Test Saturated Idiographic VAR Models in GIMME ----
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Setup ----
# ---------------------------------------------------------------------------- #

# Load package and set seed

library(gimme)

set.seed(1234)

# Load example data for 5 individuals, each with 50 observations on 3 variables

dat_ls <- ts

# Convert data frames to matrices

dat_mat_ls <- lapply(dat_ls, as.matrix)

# ---------------------------------------------------------------------------- #
# Define code_satur_gimme_model_paths() helper function ----
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
# Define create_lagged_vars_for_satur_gimme_model() helper function ----
# ---------------------------------------------------------------------------- #

# Define function to create lagged variables (append "lag" to current variables) using GIMME method in "setupTransformData.R"
# (see https://github.com/GatesLab/gimme/blob/a633a143108315941a2d09701ec7c204b4742087/R/setupTransformData.R#L131-L158 )

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
# Fit saturated GIMME idiographic VAR models ----
# ---------------------------------------------------------------------------- #

# Create paths for saturated model from example participant's data matrix using function above

satur_gimme_paths <- create_satur_gimme_paths(dat_mat_ls[[1]])

# Create lagged variables per GIMME method using function above

dat_mat_ls <- create_lagged_vars_for_satur_gimme_model(dat_mat_ls)

# Fit saturated idiographic VAR models (as when "ar = TRUE" and "VAR = TRUE")

satur_var_res_ls <- indSEM(dat_mat_ls, "./results/gimme/test/raw/",
                           paths = satur_gimme_paths$all$paths)

# TODO: Resolve errors like this for each individual:

  # individual-level search, subject 1 (ts1)
  # Error in lav_parse_model_string_orig(model.syntax = model.syntax, as.data.frame. = as.data.frame.,  : 
  #   lavaan ERROR: duplicate model element in: V1~~V1
  # Error in lav_parse_model_string_orig(model.syntax = model.syntax, as.data.frame. = as.data.frame.,  : 
  #   lavaan ERROR: duplicate model element in: V1~~V1
  # Error in lav_parse_model_string_orig(model.syntax = model.syntax, as.data.frame. = as.data.frame.,  : 
  #   lavaan ERROR: duplicate model element in: V1~~V1

# TODO: Resolve other errors and warnings:

  # Error in coefs[!coefs$param %in% dat$nonsense_paths, ] : 
  #   incorrect number of dimensions
  # In addition: Warning message:
  #   In coefs$id <- rep(names(store$coefs), sapply(store$coefs, nrow)) :
  #   Coercing LHS to a list