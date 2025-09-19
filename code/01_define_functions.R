# ---------------------------------------------------------------------------- #
# Define Functions
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Define version_control() ----
# ---------------------------------------------------------------------------- #

# Define function to check R version, load groundhog package, and return groundhog_day

version_control <- function() {
  # Ensure you are using the same version of R used at the time the script was 
  # written. To install a previous version, go to 
  # https://cran.r-project.org/bin/windows/base/old/
  
  script_R_version <- "R version 4.3.2 (2023-10-31 ucrt)"
  current_R_version <- R.Version()$version.string
  
  if(current_R_version != script_R_version) {
    warning(paste0("This script is based on ", script_R_version,
                   ". You are running ", current_R_version, "."))
  }
  
  # Load packages using "groundhog", which installs and loads the most recent
  # versions of packages available on the specified date ("groundhog_day"). This 
  # is important for reproducibility so that everyone running the script is using
  # the same versions of packages used at the time the script was written.
  
  # Note that packages may take longer to load the first time you load them with
  # "groundhog.library". This is because you may not have the correct versions of 
  # the packages installed based on the "groundhog_day". After "groundhog.library"
  # automatically installs the correct versions alongside other versions you may 
  # have installed, it will load the packages more quickly.
  
  # If in the process of loading packages with "groundhog.library" for the first 
  # time the console states that you first need to install "Rtools", follow steps 
  # here (https://cran.r-project.org/bin/windows/Rtools/) for installing "Rtools" 
  # and putting "Rtools" on the PATH. Then try loading the packages again.
  
  library(groundhog)
  meta.groundhog("2024-02-15")
  groundhog_day <- "2024-02-15"
  
  return(groundhog_day)
}

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
# Define fit_and_check_satur_model() ----
# ---------------------------------------------------------------------------- #

# Define function to fit saturated idiographic VAR model and check convergence
# using GIMME's approaches in "fit.model()" and "search.paths.ind()"
# - https://github.com/GatesLab/gimme/blob/master/R/fit.model.R
# - https://github.com/GatesLab/gimme/blob/master/R/search.paths.ind.R

fit_and_check_satur_model <- function(data_file, syntax) {
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
    converge <- lavaan::lavInspect(fit, "converged")
    zero_se  <- sum(lavInspect(fit, "se")$beta, na.rm = TRUE) == 0
    na_se    <- any(is.na(lavInspect(fit, what = "list")$se))
    
    if (converge & !na_se) { 
      indices <- fitMeasures(fit, c("chisq", "df", "pvalue", "rmsea", "srmr", "nnfi", "cfi"))
    } else {
      indices <- NULL
    }
  } else {
    indices  <- NULL
    converge <- FALSE
    zero_se  <- TRUE
    nonconv  <- TRUE
  }
  
  # Return fit and convergence indicators in list
  
  results <- list(fit      = fit,
                  converge = converge,
                  zero_se  = zero_se,
                  na_se    = na_se,
                  indices  = indices)
  
  return(results)
}

# ---------------------------------------------------------------------------- #
# Define create_var_labels() ----
# ---------------------------------------------------------------------------- #

# Define function to create variable labels

create_var_labels <- function(vars) {
  var_labels <- vars
  
  var_labels[var_labels == "bad"]      <- "Bad Self"
  var_labels[var_labels == "control"]  <- "Lack Control"
  var_labels[var_labels == "energy"]   <- "Fatigue"
  var_labels[var_labels == "focus"]    <- "Lack Focus"
  var_labels[var_labels == "fun"]      <- "Inaction"
  var_labels[var_labels == "interest"] <- "Lack Interest"
  var_labels[var_labels == "movement"] <- "Slower or Fidgety"
  var_labels[var_labels == "sad"]      <- "Sad"
  
  return(var_labels)
}