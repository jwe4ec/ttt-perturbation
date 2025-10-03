# ---------------------------------------------------------------------------- #
# Define Functions for Fitting Saturated Models
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Define code_satur_gimme_model_paths() ----
# ---------------------------------------------------------------------------- #

# Define function to create GIMME paths for saturated model where "ar = TRUE"
# and "VAR = TRUE" from example participant's data matrix

# TODO: Consider rewriting to use "gimme" approaches in setupBaseSyntax.R
# - See https://github.com/GatesLab/gimme/blob/master/R/setupBaseSyntax.R





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
    paste0(vars_current[x["row"]], "~", vars_lagged[x["col"]])
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

# Define function to create lagged variables (append "lag" to current variables) 
# using GIMME method in "setupTransformData.R"
# - https://github.com/GatesLab/gimme/blob/a633a143108315941a2d09701ec7c204b4742087/R/setupTransformData.R#L131-L158

create_lagged_vars_for_satur_gimme_model <- function(dat_mat_ls) {
  dat_mat_ls <- lapply(dat_mat_ls, function(mat) {
    first           <- mat[1:(nrow(mat) - 1), ]
    second          <- mat[2:(nrow(mat)), ]
    ts_lc           <- cbind(first, second)
    colnames(ts_lc) <- c(paste0(colnames(mat), "lag"), colnames(mat))
    
    ts_lc
  })
}

# ---------------------------------------------------------------------------- #
# Define setup_limited_dat() ----
# ---------------------------------------------------------------------------- #

# Define function to create limited version of "dat" for use in testWeights() below
# and for compiling results using GIMME approach in "setup.R"
# - https://github.com/GatesLab/gimme/blob/master/R/setup.R

# TODO: Add these elements to "dat" list to compile results
#   dat$candidate_paths # Don't think it's needed for saturated model
#   dat$candidate_corr  # Don't think it's needed for saturated model





setup_limited_dat <- function(data_file, out, plot) {
  # Note: Simplified the following for our case
  
  ctrlOpts <- list(out     = out,
                   ind_dir = file.path(out, "individual"),
                   plot    = plot)
  
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
  
  dat <- list(out          = ctrlOpts$out,
              plot         = ctrlOpts$plot,
              n_lagged     = length(varLabels$lagg),
              n_endog      = length(varLabels$endo),
              n_vars_total = length(varLabels$coln),
              varnames     = varLabels$coln,
              ind_dir      = ctrlOpts$ind_dir,
              varLabels    = varLabels,
              ctrlOpts     = ctrlOpts)
  
  return(dat)
}

# ---------------------------------------------------------------------------- #
# Define fit.model() ----
# ---------------------------------------------------------------------------- #

# Define function to fit lavaan model from GIMME's "fit.model.R"
# - https://github.com/GatesLab/gimme/blob/master/R/fit.model.R

fit.model <- function(syntax, data_file) {
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
  
  return(fit)
}

# ---------------------------------------------------------------------------- #
# Define testWeights() ----
# ---------------------------------------------------------------------------- #

# Define testWeights() for checking TODO from GIMME's "search.paths.ind.R"
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
# Define w2e() ----
# ---------------------------------------------------------------------------- #

# Define function for creating edge list from weight matrix from GIMME's "w2e.R"
# - https://github.com/GatesLab/gimme/blob/master/R/w2e.R

w2e <- function(x) cbind(which(x != 0, arr.ind = TRUE), x[x != 0])

# ---------------------------------------------------------------------------- #
# Define fit_check_compile_satur_model_ind() ----
# ---------------------------------------------------------------------------- #

# Define function to fit saturated idiographic VAR model, check convergence, and
# compile results for one participant using GIMME approaches in "search.paths.ind.R"
# - https://github.com/GatesLab/gimme/blob/master/R/search.paths.ind.R

fit_check_compile_satur_model_ind <- function(data_file, part_id, out, plot, satur_syntax) {
  # Set up limited "dat" object for testWeights() and for compiling results
  
  dat <- setup_limited_dat(data_file, out, plot)
  
  # Fit model
  
  fit <- fit.model(satur_syntax, data_file)
  
  # Check for convergence if no error during model-fitting
  
  if (!inherits(fit, "try-error")) {
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
    
    if (converge & !zero_se & !test_weights) {
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
  
  # Compile and export results and plot if model converged
  
  op           <- NULL
  ind_plot     <- NA
  ind_plot_psi <- NA
  
  fit_measures <- c("chisq", "df", "npar", "pvalue", "rmsea",    # TODO: Added so number of fit measures and coefs aren't
                    "srmr", "nnfi", "cfi", "bic", "aic", "logl") #       hardcoded below if model didn't converge
  ind_coefs_cols <- c("lhs", "op", "rhs", "est", "est.std", 
                      "se", "z", "pvalue", "ci.lower", "ci.upper")
  
  if (converge & !zero_se) {
    # Compile results
    
    ind_fit         <- fitMeasures(fit, fit_measures)   # TODO: Used "fit_measures" instead of listing
    ind_fit         <- round(ind_fit, digits = 4)
    ind_fit[["df"]] <- round(ind_fit[["df"]], digits = 0)   # TODO: JE edited to avoid hard coding

    r2 <- inspect(fit, "rsquare")
    r2 <- r2[dat$varLabels$endo]

    ind_fit <- c(ind_fit, round(r2, digits = 4))

    ind_vcov_full <- lavInspect(fit, "vcov.std.all")
    # keep          <- rownames(ind_vcov_full) %in% dat$candidate_paths   # TODO: Add "candidate_paths" to "dat",
    # ind_vcov      <- ind_vcov_full[keep, keep]                          #   but may not be needed for saturated model.
    #                                                                     #   In our case they're all but the AR effects.
    ind_vcov <- ind_vcov_full                                             # Try keeping them all
    
    ind_coefs_unst0 <- parameterEstimates(fit)                            # TODO: lavaan changes the lhs/rhs of some covariances
                                                                          #   in "lavInspect", "parameterEstimates", and
                                                                          #   "standardizedSolution" (seems to matter for 
                                                                          #   restricted models--see filtering below)
    # ind_coefs_unst_idx <- paste0(ind_coefs_unst0$lhs, ind_coefs_unst0$op, ind_coefs_unst0$rhs)
    # ind_coefs_unst <- ind_coefs_unst0[ind_coefs_unst0$op == "~" |
    #                                     ind_coefs_unst_idx %in% c(dat$candidate_paths, dat$candidate_corr), ]   # TODO: Keep all?
    ind_coefs_unst <- ind_coefs_unst0                                                                             # Try keeping them all

    ind_coefs0 <- standardizedSolution(fit)
    # ind_coefs_idx <- paste0(ind_coefs0$lhs,ind_coefs0$op,ind_coefs0$rhs)
    # ind_coefs <- ind_coefs0[ind_coefs0$op == "~" |
    #                           ind_coefs_idx %in% c(dat$candidate_paths, dat$candidate_corr), ]   # TODO: Keep all?
    ind_coefs <- ind_coefs0                                                                        # Try keeping them all
    
    ind_coefs <- data.frame(ind_coefs[c("lhs", "op", "rhs")],   # TODO: JE edited to avoid hardcoding
                            est = ind_coefs_unst$est,
                            ind_coefs[c("est.std", "se", "z", "pvalue", "ci.lower", "ci.upper")])
    if(!all(names(ind_coefs) == ind_coefs_cols)) stop("Update 'ind_coefs_cols'")
    
    ind_betas     <- round(lavInspect(fit, "std")$beta, digits = 4)
    ind_ses       <- round(lavInspect(fit, "se")$beta, digits = 4)
    ind_psi       <- round(lavInspect(fit, "std")$psi, digits = 4)
    ind_psi_unstd <- round(lavInspect(fit, "estimates")$psi, digits = 4)
    
    ## Ensure correct ordering in matrices
    
    ind_betas <- ind_betas[dat$varLabels$endo, ]
    ind_betas <- ind_betas[, dat$varLabels$coln]
    
    ind_ses <- ind_ses[dat$varLabels$endo, ]
    ind_ses <- ind_ses[, dat$varLabels$coln]

    ind_psi <- ind_psi[dat$varLabels$endo, ]
    ind_psi <- ind_psi[, dat$varLabels$coln]
    
    ind_psi_unstd <- ind_psi_unstd[dat$varLabels$endo, ]
    ind_psi_unstd <- ind_psi_unstd[, dat$varLabels$coln]
    
    # Export results
    # - Note: Simplified the following for our case
    #   - Assumed that "agg" is FALSE
    #   - Replaced "dat$file_order[k, 2]" with "part_id" (TODO: Ultimately need to change back)
    
    if (!is.null(dat$out)) {
      if (!dir.exists(dat$ind_dir)) dir.create(dat$ind_dir, recursive = TRUE)   # TODO: JE added (but consider doing it GIMME's way)
      
      write.csv(ind_betas,     file.path(dat$ind_dir, paste0(part_id, "BetasStd.csv")), row.names = TRUE)
      write.csv(ind_psi,       file.path(dat$ind_dir, paste0(part_id, "Psi.csv")), row.names = TRUE)
      write.csv(ind_psi_unstd, file.path(dat$ind_dir, paste0(part_id, "PsiUnstd.csv")), row.names = TRUE)
      write.csv(ind_ses,       file.path(dat$ind_dir, paste0(part_id, "StdErrors.csv")), row.names = TRUE)
    }
    
    # Plot results
    
    if (dat$plot) {
      ind_betas_t <- t(ind_betas)
      lagged      <- ind_betas_t[1:dat$n_lagged, ]
      contemp     <- ind_betas_t[(dat$n_lagged + 1):(dat$n_vars_total), ]
      plot_vals   <- rbind(w2e(lagged), w2e(contemp))
      is_lagged   <- c(rep(TRUE, sum(lagged != 0)), 
                       rep(FALSE, sum(contemp != 0)))
      
      # Note: Simplified the following for our case
      # - Assumed that "agg" and "hybrid" is FALSE
      # - Replaced "dat$file_order[k, 2]" with "part_id"
      
      plot_file <- file.path(dat$ind_dir, paste0(part_id, "Plot.pdf"))
      
      ind_plot <- try(qgraph(plot_vals,
                             layout       = "circle",
                             lty          = ifelse(is_lagged, 2, 1),
                             edge.labels  = FALSE,
                             curve        = FALSE,
                             parallelEdge = TRUE,
                             fade         = FALSE,
                             posCol       = "red",
                             negCol       = "blue",
                             labels       = dat$varnames[(dat$n_lagged + 1):(dat$n_vars_total)],
                             label.cex    = 2,
                             DoNotPlot    = TRUE))
      
      if (!is.null(dat$out) & !inherits(ind_plot, "try-error")) {
        pdf(plot_file)
        plot(ind_plot)
        dev.off()
      }
      
      ind_plot_psi <- NA # This is only for hybrid models
    }
  }
  
  # Compile results if model did not converge
  
  if (!converge | zero_se) {
    status1 <- "nonconvergence"                  # TODO: Wasn't this already assigned above?
    ind_fit   <- rep(NA, length(fit_measures))                       # TODO: Edited this and next 2 lines to avoid hardcoding
    ind_coefs <- matrix(NA, nrow = 1, ncol = length(ind_coefs_cols))
    colnames(ind_coefs) <- ind_coefs_cols
    ind_betas     <- NA
    ind_vcov      <- NA
    ind_plot      <- NA
    ind_plot_psi  <- NA
    ind_psi       <- NA
    ind_psi_unstd <- NA
    ind_vcov_full <- NA   # TODO: "ind_vcov" and "ind_vcov_full" are redundant for saturated model
  }
  
  # Wrap up
  
  new.obj <- list(status        = status1,     # TODO: Returned by GIMME
                  ind_fit       = ind_fit, 
                  ind_coefs     = ind_coefs,
                  ind_betas     = ind_betas,
                  ind_vcov      = ind_vcov,
                  ind_plot      = ind_plot,
                  ind_plot_psi  = ind_plot_psi, 
                  ind_psi       = ind_psi, 
                  ind_psi_unstd = ind_psi_unstd, 
                  ind_vcov_full = ind_vcov_full,
                  syntax        = satur_syntax)
  
  new.obj$testing <- list(dat           = dat,        # TODO: Return additional elements for testing
                          fit           = fit,
                          converge      = converge,
                          zero_se       = zero_se,
                          na_se         = na_se,
                          test_weights  = test_weights,
                          indices       = indices)
  
  return(new.obj)
}

# ---------------------------------------------------------------------------- #
# Define fit_check_compile_satur_models() ----
# ---------------------------------------------------------------------------- #

# Define function to fit saturated idiographic VAR models, check convergence, and
# compile results for all participants using GIMME approaches in "indiv.search.R"
# - https://github.com/GatesLab/gimme/blob/master/R/indiv.search.R

fit_check_compile_satur_models <- function(dat_mat_ls, out = NULL, plot = TRUE, satur_syntax) {
  # Note: Simplified for our case
  # - Assuming "agg" and "hybrid" are FALSE
  
  n_ind <- length(dat_mat_ls)    # TODO: Add "n_subj" to "dat"
  
  status    <- vector("list", n_ind)  # TODO: JE edited to preallocate length
  fits      <- vector("list", n_ind)
  coefs     <- vector("list", n_ind)
  betas     <- vector("list", n_ind)
  vcov      <- vector("list", n_ind)
  vcovfull  <- vector("list", n_ind)
  plots     <- vector("list", n_ind)
  syntax    <- vector("list", n_ind)
  psi       <- vector("list", n_ind)
  psiunstd  <- vector("list", n_ind)
  plots_cov <- vector("list", n_ind)
  testing   <- vector("list", n_ind)  # TODO: For testing
  
  for (k in 1:n_ind) {
    data_file <- dat_mat_ls[[k]]
    part_id <- names(dat_mat_ls)[k]
    
    writeLines(paste0("fitting individual-level model, subject ", k, " (", part_id,")"))
    
    ind_spec <- fit_check_compile_satur_model_ind(data_file, part_id, out, plot, satur_syntax)
    
    status[[k]]    <- ind_spec$status           # TODO: Returned by GIMME
    fits[[k]]      <- ind_spec$ind_fit
    coefs[[k]]     <- ind_spec$ind_coefs
    betas[[k]]     <- ind_spec$ind_betas
    vcov[[k]]      <- ind_spec$ind_vcov
    vcovfull[[k]]  <- ind_spec$ind_vcov_full
    plots[[k]]     <- ind_spec$ind_plot
    plots_cov[[k]] <- ind_spec$ind_plot_psi
    syntax[[k]]    <- ind_spec$syntax
    psi[[k]]       <- ind_spec$ind_psi
    psiunstd[[k]]  <- ind_spec$ind_psi_unstd
    testing[[k]]   <- ind_spec$testing          # TODO: Return additional elements for testing
  }
  
  names(status) <- names(fits) <- names(coefs) <-       # TODO: Added names for "plots_cov" and "syntax"
    names(betas) <- names(vcov) <- names(vcovfull) <- names(plots) <- names(plots_cov) <-
    names(syntax) <- names(psi) <- names(psiunstd) <- names(testing) <- names(dat_mat_ls)  # TODO: "testing" for testing
  
  res <- list(status    = status,
              fits      = fits,
              coefs     = coefs,
              betas     = betas,
              psi       = psi,
              psiunstd  = psiunstd,
              vcov      = vcov,
              vcovfull  = vcovfull,
              plots     = plots,
              plots_cov = plots_cov,
              syntax    = syntax,
              testing   = testing)   # TODO: For testing
  
  return(res)
}