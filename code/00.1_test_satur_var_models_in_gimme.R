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
# Try to fit saturated GIMME idiographic VAR models ----
# ---------------------------------------------------------------------------- #

# Create paths for saturated model from example participant's data matrix using function above

satur_gimme_paths <- create_satur_gimme_paths(dat_mat_ls[[1]])

# Create lagged variables per GIMME method using function above

dat_mat_ls <- create_lagged_vars_for_satur_gimme_model(dat_mat_ls)

# Fit saturated idiographic VAR models (as when "ar = TRUE" and "VAR = TRUE")

# satur_var_res_ls <- indSEM(dat_mat_ls, "./results/gimme/test/raw/",
#                            paths = satur_gimme_paths$all$paths)

  # TODO: Resolve errors like this for each individual:
  #   individual-level search, subject 1 (ts1)
  #   Error in lav_parse_model_string_orig(model.syntax = model.syntax, as.data.frame. = as.data.frame.,  : 
  #     lavaan ERROR: duplicate model element in: V1~~V1
  #   Error in lav_parse_model_string_orig(model.syntax = model.syntax, as.data.frame. = as.data.frame.,  : 
  #     lavaan ERROR: duplicate model element in: V1~~V1
  #   Error in lav_parse_model_string_orig(model.syntax = model.syntax, as.data.frame. = as.data.frame.,  : 
  #     lavaan ERROR: duplicate model element in: V1~~V1

  # TODO: Resolve other errors and warnings:
  #   Error in coefs[!coefs$param %in% dat$nonsense_paths, ] : 
  #     incorrect number of dimensions
  #   In addition: Warning message:
  #     In coefs$id <- rep(names(store$coefs), sapply(store$coefs, nrow)) :
  #     Coercing LHS to a list

  # Try specifying "VAR = TRUE" per Katie Gates's advice on 7/29/2025, who said doing 
  # so may resolve the first set of errors (and said the second set of errors may be
  # due to convergence issues. However, both sets of errors remain.

# satur_var_res_ls <- indSEM(dat_mat_ls, "./results/gimme/test/raw/",
#                            paths = satur_gimme_paths$all$paths, VAR = TRUE)

# Per Gates and Molenaar (2012), GIMME starts with an empty model, so it seems
# that it never estimates a saturated model. Thus, try fitting saturated model
# with GIMME paths directly in "lavaan" instead.

# ---------------------------------------------------------------------------- #
# Fit saturated idiographic VAR models with GIMME paths in "lavaan" ----
# ---------------------------------------------------------------------------- #

# Try GIMME's model-fitting approach in "fit.model()" and check convergence using 
# GIMME approach in "search.paths.ind()"
# - https://github.com/GatesLab/gimme/blob/master/R/setup.R
# - https://github.com/GatesLab/gimme/blob/master/R/fit.model.R
# - https://github.com/GatesLab/gimme/blob/master/R/search.paths.ind.R

library(lavaan)

# Fit saturated idiographic VAR model and check convergence on example data for 5 participants

results_ls <- lapply(dat_mat_ls, fit_and_check_satur_model, syntax = satur_gimme_paths$all$paths)

all(sapply(results_ls, function(x) x$converge))  # All models converged
all(sapply(results_ls, function(x) !x$zero_se))  # None had zero SE
all(sapply(results_ls, function(x) !x$na_se))    # None had NA SE

indices <- t(sapply(results_ls, function(x) x$indices))
indices <- as.data.frame(round(indices, 4))

all(indices[, c("chisq", "df", "rmsea", "srmr")] == 0)  # All have "chisq", "df", "rmsea", and "srmr" of 0
all(indices[, c("nnfi", "cfi")]                  == 1)  # All have "nnfi" and "cfi" of 1
all(is.na(indices$pvalue))                              # All have NA for "pvalue"





# TODO (create "dat"; see GIMME's "setup()"): Define function for testing stability

testWeights <- function(fit, dat) {
  ind_betas <- round(lavInspect(fit, "std")$beta, digits = 4)
  #added to ensure correct ordering in matrices
  ind_betas <- ind_betas[dat$varLabels$endo, ]
  ind_betas <- ind_betas[, dat$varLabels$coln]
  test      <- any(Re(eigen(ind_betas[, 1:dat$n_endog])$values) >= 1) | 
    any(Re(eigen(ind_betas[, (dat$n_endog + 1):(dat$n_endog * 2)])$values) >= 1)
  
  return(test)
}

# TODO: Create "dat" object so "testWeights()" can be tested
# - See "search.paths.ind()" where "dat" is 
# "@param dat Object created at beginning of gimme containing static info."
# - Find GIMME code where "search.paths.ind()" is called to see where "dat" is created

# if (converge & !zero_se & !testWeights(fit, dat)){
#   status1 <- "converged normally"
#   nonconv <- FALSE
# } else {
#   # if no convergence or unstable
#   if (!converge | zero_se | testWeights(fit, dat)) {
#       if (testWeights(fit, dat) | zero_se)
#         status1 <- "unstable solution"
#         break
#       if (!converge)
#         status1 <- "nonconvergence"
#         break
#   }
# }




# TODO: Find where in GIMME "add_p$goodfit" is created and consider evaluating
# model fit based on that procedure




# Compile results if model converged

op           <- NULL # appease CRAN check
ind_plot     <- NA
ind_plot_psi <- NA

if (converge & !zero_se) {
  ind_fit    <- fitMeasures(fit, c("chisq", "df", "npar", "pvalue", "rmsea", 
                                   "srmr", "nnfi", "cfi", "bic", "aic", "logl"))
  ind_fit    <- round(ind_fit, digits = 4)
  ind_fit[2] <- round(ind_fit[2], digits = 0)
  
  r2         <- inspect(fit, "rsquare")
  # r2         <- r2[dat$varLabels$endo]   # TODO: Find out where "dat" is created and create it
  
  ind_fit    <- c(ind_fit, round(r2, digits = 4))
  
  ind_vcov_full <- lavInspect(fit, "vcov.std.all")
  # keep          <- rownames(ind_vcov_full) %in% dat$candidate_paths   # TODO: Find out where "dat" is created and create it,
  # ind_vcov      <- ind_vcov_full[keep, keep]                          #       but this may not be needed for saturated model
  
  ind_coefs_unst0 <- parameterEstimates(fit)
  ind_coefs_unst_idx <- paste0(ind_coefs_unst0$lhs, ind_coefs_unst0$op, ind_coefs_unst0$rhs)
  # ind_coefs_unst <- ind_coefs_unst0[ind_coefs_unst0$op == "~" |
  #                                     ind_coefs_unst_idx %in% c(dat$candidate_paths, dat$candidate_corr), ]   # TODO: Create "dat"
  
  ind_coefs0 <- standardizedSolution(fit)
  ind_coefs_idx <- paste0(ind_coefs0$lhs,ind_coefs0$op,ind_coefs0$rhs)
  # ind_coefs <- ind_coefs0[ind_coefs0$op == "~" |
  #                           ind_coefs_idx %in% c(dat$candidate_paths, dat$candidate_corr), ]   # TODO: Create "dat"
  
  # ind_coefs <- cbind(ind_coefs[, 1:3], ind_coefs_unst$est, ind_coefs[, 4:9])
  # colnames(ind_coefs) <- c("lhs", "op", "rhs", "est", "est.std", "se", "z", "pvalue", "ci.lower", "ci.upper")
  
  #ind_coefs <- subset(standardizedSolution(fit), op == "~")
  
  ind_betas <- round(lavInspect(fit, "std")$beta, digits = 4)
  ind_ses   <- round(lavInspect(fit, "se")$beta, digits = 4)
  
  #added to ensure correct ordering in matrices
  # ind_betas <- ind_betas[dat$varLabels$endo, ]   # TODO: Create "dat"
  # ind_betas <- ind_betas[, dat$varLabels$coln]
  
  # ind_ses <- ind_ses[dat$varLabels$endo, ]
  # ind_ses <- ind_ses[, dat$varLabels$coln]
  
  # zf added 2019-01-23
  ind_psi <- round(lavInspect(fit, "std")$psi, digits = 4)
  ind_psi_unstd <- round(lavInspect(fit, "estimates")$psi, digits = 4)
  
  # ind_psi <- ind_psi[dat$varLabels$endo, ]  # TODO: Create "dat"
  # ind_psi <- ind_psi[, dat$varLabels$coln]
  # ind_psi_unstd <- ind_psi_unstd[dat$varLabels$endo, ]
  # ind_psi_unstd <- ind_psi_unstd[, dat$varLabels$coln]
  
  #rownames(ind_betas) <- rownames(ind_ses) <- dat$varnames[(dat$n_lagged+1):(dat$n_vars_total)]
  #colnames(ind_betas) <- colnames(ind_ses) <- dat$varnames
  #   } # stl comment out 11.20.17 
  
  if (dat$agg & !is.null(dat$out)) {   # TODO: Continue here once "dat" is created
    
    write.csv(ind_betas, file.path(dat$out, "allBetas.csv"), 
              row.names = TRUE)
    
    # write.csv(ind_vcov_full, file.path(dat$out, "allvcov.csv"), 
    #           row.names = TRUE)
    
    write.csv(ind_ses, file.path(dat$out, "allStdErrors.csv"), 
              row.names = TRUE)
    
    # zf added 2019-01-23
    write.csv(ind_psi, file.path(dat$out, "allPsi.csv"),row.names = TRUE)
    write.csv(ind_psi_unstd, file.path(dat$out, "allPsiUnstd.csv"),row.names = TRUE)
    
  } else if (!dat$agg & !is.null(dat$out)) { # & ind$n_ind_paths[k]>0)
    write.csv(ind_betas, file.path(dat$ind_dir, 
                                   paste0(dat$file_order[k,2], 
                                          "BetasStd.csv")), row.names = TRUE)
    
    # write.csv(ind_vcov_full, file.path(dat$ind_dir, 
    #                                paste0(dat$file_order[k,2], 
    #                                       "vcov.csv")), row.names = TRUE)
    # zf added 2019-01-23
    write.csv(ind_psi, file.path(dat$ind_dir, 
                                 paste0(dat$file_order[k,2], 
                                        "Psi.csv")), row.names = TRUE)
    write.csv(ind_psi_unstd, file.path(dat$ind_dir, 
                                       paste0(dat$file_order[k,2], 
                                              "PsiUnstd.csv")), row.names = TRUE)
    write.csv(ind_ses, file.path(dat$ind_dir,
                                 paste0(dat$file_order[k,2], 
                                        "StdErrors.csv")), row.names = TRUE)
  }
  
  if (dat$plot){
    ind_betas_t <- t(ind_betas)
    lagged      <- ind_betas_t[1:dat$n_lagged, ]
    contemp     <- ind_betas_t[(dat$n_lagged+1):(dat$n_vars_total), ]
    plot_vals   <- rbind(w2e(lagged), w2e(contemp))
    is_lagged   <- c(rep(TRUE, sum(lagged != 0)), 
                     rep(FALSE, sum(contemp != 0)))
    
    plot_file   <- ifelse(dat$agg, 
                          file.path(dat$out, "summaryPathsPlot.pdf"),
                          file.path(dat$ind_dir, paste0(dat$file_order[k,2], "Plot.pdf")))
    
    ind_plot <- try(qgraph(plot_vals,
                           layout       = "circle",
                           lty          = ifelse(is_lagged, 2, 1),
                           edge.labels  = FALSE,
                           curve        = FALSE,
                           parallelEdge = TRUE,
                           fade         = FALSE,
                           posCol       = "red",
                           negCol       = "blue",
                           labels       = 
                             dat$varnames[(dat$n_lagged+1):(dat$n_vars_total)],
                           label.cex    = 2,
                           DoNotPlot    = TRUE))
    
    if (!is.null(dat$out) & !inherits(ind_plot, "try-error")){
      pdf(plot_file)
      plot(ind_plot)
      dev.off()
    }
    if(dat$hybrid){
      covpsi     <- ind_psi[, (dat$n_lagged+1):(dat$n_vars_total)]
      covpsi[lower.tri(covpsi)] <- 0 # so we don't get duplicates
      diag(covpsi)    <- 0
      plot_vals_psi   <- w2e(covpsi)
      
      plot_file_psi   <- ifelse(dat$agg, 
                                file.path(dat$out, "summaryCovPlot.pdf"),
                                file.path(dat$ind_dir, 
                                          paste0(dat$file_order[k,2], "PlotCov.pdf")))
      
      ind_plot_psi <- try(qgraph(plot_vals_psi,
                                 layout       = "circle",
                                 lty          = 1,
                                 edge.labels  = FALSE,
                                 curve        = FALSE,
                                 parallelEdge = TRUE,
                                 fade         = FALSE,
                                 posCol       = "red",
                                 negCol       = "blue",
                                 arrows       = FALSE,
                                 labels       = 
                                   dat$varnames[(dat$n_lagged+1):(dat$n_vars_total)],
                                 label.cex    = 2,
                                 DoNotPlot    = TRUE))
      
      if (!is.null(dat$out) & !inherits(ind_plot_psi, "try-error")){
        pdf(plot_file_psi)
        plot(ind_plot_psi)
        dev.off()
      }
      
    } else {
      ind_plot_psi <- NA
    }
    
  }
}

# Compile results if model did not converge

if (!converge | zero_se) {
  status1 <- "nonconvergence"
  #if (sum(lavInspect(fit, "se")$beta, na.rm = TRUE) == 0) status <- "computationally singular"
  ind_fit   <- rep(NA, 11)
  ind_coefs <- matrix(NA, nrow = 1, ncol = 10)
  colnames(ind_coefs) <- c("lhs", "op", "rhs", "est", "est.std", "se", "z", "pvalue", "ci.lower", "ci.upper")
  ind_betas     <- NA
  ind_vcov      <- NA
  ind_plot      <- NA
  ind_plot_psi  <- NA
  ind_psi       <- NA
  ind_psi_unstd <- NA
  ind_vcov_full <- NA
}

  # Wrap up

syntax <- syntax
# name <- names(dat$ts_list)[k]   # TODO: Create "dat"

new.obj <- list(status        = status1, 
                ind_fit       = ind_fit, 
                ind_coefs     = ind_coefs,
                ind_betas     = ind_betas,
                ind_vcov      = ind_vcov,
                ind_plot      = ind_plot,
                ind_plot_psi  = ind_plot_psi, 
                ind_psi       = ind_psi, 
                ind_psi_unstd = ind_psi_unstd, 
                ind_vcov_full = ind_vcov_full,
                ind_paths     = obj[[1]]$add_syntax,
                syntax        = syntax)

## end get.params if n_sub == 1

return(new.obj)









