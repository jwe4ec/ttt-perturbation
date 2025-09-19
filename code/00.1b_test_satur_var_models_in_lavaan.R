# ---------------------------------------------------------------------------- #
# Test Saturated Idiographic VAR Models in lavaan ----
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Check R version and load packages ----
# ---------------------------------------------------------------------------- #

# Load custom functions

source("./code/01_define_functions.R")
source("./code/01b_define_functions_satur_models.R")

# Check R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages and set seed

groundhog.library(c("gimme", "lavaan"), groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Load data ----
# ---------------------------------------------------------------------------- #

# Load example data for 5 individuals, each with 50 observations on 3 variables

dat_ls <- ts

# Convert data frames to matrices

dat_mat_ls <- lapply(dat_ls, as.matrix)

# ---------------------------------------------------------------------------- #
# Fit saturated idiographic VAR models with GIMME paths in "lavaan" ----
# ---------------------------------------------------------------------------- #

# Create paths for saturated model from example participant's data matrix using helper function

satur_gimme_paths <- create_satur_gimme_paths(dat_mat_ls[[1]])

# Create lagged variables per GIMME method using helper function

dat_mat_ls <- create_lagged_vars_for_satur_gimme_model(dat_mat_ls)

# Try GIMME's model-fitting approach in "fit.model()" and check convergence using 
# GIMME approach in "search.paths.ind()"
# - https://github.com/GatesLab/gimme/blob/master/R/setup.R
# - https://github.com/GatesLab/gimme/blob/master/R/fit.model.R
# - https://github.com/GatesLab/gimme/blob/master/R/search.paths.ind.R

# Fit saturated idiographic VAR model and check convergence on example data for 5 participants

results_ls <- lapply(dat_mat_ls, fit_and_check_satur_model, syntax = satur_gimme_paths$all$paths)

all(sapply(results_ls, function(x) x$converge))                        # All models converged
all(sapply(results_ls, function(x) !x$zero_se))                        # None had zero SE
all(sapply(results_ls, function(x) !x$na_se))                          # None had NA SE
all(sapply(results_ls, function(x) !x$test_weights))                   # None had bad test weights
all(sapply(results_ls, function(x) x$status1 == "converged normally")) # All converged normally

indices <- t(sapply(results_ls, function(x) x$indices))
indices <- as.data.frame(round(indices, 4))

all(indices[, c("chisq", "df", "rmsea", "srmr")] == 0)  # All have "chisq", "df", "rmsea", and "srmr" of 0
all(indices[, c("nnfi", "cfi")]                  == 1)  # All have "nnfi" and "cfi" of 1
all(is.na(indices$pvalue))                              # All have NA for "pvalue"





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




