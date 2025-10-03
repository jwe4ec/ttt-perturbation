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

groundhog.library(c("gimme", "lavaan", "qgraph"), groundhog_day)

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
# - https://github.com/GatesLab/gimme/blob/master/R/indSEM.R
# - https://github.com/GatesLab/gimme/blob/master/R/setup.R
# - https://github.com/GatesLab/gimme/blob/master/R/setupBaseSyntax.R
# - https://github.com/GatesLab/gimme/blob/master/R/indiv.search.R
# - https://github.com/GatesLab/gimme/blob/master/R/search.paths.ind.R
# - https://github.com/GatesLab/gimme/blob/master/R/fit.model.R

# Fit saturated idiographic VAR model and check convergence on example data for 5 participants

results_ls <- fit_check_compile_satur_models(dat_mat_ls,
                                             "./results/test_lavaan_satur/",
                                             satur_syntax = satur_gimme_paths$all$paths)

all(sapply(results_ls$testing, function(x) x$converge))      # All models converged
all(sapply(results_ls$testing, function(x) !x$zero_se))      # None had zero SE
all(sapply(results_ls$testing, function(x) !x$na_se))        # None had NA SE
all(sapply(results_ls$testing, function(x) !x$test_weights)) # None had bad test weights
all(unlist(results_ls$status) == "converged normally")       # All converged normally

ind_fits <- as.data.frame(do.call(rbind, results_ls$fits))

all(ind_fits[c("chisq", "df", "rmsea", "srmr")] == 0)  # All have "chisq", "df", "rmsea", and "srmr" of 0
all(ind_fits[c("nnfi", "cfi")]                  == 1)  # All have "nnfi" and "cfi" of 1
all(is.na(ind_fits$pvalue))                            # All have NA for "pvalue"

results_ls$coefs$ts1
results_ls$betas$ts1
plot(results_ls$plots$ts1)
results_ls$vcov$ts1      # TODO: What is this?
results_ls$vcovfull$ts1  # TODO: Redundant with "ind_vcov" for saturated model
results_ls$psi$ts1       # TODO: What is this?
results_ls$psiunstd$ts1  # TODO: What is this?
results_ls$syntax$ts1

# TODO: Compile results further like GIMME "indSEM()"





