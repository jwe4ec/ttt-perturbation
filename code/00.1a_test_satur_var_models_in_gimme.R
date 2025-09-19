# ---------------------------------------------------------------------------- #
# Test Saturated Idiographic VAR Models in GIMME ----
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

groundhog.library("gimme", groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Load data ----
# ---------------------------------------------------------------------------- #

# Load example data for 5 individuals, each with 50 observations on 3 variables

dat_ls <- ts

# Convert data frames to matrices

dat_mat_ls <- lapply(dat_ls, as.matrix)

# ---------------------------------------------------------------------------- #
# Explore "dat" object created by GIMME's "setup()" in course of model fitting ----
# ---------------------------------------------------------------------------- #

# To save "dat" object before and after "indSEM()" adjusts candidate paths when
# VAR is TRUE, when prompted by trace(), replace the following code...

# if (VAR) {
#   dat$candidate_paths <- grep("*lag", dat$candidate_paths, 
#                               value = TRUE)
# }

# ...with this code...

# dat_before_var_arg <- dat
# save(dat_before_var_arg, file = "./results/gimme/ts_test/raw_ar_var/dat_before_var_arg.RData")
# if (VAR) {
#   dat$candidate_paths <- grep("*lag", dat$candidate_paths,
#                               value = TRUE)
# }
# dat_after_var_arg <- dat
# save(dat_after_var_arg, file = "./results/gimme/ts_test/raw_ar_var/dat_after_var_arg.RData")
# stop("Stop test")

trace(indSEM, edit = TRUE)

var_res_ls <- indSEM(dat_mat_ls, "./results/gimme/ts_test/raw_ar_var/",
                     ar = TRUE, VAR = TRUE)

untrace(indSEM)

load("./results/gimme/ts_test/raw_ar_var/dat_before_var_arg.RData")
load("./results/gimme/ts_test/raw_ar_var/dat_after_var_arg.RData")

# Explore "dat" before "indSEM()" adjusts candidate paths when VAR is TRUE

dat_before_var_arg$zero.paths      # (empty)
dat_before_var_arg$fixed_paths     # Autoregressive paths
dat_before_var_arg$nonsense_paths  # Current predicting lag
dat_before_var_arg$candidate_paths # Contemporaneous and cross-lagged relations
dat_before_var_arg$candidate_corr  # TODO (describe)
dat_before_var_arg$syntax          # TODO (describe)

dat_before_var_arg$agg             # FALSE
dat_before_var_arg$n_lagged          == 3
dat_before_var_arg$n_exog_total      == 0
dat_before_var_arg$n_endog           == 3
dat_before_var_arg$n_vars_total      == 6
dat_before_var_arg$n_contemporaneous == 3
all(dat_before_var_arg$varnames  == c("V1lag", "V2lag", "V3lag", "V1", "V2", "V3"))
all(dat_before_var_arg$lvarnames == c("V1lag", "V2lag", "V3lag", "V1", "V2", "V3"))

all(dat_before_var_arg$varLabels$orig == c("V1", "V2", "V3"))
all(dat_before_var_arg$varLabels$lagg == c("V1lag", "V2lag", "V3lag"))
all(dat_before_var_arg$varLabels$exog == c("V1lag", "V2lag", "V3lag"))
all(dat_before_var_arg$varLabels$endo == c("V1", "V2", "V3"))
all(dat_before_var_arg$varLabels$coln == c("V1lag", "V2lag", "V3lag", "V1", "V2", "V3"))

# Explore "dat" after "indSEM()" adjusts candidate paths when VAR is TRUE (which
# changes "hybrid" from default of FALSE to TRUE)

  # All above slots are the same except "candidate_paths"

dat_after_var_arg$candidate_paths # Only cross-lagged relations (no contemporaneous)

# TODO: Continue comparing paths against saturated paths from my function below






# ---------------------------------------------------------------------------- #
# Try to fit saturated GIMME idiographic VAR models ----
# ---------------------------------------------------------------------------- #

# Create paths for saturated model from example participant's data matrix using helper function

satur_gimme_paths <- create_satur_gimme_paths(dat_mat_ls[[1]])

# Create lagged variables per GIMME method using helper function

dat_mat_ls <- create_lagged_vars_for_satur_gimme_model(dat_mat_ls)

# Fit saturated idiographic VAR models (as when "ar = TRUE" and "VAR = TRUE")

satur_var_res_ls <- indSEM(dat_mat_ls, "./results/gimme/ts_test/raw_satur/",
                           paths = satur_gimme_paths$all$paths)

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

satur_var_res_ls <- indSEM(dat_mat_ls, "./results/gimme/ts_test/raw_satur/",
                           paths = satur_gimme_paths$all$paths, VAR = TRUE)

# Per Gates and Molenaar (2012), GIMME starts with an empty model, so it seems
# that it never estimates a saturated model. Thus, try fitting saturated model
# with GIMME paths directly in "lavaan" instead.