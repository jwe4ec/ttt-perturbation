# ---------------------------------------------------------------------------- #
# Fit GIMME Idiographic VAR Models -----
# Author: Jeremy W. Eberle
# ---------------------------------------------------------------------------- #

# ---------------------------------------------------------------------------- #
# Notes ----
# ---------------------------------------------------------------------------- #

# Before running script, restart R (CTRL+SHIFT+F10 on Windows) and set working 
# directory to parent folder

# ---------------------------------------------------------------------------- #
# Store working directory, check correct R version, load packages ----
# ---------------------------------------------------------------------------- #

# Store working directory

wd_dir <- getwd()

# Load custom functions

source("./code/01_define_functions.R")

# Check correct R version, load groundhog package, and specify groundhog_day

groundhog_day <- version_control()

# Load packages and set seed

groundhog.library("gimme", groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data and example IDs ----
# ---------------------------------------------------------------------------- #

load("./data/recentered/data_var_perturb2.RDS")

dat <- data_var_perturb2

# TODO: Temporarily load example IDs from later script "explore_ex_tem_networks.R"

load("./data/temp/retain_ids.RDS")





# ---------------------------------------------------------------------------- #
# Prepare data ----
# ---------------------------------------------------------------------------- #

dat_ls <- split(dat, dat$lifepak_id)

# TODO: Temporarily restrict sample until ready to analyze full sample

dat_ls <- dat_ls[retain_ids]





# Restrict each participant's data to matrix of relevant columns (including NAs
# inserted for aligning observations to ensure equal time intervals)

node_vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")

node_vars_d2 <- paste0(node_vars, "_d2")

dat_mat_ls <- lapply(dat_ls, function(x) { 
  x <- as.matrix(x[, node_vars_d2])
  
  row.names(x) <- 1:nrow(x)
  
  return(x)
})

# ---------------------------------------------------------------------------- #
# TODO: Test GIMME models ----
# ---------------------------------------------------------------------------- #

  # Specifying ar = FALSE and VAR = FALSE (error)

# test    <- indSEM(dat_mat_ls, "./results/test_gimme_none/",    # TODO: Error in `[.data.frame`(x, r, vars, drop = drop) :
#                   ar = FALSE, standardize = TRUE, VAR = FALSE) #         undefined columns selected
# 
# sink(file = "./results/test_gimme_none/test_gimme_none_syntax.txt")
# test$syntax[[1]]
# sink()

  # Specifying ar = TRUE and VAR = FALSE

# test_ar <- indSEM(dat_mat_ls, "./results/test_gimme_ar/",
#                   ar = TRUE, standardize = TRUE, VAR = FALSE)
# 
# sink(file = "./results/test_gimme_ar/test_gimme_ar_syntax.txt")
# test_ar$syntax[[1]]
# sink()

# plot(test_ar$plots[[1]])     # Contemporaneous relations seem to have solid edges

  # Specifying ar = FALSE and VAR = TRUE (error)

# test_var <- indSEM(dat_mat_ls, "./results/test_gimme_var/",    # TODO: Error in `[.data.frame`(x, r, vars, drop = drop) :
#                    ar = FALSE, standardize = TRUE, VAR = TRUE) #         undefined columns selected
# 
# sink(file = "test_gimme_var_syntax.txt")
# test_var$syntax[[1]]
# sink()

  # Specifying ar = TRUE and VAR = TRUE

# test_ar_var <- indSEM(dat_mat_ls, "./results/test_gimme_ar_var/",
#                       ar = TRUE, standardize = TRUE, VAR = TRUE)
# 
# sink(file = "./results/test_gimme_ar_var/test_gimme_ar_var_syntax.txt")
# test_ar_var$syntax[[1]]
# sink()

# plot(test_ar_var$plots[[1]]) # Lagged relations seem to have dashed edges

# ---------------------------------------------------------------------------- #
# Fit GIMME idiographic VAR models ----
# ---------------------------------------------------------------------------- #

# TODO (seem unable to fit saturated networks): Initial idiographic VAR models fit 
# in "ttt-p1-main-analysis" repo using Mplus were based on "_d" variables that were 
# centered by removing linear trend. Fit idiographic VAR models using "gimme" based 
# on "_d2" variables that were centered by removing linear trend and weekend effect.

  # TODO: Resolve warnings
    # When standardize = FALSE, 50 or more warning messages (first 50 like this):
      # In lav_mvnorm_missing_h1_estimate_moments(Y = X[[g]],  ... : lavaan WARNING:
      # Maximum number of iterations reached when computing the sample
      # moments using EM; use the em.h1.iter.max= argument to increase the
      # number of iterations
    # When standardize = TRUE, 13 warnings like that above
  
var_res_ls     <- indSEM(dat_mat_ls, "./results/gimme/raw/",
                         ar = TRUE, VAR = TRUE)

var_res_ls_std <- indSEM(dat_mat_ls, "./results/gimme/raw_std/",
                         ar = TRUE, VAR = TRUE, standardize = TRUE)




# TODO: Figure out if standardize should be TRUE or FALSE (takes longer when
# standardize is FALSE as directly above, and quite different results)





# TODO: Confirm whether "beta.std" and "se" is completely standardized solution





# ---------------------------------------------------------------------------- #
# Save results list ----
# ---------------------------------------------------------------------------- #

saveRDS(var_res_ls,     file = "./results/gimme/raw/var_res_ls.RDS")
saveRDS(var_res_ls_std, file = "./results/gimme/raw_std/var_res_ls_std.RDS")