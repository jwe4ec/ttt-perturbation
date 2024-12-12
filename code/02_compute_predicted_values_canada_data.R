# ---------------------------------------------------------------------------- #
# Compute Predicted Values in Canada Example Dataset -----
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

# Load package

groundhog.library("vars", groundhog_day)

# ---------------------------------------------------------------------------- #
# Import Canada example dataset ----
# ---------------------------------------------------------------------------- #

data(Canada)

# ---------------------------------------------------------------------------- #
# Compute predicted values ----
# ---------------------------------------------------------------------------- #

# Fit VAR and extract temporal results

fit <- VAR(Canada, p = 1, type = "none")

coefs_e    <- fit$varresult$e$coefficients
coefs_prod <- fit$varresult$prod$coefficients
coefs_rw   <- fit$varresult$rw$coefficients
coefs_U    <- fit$varresult$U$coefficients

vars    <- c("e", "prod", "rw", "U")
vars.l1 <- paste0(vars, ".l1")

predictor_vec <- rep(vars.l1, 4)
criterion_vec <- rep(vars, each = 4)
relation_vec  <- c(coefs_e, coefs_prod, coefs_rw, coefs_U)

tem_res <- data.frame(predictor = predictor_vec,
                      criterion = criterion_vec,
                      relation  = relation_vec)

# Create (unthresholded) adjacency matrix (where rows are predictors and columns 
# are criterions)

adj_mat <- matrix(NA, nrow = length(vars), ncol = length(vars))

rownames(adj_mat) <- vars.l1
colnames(adj_mat) <- vars

for (i in 1:nrow(tem_res)) {
  predictor <- tem_res$predictor[i]
  criterion <- tem_res$criterion[i]
  relation  <- tem_res$relation[i]
  
  adj_mat[predictor, criterion] <- relation
}

# Define function to compute predicted values over desired time points from adjacency 
# matrix and desired starting values

compute_pred_canada <- function(adj_mat, n_timepoints, start_e, start_prod, start_rw, start_U) {
  e_pred    <- rep(NA, n_timepoints)
  prod_pred <- rep(NA, n_timepoints)
  rw_pred   <- rep(NA, n_timepoints)
  U_pred    <- rep(NA, n_timepoints)
  
  for (i in 1:n_timepoints) {
    if (i == 1) {
      e_pred[i]    <- start_e
      prod_pred[i] <- start_prod
      rw_pred[i]   <- start_rw
      U_pred[i]    <- start_U
    } else if (i > 1) {
      e_l1    <- e_pred[i - 1]
      prod_l1 <- prod_pred[i - 1]
      rw_l1   <- rw_pred[i - 1]
      U_l1    <- U_pred[i - 1]
      
      e_pred[i]    <- e_l1*adj_mat["e.l1", "e"]    + prod_l1*adj_mat["prod.l1", "e"]    + rw_l1*adj_mat["rw.l1", "e"]    + U_l1*adj_mat["U.l1", "e"]
      prod_pred[i] <- e_l1*adj_mat["e.l1", "prod"] + prod_l1*adj_mat["prod.l1", "prod"] + rw_l1*adj_mat["rw.l1", "prod"] + U_l1*adj_mat["U.l1", "prod"]
      rw_pred[i]   <- e_l1*adj_mat["e.l1", "rw"]   + prod_l1*adj_mat["prod.l1", "rw"]   + rw_l1*adj_mat["rw.l1", "rw"]   + U_l1*adj_mat["U.l1", "rw"]
      U_pred[i]    <- e_l1*adj_mat["e.l1", "U"]    + prod_l1*adj_mat["prod.l1", "U"]    + rw_l1*adj_mat["rw.l1", "U"]    + U_l1*adj_mat["U.l1", "U"]
    }
  }
  
  pred <- data.frame(t         = 1:n_timepoints,
                     e_pred    = e_pred,
                     prod_pred = prod_pred,
                     rw_pred   = rw_pred,
                     U_pred    = U_pred)
  
  return(pred)
}

# Compute predicted values over study period starting from baseline

n_study_timepoints <- nrow(Canada)

start_bl_e    <- Canada[1, "e"]
start_bl_prod <- Canada[1, "prod"]
start_bl_rw   <- Canada[1, "rw"]
start_bl_U    <- Canada[1, "U"]

canada_pred_study_bl <- compute_pred_canada(adj_mat, n_study_timepoints, start_bl_e, start_bl_prod, start_bl_rw, start_bl_U)

# TODO (How? Will always get 0): Compute predicted values over study period starting from 0

canada_pred_study_0   <- compute_pred_canada(adj_mat, n_study_timepoints, 0, 0, 0, 0)

canada_pred_study_001 <- compute_pred_canada(adj_mat, n_study_timepoints, .001, .001, .001, .001)





# Compute predicted values into future starting from baseline

canada_pred_200_bl   <- compute_pred_canada(adj_mat, 200,    start_bl_e, start_bl_prod, start_bl_rw, start_bl_U)
canada_pred_2000_bl  <- compute_pred_canada(adj_mat, 2000,   start_bl_e, start_bl_prod, start_bl_rw, start_bl_U)
canada_pred_20000_bl <- compute_pred_canada(adj_mat, 20000,  start_bl_e, start_bl_prod, start_bl_rw, start_bl_U)





# ---------------------------------------------------------------------------- #
# Plot predicted values ----
# ---------------------------------------------------------------------------- #

# Define function to plot predicted (blue) and observed (black) values

plot_pred_obs_canada <- function(canada_pred) {
  canada_df <- as.data.frame(Canada)
  
  canada_df$t <- 1:nrow(canada_df)
  
  par(mfrow = c(2, 2))
  
  col_pred <- "blue"
  
  plot(canada_pred$t, canada_pred$e_pred,    col = col_pred)
  points(canada_df$t, canada_df$e)
  
  plot(canada_pred$t, canada_pred$prod_pred, col = col_pred)
  points(canada_df$t, canada_df$prod)
  
  plot(canada_pred$t, canada_pred$rw_pred,   col = col_pred)
  points(canada_df$t, canada_df$rw)
  
  plot(canada_pred$t, canada_pred$U_pred,    col = col_pred)
  points(canada_df$t, canada_df$U)
  
  par(mfrow = c(1, 1))
}

# Run function

plot_pred_obs_canada(canada_pred_study_bl)

plot_pred_obs_canada(canada_pred_study_0)  # TODO: Run once values computed (if possible)
plot_pred_obs_canada(canada_pred_study_001)

plot_pred_obs_canada(canada_pred_200_bl)
plot_pred_obs_canada(canada_pred_2000_bl)
plot_pred_obs_canada(canada_pred_20000_bl)