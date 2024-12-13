# ---------------------------------------------------------------------------- #
# Created Adjacency Matrices for Idiographic VAR Temporal Networks -----
# Authors: Josip Razum, Jeremy W. Eberle
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

groundhog.library("dplyr", groundhog_day)

# ---------------------------------------------------------------------------- #
# Import selected results ----
# ---------------------------------------------------------------------------- #

# Temporal results from idiographic VAR models

extracted_results_path <- "./results/from_ttt-p1-main-analysis/extracted/"

load(paste0(extracted_results_path, "results_var.RDS"))

# ---------------------------------------------------------------------------- #
# Create saturated and thresholded adjacency matrices ----
# ---------------------------------------------------------------------------- #

# Define function to create (saturated or thresholded) directed adjacency matrix of
# autoregressive and cross-lagged coefficients for a given participant (where rows 
# are predictors and columns are criterions)

create_adj_mat <- function(participant_results, mat_type) {
  if (mat_type == "thresholded") {
    est_col <- "est_thres"
  } else if (mat_type == "saturated") {
    est_col <- "est"
  }
  
  variables <- unique(c(participant_results$criterion, participant_results$predictor))
  
  adj_mat <- matrix(NA, nrow = length(variables), ncol = length(variables))
  
  rownames(adj_mat) <- variables
  colnames(adj_mat) <- variables
  
  for (i in 1:nrow(participant_results)) {
    predictor <- participant_results$predictor[i]
    criterion <- participant_results$criterion[i]
    relationship <- participant_results[i, est_col]
    adj_mat[predictor, criterion] <- relationship
  }
  
  return(adj_mat)
}

# Define function to group results by participant and create adjacency matrices

create_adj_mats <- function(results, mat_type) {
  # Note: Use factor version of "lifepak_id" to preserve participant order
  
  results$lifepak_id_fct <- factor(results$lifepak_id, levels = unique(results$lifepak_id))
  
  thres_adj_mats <- results %>%
    split(.$lifepak_id_fct) %>%
    lapply(create_adj_mat, mat_type)
  
  return(thres_adj_mats)
}

# Run function to create saturated and thresholded adjacency matrices

thres_adj_mats_var <- create_adj_mats(results_var, "thresholded")
satur_adj_mats_var <- create_adj_mats(results_var, "saturated")

# ---------------------------------------------------------------------------- #
# Export adjacency matrices ----
# ---------------------------------------------------------------------------- #

adj_mats_path <- "./results/adj_mats/"

dir.create(adj_mats_path)

save(thres_adj_mats_var, file = paste0(adj_mats_path, "thres_adj_mats_var.Rdata"))
save(satur_adj_mats_var, file = paste0(adj_mats_path, "satur_adj_mats_var.Rdata"))