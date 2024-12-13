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

# No packages loaded

# ---------------------------------------------------------------------------- #
# Import selected results ----
# ---------------------------------------------------------------------------- #

# Temporal results from idiographic VAR models

extracted_results_path <- "./results/from_ttt-p1-main-analysis/extracted/"

load(paste0(extracted_results_path, "results_var.RDS"))

# TODO: Change README to reflect that "thres_adj_mats_var.RDS" is not used from ttt-p1-main-analysis





# ---------------------------------------------------------------------------- #
# Create saturated and thresholded adjacency matrices ----
# ---------------------------------------------------------------------------- #

# TODO: Define function to create directed adjacency matrix of saturated and thresholded 
# autoregressive and cross-lagged coefficients for a given participant (where rows 
# are predictors and columns are criterions)





create_thres_adj_mat <- function(participant_results) {
  variables <- unique(c(participant_results$criterion, participant_results$predictor))
  
  adj_mat <- matrix(NA, nrow = length(variables), ncol = length(variables))
  
  rownames(adj_mat) <- variables
  colnames(adj_mat) <- variables
  
  for (i in 1:nrow(participant_results)) {
    predictor <- participant_results$predictor[i]
    criterion <- participant_results$criterion[i]
    relationship <- participant_results$est_thres[i]
    adj_mat[predictor, criterion] <- relationship
  }
  
  return(adj_mat)
}

# Define function to group results by participant and create adjacency matrices

create_thres_adj_mats <- function(results) {
  # Note: Use factor version of "lifepak_id" to preserve participant order
  
  results$lifepak_id_fct <- factor(results$lifepak_id, levels = unique(results$lifepak_id))
  
  thres_adj_mats <- results %>%
    split(.$lifepak_id_fct) %>%
    lapply(create_thres_adj_mat)
  
  return(thres_adj_mats)
}

# TODO: Run function to create saturated and thresholded adjacency matrices

thres_adj_mats_var           <- create_thres_adj_mats(results_var)





# ---------------------------------------------------------------------------- #
# Export adjacency matrices ----
# ---------------------------------------------------------------------------- #

# TODO




