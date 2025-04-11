# ---------------------------------------------------------------------------- #
# Created Adjacency Matrices for GIMME Idiographic VAR Temporal Networks -----
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

# Load packages

pkgs <- c("gimme", "stringr")

groundhog.library(pkgs, groundhog_day)

# ---------------------------------------------------------------------------- #
# Import GIMME results ----
# ---------------------------------------------------------------------------- #

var_res_ls     <- readRDS("./results/gimme/raw/var_res_ls.RDS")
var_res_ls_std <- readRDS("./results/gimme/raw_std/var_res_ls_std.RDS")

# ---------------------------------------------------------------------------- #
# Reformat adjacency matrices ----
# ---------------------------------------------------------------------------- #

# Define function to reformat directed adjacency matrix of standardized autoregressive 
# and cross-lagged coefficients for a given participant so that contemporaneous relations 
# are excluded, that rows are predictors and columns are criterions, and that row and
# column names have no suffixes (to match format of matrices created for Mplus results)

reformat_adj_mats <- function(var_res_ls) {
  path_est_mats <- var_res_ls$path_est_mats
  
  node_vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  node_vars_d2lag <- paste0(node_vars, "_d2lag")
  
  gimme_adj_mats_var <- lapply(path_est_mats, function(x) {
    # Exclude contemporaneous relations
    
    x <- x[, node_vars_d2lag]
    
    # Make rows predictors and columns criterions
    
    x_t <- t(x)
    
    # Remove suffixes from row and column names 
    
    rownames(x_t) <- str_split_i(rownames(x_t), "_", 1)
    colnames(x_t) <- str_split_i(colnames(x_t), "_", 1)
    
    return(x_t)
  })
  
  return(gimme_adj_mats_var)
}

# Run function

gimme_adj_mats_var     <- reformat_adj_mats(var_res_ls)     
gimme_adj_mats_var_std <- reformat_adj_mats(var_res_ls_std) # TODO: Standardized variables in advance,
                                                            # though all adj_mats have standardized coefs

# TODO: Far fewer retained edges when standardizing in advance

gimme_adj_mats_var[[1]]
gimme_adj_mats_var_std[[1]]





# ---------------------------------------------------------------------------- #
# Export adjacency matrices ----
# ---------------------------------------------------------------------------- #

adj_mats_path <- "./results/adj_mats/"

save(gimme_adj_mats_var,     file = paste0(adj_mats_path, "gimme_adj_mats_var.Rdata"))
save(gimme_adj_mats_var_std, file = paste0(adj_mats_path, "gimme_adj_mats_var_std.Rdata"))