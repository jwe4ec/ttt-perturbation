# ---------------------------------------------------------------------------- #
# Explore Example Temporal Networks From Actual Data -----
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

groundhog.library("qgraph", groundhog_day)

# ---------------------------------------------------------------------------- #
# Import data and selected results ----
# ---------------------------------------------------------------------------- #

# Data

load("./data/from_ttt-p1-main-analysis/final_clean/data_var.RDS")

# Temporal results from idiographic VAR models

extracted_results_path <- "./results/from_ttt-p1-main-analysis/extracted/"

load(paste0(extracted_results_path, "results_var.RDS"))
load(paste0(extracted_results_path, "thres_adj_mats_var.RDS"))

# ---------------------------------------------------------------------------- #
# Explore number of significant autoregressive and cross-lagged effects ----
# ---------------------------------------------------------------------------- #

# Most participants have few significant edges

table(unlist(lapply(thres_adj_mats_var, function(x) sum(x != 0))))

hist(unlist(lapply(thres_adj_mats_var, function(x) sum(x != 0))),
     breaks = 21, right = FALSE,
     main = "Distribution of Significant Edges",
     xlab = "Number of Significant Edges",
     ylab = "Number of Participants",
     xlim = c(0, 20))

# Order of participants by number of significant edges

sort(unlist(lapply(thres_adj_mats_var, function(x) sum(x != 0))))

# TODO: Sample across number of sig. edges





# Determine number of participants for which each edge is significant

thres_adj_mats_var_sig_edges <- lapply(thres_adj_mats_var, function(x) (x != 0))

thres_adj_mats_var_sig_freq <- Reduce("+", thres_adj_mats_var_sig_edges)

range(thres_adj_mats_var_sig_freq) == c(0, 8)

# Determine percentage of participants for which each edge is significant

n <- length(thres_adj_mats_var)

thres_adj_mats_var_sig_incl_perc <- round((thres_adj_mats_var_sig_freq / n) * 100, 1)

range(thres_adj_mats_var_sig_incl_perc) == c(0.0, 15.1)

# Show number (and percentage) of participants for which each edge is significant

thres_adj_mats_var_sig_freq_incl_perc <- thres_adj_mats_var_sig_freq
thres_adj_mats_var_sig_freq_incl_perc[, ] <- NA

for (i in 1:nrow(thres_adj_mats_var_sig_freq_incl_perc)) {
  for (j in 1:ncol(thres_adj_mats_var_sig_freq_incl_perc)) {
    thres_adj_mats_var_sig_freq_incl_perc[i, j] <- paste0(thres_adj_mats_var_sig_freq[i, j],
                                                          " (",
                                                          format(thres_adj_mats_var_sig_incl_perc[i, j], 
                                                                 nsmall = 1, trim = TRUE),
                                                          ")")
  }
}

thres_adj_mats_var_sig_freq_incl_perc

# Determine range of significant edge weights across participants

thres_adj_mats_var_ever_sig <- thres_adj_mats_var_sig_freq > 0

thres_adj_mats_var_sig_range <- thres_adj_mats_var_ever_sig
thres_adj_mats_var_sig_range[, ] <- NA

for (i in 1:nrow(thres_adj_mats_var_sig_range)) {
  for (j in 1:ncol(thres_adj_mats_var_sig_range)) {
    if (thres_adj_mats_var_ever_sig[i, j] == TRUE) {
      # Obtain significant edge weights
      
      element_values <- sapply(thres_adj_mats_var, function(x) x[i, j])
      
      element_values <- element_values[element_values != 0]
      
      # Compute range of significant edge weights
      
      range <- format(round(range(element_values), 2),
                      nsmall = 2, trim = TRUE)
      
      thres_adj_mats_var_sig_range[i, j] <- paste(range, collapse = ", ")
    }
  }
}

thres_adj_mats_var_sig_range

# Show number of participants for which each edge is significant along with
# range of significant edges

thres_adj_mats_var_sig_freq_range <- thres_adj_mats_var_sig_freq
thres_adj_mats_var_sig_freq_range[, ] <- NA

for (i in 1:nrow(thres_adj_mats_var_sig_freq_range)) {
  for (j in 1:ncol(thres_adj_mats_var_sig_freq_range)) {
    thres_adj_mats_var_sig_freq_range[i, j] <- paste0(thres_adj_mats_var_sig_freq[i, j],
                                                      " (",
                                                      thres_adj_mats_var_sig_range[i, j], 
                                                      ")")
  }
}

thres_adj_mats_var_sig_freq_range

# ---------------------------------------------------------------------------- #
# Plot temporal networks ----
# ---------------------------------------------------------------------------- #

# Define function to plot temporal network

plot_network <- function(tem, thres) {
  # Get temporal adjacency matrix object name
  
  model_name <- deparse(substitute(tem))
  
  # Get node variables and rename as labels
  
  labels <- row.names(tem)
  
  labels[labels == "bad"]      <- "Bad\nSelf"
  labels[labels == "control"]  <- "Lack\nControl"
  labels[labels == "energy"]   <- "Fatigue"
  labels[labels == "focus"]    <- "Lack\nFocus"
  labels[labels == "fun"]      <- "Inaction"
  labels[labels == "interest"] <- "Lack\nInterest"
  labels[labels == "movement"] <- "Slower\nor \nFidgety"
  labels[labels == "sad"]      <- "Sad"
  
  # Define plotting options
  
    # Include edge labels only for thresholded networks (too cluttered in saturated networks)
  
  if (thres == "full") {
    edge_labels <- FALSE
  } else {
    edge_labels <- TRUE
  }
  
    tem_to_plot <- tem
    max_for_plot <- max(abs(tem))
    
    tem_title <- "Temporal"

    plots_path <- "./results/from_ttt-p1-main-analysis/var/plots/"
    
    tem_plot_filename  <- paste0(plots_path, model_name, "_tem_plot_", thres)

  # Plot circle graphs
  
  tem_plot <- qgraph(tem_to_plot,
                     edge.labels = edge_labels,
                     edge.label.color = "black",
                     edge.label.margin = .01,
                     edge.label.cex = .9,
                     layout = "circle", 
                     labels = labels, 
                     theme = "colorblind",
                     asize = 7, 
                     vsize = 8, 
                     label.cex = c(rep(.8, 6), .7), 
                     mar = rep(6, 4), 
                     title = tem_title,
                     label.scale = FALSE,
                     maximum = max_for_plot,
                     esize = 10)
  
  # Export plot
  
  dir.create(plots_path, recursive = TRUE, showWarnings = FALSE)
  
  qgraph(tem_plot,
         filetype = "pdf",
         filename = tem_plot_filename)
}

# Run function

  # For participant with greatest number of significant edges (18)

adj_mat_516921 <- thres_adj_mats_var[["516921"]]

plot_network(adj_mat_516921, "thres_a05")

  # For participant with next greatest number of significant edges (10)

adj_mat_326177 <- thres_adj_mats_var[["326177"]]

plot_network(adj_mat_326177, "thres_a05")

# ---------------------------------------------------------------------------- #
# Explore number of observations ----
# ---------------------------------------------------------------------------- #

# View data for selected participants above

# View(data_var[data_var$lifepak_id == "516921", ])
# View(data_var[data_var$lifepak_id == "326177", ])

# Identify approximate number of observations

sum(!is.na(data_var[data_var$lifepak_id == "516921", "bad"])) # ~91 observations
sum(!is.na(data_var[data_var$lifepak_id == "326177", "bad"])) # ~98 observations

# Identify distribution of approximate number of observations (range is 62-102)

data_var_ls <- split(data_var, data_var$lifepak_id)

hist(unlist(lapply(data_var_ls, function(x) sum(!is.na(x$bad_d)))),
     main = "Distribution of Number of Observations",
     xlab = "Number of Observations",
     ylab = "Number of Participants",
     xlim = c(60, 105))

all(range(unlist(lapply(data_var_ls, function(x) sum(!is.na(x$bad_d))))) == c(62, 102))

# ---------------------------------------------------------------------------- #
# Explore item distributions ----
# ---------------------------------------------------------------------------- #

# For "516921"

data_516921 <- as.data.frame(data_var_ls[["516921"]])

  # Before detrending

par(mfrow = c(4, 2))
hist(data_516921$bad)
hist(data_516921$control)
hist(data_516921$energy)
hist(data_516921$focus)
hist(data_516921$fun)
hist(data_516921$interest)
hist(data_516921$movement)
hist(data_516921$sad)
par(mfrow = c(1, 1))

  # After detrending

par(mfrow = c(4, 2))
hist(data_516921$bad_d)
hist(data_516921$control_d)
hist(data_516921$energy_d)
hist(data_516921$focus_d)
hist(data_516921$fun_d)
hist(data_516921$interest_d)
hist(data_516921$movement_d)
hist(data_516921$sad_d)
par(mfrow = c(1, 1))

# For "326177"

data_326177 <- as.data.frame(data_var_ls[["326177"]])

  # Before detrending

par(mfrow = c(4, 2))
hist(data_326177$bad)
hist(data_326177$control)
hist(data_326177$energy)
hist(data_326177$focus)
hist(data_326177$fun)
hist(data_326177$interest)
hist(data_326177$movement)
hist(data_326177$sad)
par(mfrow = c(1, 1))

  # After detrending

par(mfrow = c(4, 2))
hist(data_326177$bad_d)
hist(data_326177$control_d)
hist(data_326177$energy_d)
hist(data_326177$focus_d)
hist(data_326177$fun_d)
hist(data_326177$interest_d)
hist(data_326177$movement_d)
hist(data_326177$sad_d)
par(mfrow = c(1, 1))

# ---------------------------------------------------------------------------- #
# Plot node values over time ----
# ---------------------------------------------------------------------------- #

# For "516921"

  # Before detrending

plot(data_516921$bin_no_adj, data_516921$bad)
plot(data_516921$bin_no_adj, data_516921$control)
plot(data_516921$bin_no_adj, data_516921$energy)
plot(data_516921$bin_no_adj, data_516921$focus)
plot(data_516921$bin_no_adj, data_516921$fun)
plot(data_516921$bin_no_adj, data_516921$interest)
plot(data_516921$bin_no_adj, data_516921$movement)
plot(data_516921$bin_no_adj, data_516921$sad)

  # After detrending

plot(data_516921$bin_no_adj, data_516921$bad_d)
plot(data_516921$bin_no_adj, data_516921$control_d)
plot(data_516921$bin_no_adj, data_516921$energy_d)
plot(data_516921$bin_no_adj, data_516921$focus_d)
plot(data_516921$bin_no_adj, data_516921$fun_d)
plot(data_516921$bin_no_adj, data_516921$interest_d)
plot(data_516921$bin_no_adj, data_516921$movement_d)
plot(data_516921$bin_no_adj, data_516921$sad_d)

# For "326177"

  # Before detrending

plot(data_326177$bin_no_adj, data_326177$bad)
plot(data_326177$bin_no_adj, data_326177$control)
plot(data_326177$bin_no_adj, data_326177$energy)
plot(data_326177$bin_no_adj, data_326177$focus)
plot(data_326177$bin_no_adj, data_326177$fun)
plot(data_326177$bin_no_adj, data_326177$interest)
plot(data_326177$bin_no_adj, data_326177$movement)
plot(data_326177$bin_no_adj, data_326177$sad)

  # After detrending

plot(data_326177$bin_no_adj, data_326177$bad_d)
plot(data_326177$bin_no_adj, data_326177$control_d)
plot(data_326177$bin_no_adj, data_326177$energy_d)
plot(data_326177$bin_no_adj, data_326177$focus_d)
plot(data_326177$bin_no_adj, data_326177$fun_d)
plot(data_326177$bin_no_adj, data_326177$interest_d)
plot(data_326177$bin_no_adj, data_326177$movement_d)
plot(data_326177$bin_no_adj, data_326177$sad_d)

# ---------------------------------------------------------------------------- #
# Compute predicted values ----
# ---------------------------------------------------------------------------- #

# Define function to compute predicted values over desired time points from adjacency 
# matrix and desired starting values

compute_pred <- function(adj_mat, n_timepoints,
                         start_bad, start_control, start_energy, start_focus,
                         start_fun, start_interest, start_movement, start_sad) {
  init_vec      <- rep(NA, n_timepoints)
  
  bad_pred      <- init_vec
  control_pred  <- init_vec
  energy_pred   <- init_vec
  focus_pred    <- init_vec
  fun_pred      <- init_vec
  interest_pred <- init_vec
  movement_pred <- init_vec
  sad_pred      <- init_vec
  
  for (i in 1:n_timepoints) {
    if (i == 1) {
      bad_pred[i]      <- start_bad
      control_pred[i]  <- start_control
      energy_pred[i]   <- start_energy
      focus_pred[i]    <- start_focus
      fun_pred[i]      <- start_fun
      interest_pred[i] <- start_interest
      movement_pred[i] <- start_movement
      sad_pred[i]      <- start_sad
    } else if (i > 1) {
      bad_l1      <- bad_pred[i - 1]
      control_l1  <- control_pred[i - 1]
      energy_l1   <- energy_pred[i - 1]
      focus_l1    <- focus_pred[i - 1]
      fun_l1      <- fun_pred[i - 1]
      interest_l1 <- interest_pred[i - 1]
      movement_l1 <- movement_pred[i - 1]
      sad_l1      <- sad_pred[i - 1]
      
      bad_pred[i]      <- bad_l1*adj_mat["bad", "bad"]      + control_l1*adj_mat["control", "bad"]      + energy_l1*adj_mat["energy", "bad"]      + focus_l1*adj_mat["focus", "bad"]      + fun_l1*adj_mat["fun", "bad"]      + interest_l1*adj_mat["interest", "bad"]      + movement_l1*adj_mat["movement", "bad"]      + sad_l1*adj_mat["sad", "bad"]
      control_pred[i]  <- bad_l1*adj_mat["bad", "control"]  + control_l1*adj_mat["control", "control"]  + energy_l1*adj_mat["energy", "control"]  + focus_l1*adj_mat["focus", "control"]  + fun_l1*adj_mat["fun", "control"]  + interest_l1*adj_mat["interest", "control"]  + movement_l1*adj_mat["movement", "control"]  + sad_l1*adj_mat["sad", "control"]
      energy_pred[i]   <- bad_l1*adj_mat["bad", "energy"]   + control_l1*adj_mat["control", "energy"]   + energy_l1*adj_mat["energy", "energy"]   + focus_l1*adj_mat["focus", "energy"]   + fun_l1*adj_mat["fun", "energy"]   + interest_l1*adj_mat["interest", "energy"]   + movement_l1*adj_mat["movement", "energy"]   + sad_l1*adj_mat["sad", "energy"]
      focus_pred[i]    <- bad_l1*adj_mat["bad", "focus"]    + control_l1*adj_mat["control", "focus"]    + energy_l1*adj_mat["energy", "focus"]    + focus_l1*adj_mat["focus", "focus"]    + fun_l1*adj_mat["fun", "focus"]    + interest_l1*adj_mat["interest", "focus"]    + movement_l1*adj_mat["movement", "focus"]    + sad_l1*adj_mat["sad", "focus"]
      fun_pred[i]      <- bad_l1*adj_mat["bad", "fun"]      + control_l1*adj_mat["control", "fun"]      + energy_l1*adj_mat["energy", "fun"]      + focus_l1*adj_mat["focus", "fun"]      + fun_l1*adj_mat["fun", "fun"]      + interest_l1*adj_mat["interest", "fun"]      + movement_l1*adj_mat["movement", "fun"]      + sad_l1*adj_mat["sad", "fun"]
      interest_pred[i] <- bad_l1*adj_mat["bad", "interest"] + control_l1*adj_mat["control", "interest"] + energy_l1*adj_mat["energy", "interest"] + focus_l1*adj_mat["focus", "interest"] + fun_l1*adj_mat["fun", "interest"] + interest_l1*adj_mat["interest", "interest"] + movement_l1*adj_mat["movement", "interest"] + sad_l1*adj_mat["sad", "interest"]
      movement_pred[i] <- bad_l1*adj_mat["bad", "movement"] + control_l1*adj_mat["control", "movement"] + energy_l1*adj_mat["energy", "movement"] + focus_l1*adj_mat["focus", "movement"] + fun_l1*adj_mat["fun", "movement"] + interest_l1*adj_mat["interest", "movement"] + movement_l1*adj_mat["movement", "movement"] + sad_l1*adj_mat["sad", "movement"]
      sad_pred[i]      <- bad_l1*adj_mat["bad", "sad"]      + control_l1*adj_mat["control", "sad"]      + energy_l1*adj_mat["energy", "sad"]      + focus_l1*adj_mat["focus", "sad"]      + fun_l1*adj_mat["fun", "sad"]      + interest_l1*adj_mat["interest", "sad"]      + movement_l1*adj_mat["movement", "sad"]      + sad_l1*adj_mat["sad", "sad"]
    }
  }
  
  pred <- data.frame(t             = 1:n_timepoints,
                     bad_pred      = bad_pred,
                     control_pred  = control_pred,
                     energy_pred   = energy_pred,
                     focus_pred    = focus_pred,
                     fun_pred      = fun_pred,
                     interest_pred = interest_pred,
                     movement_pred = movement_pred,
                     sad_pred      = sad_pred)
  
  return(pred)
}

# Compute predicted values for "326177" over study period starting from detrended
# values at baseline

n_study_timepoints <- nrow(data_326177)

start_bl_bad_d      <- data_326177[1, "bad_d"]
start_bl_control_d  <- data_326177[1, "control_d"]
start_bl_energy_d   <- data_326177[1, "energy_d"]
start_bl_focus_d    <- data_326177[1, "focus_d"]
start_bl_fun_d      <- data_326177[1, "fun_d"]
start_bl_interest_d <- data_326177[1, "interest_d"]
start_bl_movement_d <- data_326177[1, "movement_d"]
start_bl_sad_d      <- data_326177[1, "sad_d"]

pred_326177_study_bl <- compute_pred(adj_mat_326177, n_study_timepoints, 
                                     start_bl_bad_d, start_bl_control_d, start_bl_energy_d, start_bl_focus_d, 
                                     start_bl_fun_d, start_bl_interest_d, start_bl_movement_d, start_bl_sad_d)

# Compute predicted values for "326177" into future starting from baseline

pred_326177_400_bl   <- compute_pred(adj_mat_326177, 400,
                                     start_bl_bad_d, start_bl_control_d, start_bl_energy_d, start_bl_focus_d,
                                     start_bl_fun_d, start_bl_interest_d, start_bl_movement_d, start_bl_sad_d)
pred_326177_4000_bl  <- compute_pred(adj_mat_326177, 4000,
                                     start_bl_bad_d, start_bl_control_d, start_bl_energy_d, start_bl_focus_d,
                                     start_bl_fun_d, start_bl_interest_d, start_bl_movement_d, start_bl_sad_d)
pred_326177_40000_bl <- compute_pred(adj_mat_326177, 40000,
                                     start_bl_bad_d, start_bl_control_d, start_bl_energy_d, start_bl_focus_d,
                                     start_bl_fun_d, start_bl_interest_d, start_bl_movement_d, start_bl_sad_d)

# Compute predicted values for "326177" starting from 0 for all nodes except one

start_max_bad_d    <- max(data_326177["bad"], na.rm = TRUE) # max = 77
start_0_control_d  <- 0
start_0_energy_d   <- 0
start_0_focus_d    <- 0
start_0_fun_d      <- 0
start_0_interest_d <- 0
start_0_movement_d <- 0
start_0_sad_d      <- 0

pred_326177_study_max_bad_d <- compute_pred(adj_mat_326177, n_study_timepoints, 
                                            start_max_bad_d, start_0_control_d, start_0_energy_d, start_0_focus_d, 
                                            start_0_fun_d, start_0_interest_d, start_0_movement_d, start_0_sad_d)

pred_326177_400_max_bad_d   <- compute_pred(adj_mat_326177, 400, 
                                            start_max_bad_d, start_0_control_d, start_0_energy_d, start_0_focus_d, 
                                            start_0_fun_d, start_0_interest_d, start_0_movement_d, start_0_sad_d)

# TODO: Continue for other variables (but consider better code)





# ---------------------------------------------------------------------------- #
# Plot predicted values ----
# ---------------------------------------------------------------------------- #

# Define function to plot predicted (blue) and observed (black) values

plot_pred_obs <- function(pred_df, obs_df, plot_title) {
  obs_df$t <- 1:nrow(obs_df)
  
  plot_name <- deparse(substitute(pred_df))
  plot_title <- plot_title

  pdf(paste0("./results/pred_values/", plot_name, ".pdf"))
  
  par(mfrow = c(2, 2))
  
  col_pred <- "green"
  
  plot(pred_df$t, pred_df$bad_pred,      main = "Bad Self",
       xlab = "Time", ylab = "Detrended Value", col = col_pred, pch = 16, ylim = c(-100, 100))
  points(obs_df$t, obs_df$bad_d)
  
  plot(pred_df$t, pred_df$control_pred,  main = "Lack Control",
       xlab = "Time", ylab = "Detrended Value", col = col_pred, pch = 16, ylim = c(-100, 100))
  points(obs_df$t, obs_df$control_d)
  
  plot(pred_df$t, pred_df$energy_pred,   main = "Fatigue",
       xlab = "Time", ylab = "Detrended Value", col = col_pred, pch = 16, ylim = c(-100, 100))
  points(obs_df$t, obs_df$energy_d)
  
  plot(pred_df$t, pred_df$focus_pred,    main = "Lack Focus",
       xlab = "Time", ylab = "Detrended Value", col = col_pred, pch = 16, ylim = c(-100, 100))
  points(obs_df$t, obs_df$focus_d)
  
  mtext(plot_title, side = 3, line = -1, outer = TRUE)
  
  plot(pred_df$t, pred_df$fun_pred,      main = "Inaction",  
       xlab = "Time", ylab = "Detrended Value", col = col_pred, pch = 16, ylim = c(-100, 100))
  points(obs_df$t, obs_df$fun_d)
  
  plot(pred_df$t, pred_df$interest_pred, main = "Lack Interest",
       xlab = "Time", ylab = "Detrended Value", col = col_pred, pch = 16, ylim = c(-100, 100))
  points(obs_df$t, obs_df$interest_d)
  
  plot(pred_df$t, pred_df$movement_pred, main = "Slower or Fidgety",
       xlab = "Time", ylab = "Detrended Value", col = col_pred, pch = 16, ylim = c(-100, 100))
  points(obs_df$t, obs_df$movement_d)
  
  plot(pred_df$t, pred_df$sad_pred,      main = "Sad", 
       xlab = "Time", ylab = "Detrended Value", col = col_pred, pch = 16, ylim = c(-100, 100))
  points(obs_df$t, obs_df$sad_d)
  
  mtext(plot_title, side = 3, line = -1, outer = TRUE)
  
  par(mfrow = c(1, 1))
  
  dev.off()
}

# Run function

dir.create("./results/pred_values/")

plot_pred_obs(pred_326177_study_bl, data_326177, "Through Study Period Starting From Obs. Baseline Values (ID 326177)")

plot_pred_obs(pred_326177_400_bl, data_326177,  "Through 400 Starting From Obs. Baseline Values (ID 326177)")
plot_pred_obs(pred_326177_4000_bl, data_326177, "Through 4000 Starting From Obs. Baseline Values (ID 326177)")

plot_pred_obs(pred_326177_study_max_bad_d, data_326177, 'Through Study Period Starting From Max "Bad Self" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_400_max_bad_d, data_326177,   'Through 400 Starting From Max "Bad Self" and 0 Otherwise (ID 326177)')





# ---------------------------------------------------------------------------- #
# TODO: Experiment with GLLA ----
# ---------------------------------------------------------------------------- #

groundhog.library("EGAnet", groundhog_day)

tseries <- 49:56
deriv.tseries <- glla(tseries, n.embed = 4, tau = 1, delta = 1, order = 2)

plot(deriv.tseries[, "Obs"], deriv.tseries[, "DerivOrd1"])





