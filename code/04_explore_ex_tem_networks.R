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

# Load package and set seed

groundhog.library("qgraph", groundhog_day)

set.seed(1234)

# ---------------------------------------------------------------------------- #
# Import data, selected results, and adjacency matrices ----
# ---------------------------------------------------------------------------- #

# Data

load("./data/from_ttt-p1-main-analysis/final_clean/data_var.RDS")

# Temporal results from idiographic VAR models

extracted_results_path <- "./results/from_ttt-p1-main-analysis/extracted/"

load(paste0(extracted_results_path, "results_var.RDS"))

# Adjacency matrices for temporal results from idiographic VAR models

adj_mats_path <- "./results/adj_mats/"

load(paste0(adj_mats_path, "thres_adj_mats_var.Rdata"))
load(paste0(adj_mats_path, "satur_adj_mats_var.Rdata"))

# TODO: Investigate missing data patterns





# ---------------------------------------------------------------------------- #
# Explore number of significant autoregressive and cross-lagged effects ----
# ---------------------------------------------------------------------------- #

# Most participants have few significant edges

n_sig_edges <- unlist(lapply(thres_adj_mats_var, function(x) sum(x != 0)))

mean(n_sig_edges)   # 3.51
median(n_sig_edges) # 3
sd(n_sig_edges)     # 3.15

table(n_sig_edges)

hist(n_sig_edges,
     breaks = 21, right = FALSE,
     main = "Distribution of Significant Edges",
     xlab = "Number of Significant Edges",
     ylab = "Number of Participants",
     xlim = c(0, 20))

# Order of participants by number of significant edges

sort(n_sig_edges)

# Randomly select example participants with (a) 1 and (b) 3-4 significant edges.
# Example participant with 10 significant edges was manually chosen.

low_sig_edges_ids <- names(n_sig_edges[n_sig_edges == 1])
med_sig_edges_ids <- names(n_sig_edges[n_sig_edges %in% c(3, 4)])

low_sig_edges_ex_id <- sample(low_sig_edges_ids, 1)
med_sig_edges_ex_id <- sample(med_sig_edges_ids, 1)

if (low_sig_edges_ex_id != "999341") {
  stop("low_sig_edges_ex_id should be 999341. Restart R and rerun script.")
}
if (med_sig_edges_ex_id != "861114") {
  stop("med_sig_edges_ex_id should be 861114. Restart R and rerun script.")
}

high_sig_edges_ex_id <- "326177"
med_sig_edges_ex_id  <- "861114"
low_sig_edges_ex_id  <- "999341"

n_sig_edges[med_sig_edges_ex_id] == 3 # "861114" has 3 significant edges

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
# Explore number of observations ----
# ---------------------------------------------------------------------------- #

data_var <- as.data.frame(data_var)

# Identify approximate number of observations

sum(!is.na(data_var[data_var$lifepak_id == high_sig_edges_ex_id, "bad"])) == 98
sum(!is.na(data_var[data_var$lifepak_id == med_sig_edges_ex_id, "bad"])) == 93
sum(!is.na(data_var[data_var$lifepak_id == low_sig_edges_ex_id, "bad"])) == 82

# Identify distribution of approximate number of observations (range is 62-102)

data_var_ls <- split(data_var, data_var$lifepak_id)

hist(unlist(lapply(data_var_ls, function(x) sum(!is.na(x$bad_d)))),
     main = "Distribution of Number of Observations",
     xlab = "Number of Observations",
     ylab = "Number of Participants",
     xlim = c(60, 105))

all(range(unlist(lapply(data_var_ls, function(x) sum(!is.na(x$bad_d))))) == c(62, 102))

# ---------------------------------------------------------------------------- #
# Restrict to example participants ----
# ---------------------------------------------------------------------------- #

# TODO: Temporarily restrict sample until ready to analyze full sample





retain_ids <- c(high_sig_edges_ex_id, med_sig_edges_ex_id, low_sig_edges_ex_id)

data_var_ls <- data_var_ls[retain_ids]

thres_adj_mats_var <- thres_adj_mats_var[retain_ids]
satur_adj_mats_var <- satur_adj_mats_var[retain_ids]

# ---------------------------------------------------------------------------- #
# Explore item distributions ----
# ---------------------------------------------------------------------------- #

# For example participant with high number of significant edges

# Before detrending

par(mfrow = c(4, 2))
hist(data_var_ls[[high_sig_edges_ex_id]]$bad)
hist(data_var_ls[[high_sig_edges_ex_id]]$control)
hist(data_var_ls[[high_sig_edges_ex_id]]$energy)
hist(data_var_ls[[high_sig_edges_ex_id]]$focus)
hist(data_var_ls[[high_sig_edges_ex_id]]$fun)
hist(data_var_ls[[high_sig_edges_ex_id]]$interest)
hist(data_var_ls[[high_sig_edges_ex_id]]$movement)
hist(data_var_ls[[high_sig_edges_ex_id]]$sad)
par(mfrow = c(1, 1))

# After detrending

par(mfrow = c(4, 2))
hist(data_var_ls[[high_sig_edges_ex_id]]$bad_d)
hist(data_var_ls[[high_sig_edges_ex_id]]$control_d)
hist(data_var_ls[[high_sig_edges_ex_id]]$energy_d)
hist(data_var_ls[[high_sig_edges_ex_id]]$focus_d)
hist(data_var_ls[[high_sig_edges_ex_id]]$fun_d)
hist(data_var_ls[[high_sig_edges_ex_id]]$interest_d)
hist(data_var_ls[[high_sig_edges_ex_id]]$movement_d)
hist(data_var_ls[[high_sig_edges_ex_id]]$sad_d)
par(mfrow = c(1, 1))

# Not done for example participants with medium or low numbers of significant edges

# ---------------------------------------------------------------------------- #
# Plot temporal networks ----
# ---------------------------------------------------------------------------- #

# Define function to plot temporal network

plot_network <- function(tem, lifepak_id, thres) {
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
  
  if (thres == "satur") {
    edge_labels <- FALSE
  } else {
    edge_labels <- TRUE
  }
  
    tem_to_plot <- tem
    max_for_plot <- max(abs(tem))
    
    tem_title <- paste0("Temporal (ID ", lifepak_id, ")")

    plots_path <- "./results/network_plots/"
    
    tem_plot_filename  <- paste0(plots_path, lifepak_id, "_tem_plot_", thres)

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
  
  invisible()
}

# Run function for thresholded and saturated networks

dir.create("./results/network_plots/")

lapply(names(thres_adj_mats_var), function(lifepak_id) {
  plot_network(thres_adj_mats_var[[lifepak_id]], lifepak_id, "thres_a05")
})

lapply(names(satur_adj_mats_var), function(lifepak_id) {
  plot_network(satur_adj_mats_var[[lifepak_id]], lifepak_id, "satur")
})

# ---------------------------------------------------------------------------- #
# Define function to compute predicted values ----
# ---------------------------------------------------------------------------- #

# Define function to compute predicted values over desired time points from adjacency 
# matrix and desired starting values

compute_pred <- function(adj_mat, n_timepoints, start_list) {
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
      bad_pred[i]      <- start_list$bad
      control_pred[i]  <- start_list$control
      energy_pred[i]   <- start_list$energy
      focus_pred[i]    <- start_list$focus
      fun_pred[i]      <- start_list$fun
      interest_pred[i] <- start_list$interest
      movement_pred[i] <- start_list$movement
      sad_pred[i]      <- start_list$sad
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

# ---------------------------------------------------------------------------- #
# Compute predicted values starting from participant's detrended values at baseline ----
# ---------------------------------------------------------------------------- #

# Compute number of study time points for each participant

n_study_timepoints <- lapply(data_var_ls, nrow)

# Define starting values for each participant

define_start_bl <- function(part_data) {
  start_list_bl <- list(bad      = part_data[1, "bad_d"],
                        control  = part_data[1, "control_d"],
                        energy   = part_data[1, "energy_d"],
                        focus    = part_data[1, "focus_d"],
                        fun      = part_data[1, "fun_d"],
                        interest = part_data[1, "interest_d"],
                        movement = part_data[1, "movement_d"],
                        sad      = part_data[1, "sad_d"])
}

start_list_bl <- lapply(data_var_ls, define_start_bl)

# Compute predicted values for thresholded and saturated networks (a) over study 
# period and (b) into future 

pred_study_bl_thres_a05 <- lapply(names(thres_adj_mats_var), function(lifepak_id) {
  compute_pred(thres_adj_mats_var[[lifepak_id]], n_study_timepoints[[lifepak_id]], start_list_bl[[lifepak_id]])
})
pred_400_bl_thres_a05   <- lapply(names(thres_adj_mats_var), function(lifepak_id) {
  compute_pred(thres_adj_mats_var[[lifepak_id]], 400,                              start_list_bl[[lifepak_id]])
})

pred_study_bl_satur     <- lapply(names(satur_adj_mats_var), function(lifepak_id) {
  compute_pred(satur_adj_mats_var[[lifepak_id]], n_study_timepoints[[lifepak_id]], start_list_bl[[lifepak_id]])
})
pred_400_bl_satur       <- lapply(names(satur_adj_mats_var), function(lifepak_id) {
  compute_pred(satur_adj_mats_var[[lifepak_id]], 400,                              start_list_bl[[lifepak_id]])
})

names(pred_study_bl_thres_a05) <- names(thres_adj_mats_var)
names(pred_400_bl_thres_a05)   <- names(thres_adj_mats_var)

names(pred_study_bl_satur)     <- names(satur_adj_mats_var)
names(pred_400_bl_satur)       <- names(satur_adj_mats_var)

# ---------------------------------------------------------------------------- #
# Compute predicted values starting from participant's max for one node and 0 for others ----
# ---------------------------------------------------------------------------- #

# Define various starting values for each participant

define_start_max_one_0_others <- function(part_data) {
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  start_list_max_one_0_others_element <- vector("list", length = length(vars))
  names(start_list_max_one_0_others_element) <- vars
  start_list_max_one_0_others_element[ ] <- 0
  
  start_list_max_one_0_others <- vector("list", length = length(vars))
  names(start_list_max_one_0_others) <- paste0("max_", vars, "_d")
  start_list_max_one_0_others[ ] <- list(start_list_max_one_0_others_element)
  
  start_list_max_one_0_others$max_bad_d$bad           <- max(part_data["bad_d"],      na.rm = TRUE)
  start_list_max_one_0_others$max_control_d$control   <- max(part_data["control_d"],  na.rm = TRUE)
  start_list_max_one_0_others$max_energy_d$energy     <- max(part_data["energy_d"],   na.rm = TRUE)
  start_list_max_one_0_others$max_focus_d$focus       <- max(part_data["focus_d"],    na.rm = TRUE)
  start_list_max_one_0_others$max_fun_d$fun           <- max(part_data["fun_d"],      na.rm = TRUE)
  start_list_max_one_0_others$max_interest_d$interest <- max(part_data["interest_d"], na.rm = TRUE)
  start_list_max_one_0_others$max_movement_d$movement <- max(part_data["movement_d"], na.rm = TRUE)
  start_list_max_one_0_others$max_sad_d$sad           <- max(part_data["sad_d"],      na.rm = TRUE)
  
  return(start_list_max_one_0_others)
}

start_list_max_one_0_others <- lapply(data_var_ls, define_start_max_one_0_others)

# Define function to compute predicted values from various starting values

compute_pred_various_start <- function(adj_mats_var, n_timepoints, various_start_lists, various_start_list_names) {
  various_pred_lists <- vector("list", length(various_start_list_names))
  names(various_pred_lists) <- various_start_list_names
  
  for (start_list_name in various_start_list_names) {
    various_pred_lists[[start_list_name]] <- lapply(names(adj_mats_var), function(lifepak_id) {
      if (length(n_timepoints) > 1) {
        compute_pred(adj_mats_var[[lifepak_id]], n_timepoints[[lifepak_id]], various_start_lists[[lifepak_id]][[start_list_name]])
      } else if (length(n_timepoints == 1)) {
        compute_pred(adj_mats_var[[lifepak_id]], n_timepoints              , various_start_lists[[lifepak_id]][[start_list_name]])
      }
    })
    
    names(various_pred_lists[[start_list_name]]) <- names(adj_mats_var)
  }
  
  return(various_pred_lists)
}

# Run function to compute predicted values for thresholded and saturated networks 
# (a) over study period and (b) into future

start_list_max_one_0_others_names <- paste0("max_", c("bad_d", "control_d", "energy_d", "focus_d",
                                                      "fun_d", "interest_d", "movement_d", "sad_d"))

pred_study_max_one_0_others_thres_a05 <- 
  compute_pred_various_start(thres_adj_mats_var, n_study_timepoints, start_list_max_one_0_others, start_list_max_one_0_others_names)
pred_400_max_one_0_others_thres_a05 <- 
  compute_pred_various_start(thres_adj_mats_var, 400,                start_list_max_one_0_others, start_list_max_one_0_others_names)

pred_study_max_one_0_others_satur <- 
  compute_pred_various_start(satur_adj_mats_var, n_study_timepoints, start_list_max_one_0_others, start_list_max_one_0_others_names)
pred_400_max_one_0_others_satur <- 
  compute_pred_various_start(satur_adj_mats_var, 400,                start_list_max_one_0_others, start_list_max_one_0_others_names)

# ---------------------------------------------------------------------------- #
# Plot predicted values ----
# ---------------------------------------------------------------------------- #

# Define function to plot predicted (blue) and observed (black) values

plot_pred_obs <- function(pred_df, obs_df, plot_name, plot_title) {
  obs_df$t <- 1:nrow(obs_df)
  
  plot_title <- plot_title

  pdf(paste0("./results/pred_values/", plot_name, ".pdf"))
  
  par(mfrow = c(2, 2))
  
  xlab <- "Time"
  ylab <- "Detrended Value"
  col_pred <- "green"
  pch <- 16
  ylim <- c(-100, 100)
  
  plot(pred_df$t, pred_df$bad_pred,      main = "Bad Self",
       xlab = xlab, ylab = ylab, col = col_pred, pch = pch, ylim = ylim)
  points(obs_df$t, obs_df$bad_d)
  
  plot(pred_df$t, pred_df$control_pred,  main = "Lack Control",
       xlab = xlab, ylab = ylab, col = col_pred, pch = pch, ylim = ylim)
  points(obs_df$t, obs_df$control_d)
  
  plot(pred_df$t, pred_df$energy_pred,   main = "Fatigue",
       xlab = xlab, ylab = ylab, col = col_pred, pch = pch, ylim = ylim)
  points(obs_df$t, obs_df$energy_d)
  
  plot(pred_df$t, pred_df$focus_pred,    main = "Lack Focus",
       xlab = xlab, ylab = ylab, col = col_pred, pch = pch, ylim = ylim)
  points(obs_df$t, obs_df$focus_d)
  
  mtext(plot_title, side = 3, line = -1, outer = TRUE)
  
  plot(pred_df$t, pred_df$fun_pred,      main = "Inaction",  
       xlab = xlab, ylab = ylab, col = col_pred, pch = pch, ylim = ylim)
  points(obs_df$t, obs_df$fun_d)
  
  plot(pred_df$t, pred_df$interest_pred, main = "Lack Interest",
       xlab = xlab, ylab = ylab, col = col_pred, pch = pch, ylim = ylim)
  points(obs_df$t, obs_df$interest_d)
  
  plot(pred_df$t, pred_df$movement_pred, main = "Slower or Fidgety",
       xlab = xlab, ylab = ylab, col = col_pred, pch = pch, ylim = ylim)
  points(obs_df$t, obs_df$movement_d)
  
  plot(pred_df$t, pred_df$sad_pred,      main = "Sad", 
       xlab = xlab, ylab = ylab, col = col_pred, pch = pch, ylim = ylim)
  points(obs_df$t, obs_df$sad_d)
  
  mtext(plot_title, side = 3, line = -1, outer = TRUE)
  
  par(mfrow = c(1, 1))
  
  dev.off()
}

# (TODO: Revise filenames for better sorting of outputs) Run function





dir.create("./results/pred_values/")

  # For predicted values starting from observed baseline values

lapply(names(pred_study_bl_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_bl_thres_a05[[lifepak_id]], data_var_ls[[lifepak_id]],
                paste0("pred_study_bl_thres_a05_", lifepak_id),
                paste0("Through Study for Thresholded Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})
lapply(names(pred_400_bl_thres_a05),   function(lifepak_id) {
  plot_pred_obs(pred_400_bl_thres_a05[[lifepak_id]],   data_var_ls[[lifepak_id]],
                paste0("pred_400_bl_thres_a05_",   lifepak_id),
                paste0("Through 400 for Thresholded Starting From Obs. Baseline Values (ID ",   lifepak_id, ")"))
})

lapply(names(pred_study_bl_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_bl_satur[[lifepak_id]],     data_var_ls[[lifepak_id]],
                paste0("pred_study_bl_satur_",     lifepak_id),
                paste0("Through Study for Saturated Starting From Obs. Baseline Values (ID ",   lifepak_id, ")"))
})
lapply(names(pred_400_bl_satur),       function(lifepak_id) {
  plot_pred_obs(pred_400_bl_satur[[lifepak_id]],       data_var_ls[[lifepak_id]],
                paste0("pred_400_bl_satur_",       lifepak_id),
                paste0("Through 400 for Saturated Starting From Obs. Baseline Values (ID ",     lifepak_id, ")"))
})

  # Define function to plot predicted values from various starting points

plot_pred_obs_various_start <- function(various_pred_lists, various_pred_lists_focal_var_labels, data_var_ls, 
                                        plot_name_stem, thres, plot_title_stem) {
  for (i in 1:length(various_pred_lists)) {
    pred_list <- various_pred_lists[[i]]
    pred_list_name <- names(various_pred_lists)[i]
    pred_list_focal_var_label <- various_pred_lists_focal_var_labels[i]
    
    pred_list_plot_title_stem <- sub("pred_list_focal_var_label", pred_list_focal_var_label, plot_title_stem)
    
    lapply(names(pred_list), function(lifepak_id) {
      plot_pred_obs(pred_list[[lifepak_id]], data_var_ls[[lifepak_id]],
                    paste0(plot_name_stem, "_", pred_list_name, "_", thres, "_", lifepak_id),
                    paste0(pred_list_plot_title_stem, lifepak_id, ")"))
    })
  }
}

  # Define label of focal variable for each list of starting values (use list names in
  # "pred_study_max_one_0_others_thres_a05" as paradigmatic)

pred_max_one_0_others_focal_var_labels <- names(pred_study_max_one_0_others_thres_a05)

pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_bad_d"]      <- "Bad Self"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_control_d"]  <- "Lack Control"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_energy_d"]   <- "Fatigue"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_focus_d"]    <- "Lack Focus"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_fun_d"]      <- "Inaction"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_interest_d"] <- "Lack Interest"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_movement_d"] <- "Slower or Fidgety"
pred_max_one_0_others_focal_var_labels[pred_max_one_0_others_focal_var_labels == "max_sad_d"]      <- "Sad"

  # Run "plot_pred_obs_various_start()" function

plot_pred_obs_various_start(pred_study_max_one_0_others_thres_a05, pred_max_one_0_others_focal_var_labels, data_var_ls,
                            "pred_study_max_one_0_others", "thres_a05",
                            'Through Study for Thres. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')
plot_pred_obs_various_start(pred_400_max_one_0_others_thres_a05,   pred_max_one_0_others_focal_var_labels, data_var_ls,
                            "pred_400_max_one_0_others",   "thres_a05",
                            'Through 400 for Thres. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')

plot_pred_obs_various_start(pred_study_max_one_0_others_satur,     pred_max_one_0_others_focal_var_labels, data_var_ls,
                            "pred_study_max_one_0_others", "satur",
                            'Through Study for Satur. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')
plot_pred_obs_various_start(pred_400_max_one_0_others_satur,       pred_max_one_0_others_focal_var_labels, data_var_ls,
                            "pred_400_max_one_0_others",   "satur",
                            'Through 400 for Satur. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')

# ---------------------------------------------------------------------------- #
# TODO: Experiment with GLLA ----
# ---------------------------------------------------------------------------- #

groundhog.library("EGAnet", groundhog_day)

tseries <- 49:56
deriv.tseries <- glla(tseries, n.embed = 4, tau = 1, delta = 1, order = 2)

plot(deriv.tseries[, "Obs"], deriv.tseries[, "DerivOrd1"])





