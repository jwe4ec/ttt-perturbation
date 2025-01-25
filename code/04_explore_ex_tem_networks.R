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

# Load packages and set seed

pkgs <- c("qgraph", "randomcoloR")
groundhog.library(pkgs, groundhog_day)

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

# Identify distribution of approximate number of observations (range: 62-102, median: 86)

data_var_ls <- split(data_var, data_var$lifepak_id)

hist(unlist(lapply(data_var_ls, function(x) sum(!is.na(x$bad_d)))),
     main = "Distribution of Number of Observations",
     xlab = "Number of Observations",
     ylab = "Number of Participants",
     xlim = c(60, 105))

all(range(unlist(lapply(data_var_ls, function(x) sum(!is.na(x$bad_d)))))      == c(62, 102))
median(unlist(lapply(data_var_ls,    function(x) sum(!is.na(x$bad_d)))))      == 86

# ---------------------------------------------------------------------------- #
# Investigate missing data patterns ----
# ---------------------------------------------------------------------------- #

# Plot observations over time for participants in first quartile of number of observations (74)

q1 <- quantile(unlist(lapply(data_var_ls,  function(x) sum(!is.na(x$bad_d)))), .25)
q1 == 74

q1_ids <- names(data_var_ls)[unlist(lapply(data_var_ls, function(x) sum(!is.na(x$bad_d)))) < q1]

# TODO: Adjust to presence/absence plot with a row for each node

plot(data_var_ls[["163152"]]$bin_no_adj, is.na(data_var_ls[["163152"]]$bad_d))





# TODO: Compute distance between each observation in time and then plot distribution.
# Consider relevant summary statistics.





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

# par(mfrow = c(4, 2))
# hist(data_var_ls[[high_sig_edges_ex_id]]$bad)
# hist(data_var_ls[[high_sig_edges_ex_id]]$control)
# hist(data_var_ls[[high_sig_edges_ex_id]]$energy)
# hist(data_var_ls[[high_sig_edges_ex_id]]$focus)
# hist(data_var_ls[[high_sig_edges_ex_id]]$fun)
# hist(data_var_ls[[high_sig_edges_ex_id]]$interest)
# hist(data_var_ls[[high_sig_edges_ex_id]]$movement)
# hist(data_var_ls[[high_sig_edges_ex_id]]$sad)
# par(mfrow = c(1, 1))

# After detrending

# par(mfrow = c(4, 2))
# hist(data_var_ls[[high_sig_edges_ex_id]]$bad_d)
# hist(data_var_ls[[high_sig_edges_ex_id]]$control_d)
# hist(data_var_ls[[high_sig_edges_ex_id]]$energy_d)
# hist(data_var_ls[[high_sig_edges_ex_id]]$focus_d)
# hist(data_var_ls[[high_sig_edges_ex_id]]$fun_d)
# hist(data_var_ls[[high_sig_edges_ex_id]]$interest_d)
# hist(data_var_ls[[high_sig_edges_ex_id]]$movement_d)
# hist(data_var_ls[[high_sig_edges_ex_id]]$sad_d)
# par(mfrow = c(1, 1))

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
# Define function for defining starting values from detrended values in data ----
# ---------------------------------------------------------------------------- #

# Define function for defining starting values from participant's detrended values 
# in data at a given time point "j"

define_start <- function(part_data, j) {
  start_list <- list(bad      = part_data[j, "bad_d"],
                     control  = part_data[j, "control_d"],
                     energy   = part_data[j, "energy_d"],
                     focus    = part_data[j, "focus_d"],
                     fun      = part_data[j, "fun_d"],
                     interest = part_data[j, "interest_d"],
                     movement = part_data[j, "movement_d"],
                     sad      = part_data[j, "sad_d"])
}

# ---------------------------------------------------------------------------- #
# Compute all predicted values starting from participant's detrended values at baseline ----
# ---------------------------------------------------------------------------- #

# Compute number of study time points for each participant

n_study_timepoints <- lapply(data_var_ls, nrow)

# Define starting values for each participant from detrended values at baseline

start_list_bl <- lapply(data_var_ls, define_start, 1)

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
# Define function to compute "k" predicted values starting from each time point ----
# ---------------------------------------------------------------------------- #

# Define function to compute "k" predicted values over desired time points ("iterations") from
# adjacency matrix, starting each iteration from observed value at that iteration's time point

compute_k_pred <- function(part_data, adj_mat, k, iterations) {
  pred <- data.frame()
  
  for (iter in 1:iterations) {
    start_list <- define_start(part_data, iter)
    
    iter_pred <- compute_pred(adj_mat, k, start_list)
    
    names(iter_pred)[names(iter_pred) == "t"] <- "iter_t"
    
    iter_pred$iter <- iter
    iter_pred <- iter_pred[, c("iter", names(iter_pred)[names(iter_pred) != "iter"])]
    
    pred <- rbind(pred, iter_pred)
  }
  
  pred$t <- pred$iter + pred$iter_t - 1
  pred <- pred[, c("t", names(pred)[names(pred) != "t"])]
  
  return(pred)
}

# ---------------------------------------------------------------------------- #
# Define function to compute mean of "k" predicted values starting from each time point  ----
# ---------------------------------------------------------------------------- #

compute_k_pred_m <- function(pred) {
  # Exclude observed values, which were used as starting values for each iteration
  
  target_cols <- paste0(c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad"), "_pred")
  
  pred[pred$iter_t == 1, target_cols] <- NA
  
  # Compute mean predicted values at each time point across iterations
  
  pred_m <- aggregate(. ~ t, pred[, c("t", target_cols)], FUN = function(x) mean(x, na.rm = TRUE))
  
  all_t <- data.frame(t = unique(pred$t))
  pred_m <- merge(all_t, pred_m, by = "t", all.x = TRUE)
  
  return(pred_m)
}

# ---------------------------------------------------------------------------- #
# Compute 4 predicted values starting from participant's detrended values at each time point ----
# ---------------------------------------------------------------------------- #

# Compute predicted values for saturated and thresholded networks over study period

pred_study_4_satur     <- lapply(names(satur_adj_mats_var), function(lifepak_id) {
  compute_k_pred(data_var_ls[[lifepak_id]], satur_adj_mats_var[[lifepak_id]], 4, n_study_timepoints[[lifepak_id]])
})

pred_study_4_thres_a05 <- lapply(names(thres_adj_mats_var), function(lifepak_id) {
  compute_k_pred(data_var_ls[[lifepak_id]], thres_adj_mats_var[[lifepak_id]], 4, n_study_timepoints[[lifepak_id]])
})

names(pred_study_4_satur)     <- names(satur_adj_mats_var)
names(pred_study_4_thres_a05) <- names(thres_adj_mats_var)

# Compute mean of predicted values at each time point (excluding observed values)

pred_m_study_4_satur     <- lapply(pred_study_4_satur,     compute_k_pred_m)
pred_m_study_4_thres_a05 <- lapply(pred_study_4_thres_a05, compute_k_pred_m)

# ---------------------------------------------------------------------------- #
# Define function to compute prediction error  ----
# ---------------------------------------------------------------------------- #

compute_pred_error <- function(part_data, pred) {
  names(part_data)[names(part_data) == "bin_no_adj"] <- "t"
  
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  target_cols <- paste0(vars, "_d")
  pred <- merge(pred, part_data[, c("t", target_cols)], "t", all.x = TRUE)
  
  # Compute signed prediction error
  
  for (var in vars) {
    pred[, paste0(var, "_error")] <- pred[, paste0(var, "_d")] - pred[, paste0(var, "_pred")]
  }
  
  # Compute cumulative absolute prediction error (cumulative sum of absolute value of prediction error)
  
  for (var in vars) {
    pred[, paste0(var, "_cum_err")] <- cumsum(ifelse(is.na(pred[, paste0(var, "_error")]), 0, 
                                                     abs(pred[, paste0(var, "_error")])))
    
    pred[is.na(pred[, paste0(var, "_error")]), paste0(var, "_cum_err")] <- NA
  }
  
  return(pred)
}

# ---------------------------------------------------------------------------- #
# Compute signed prediction error and cumulative absolute prediction error ----
# ---------------------------------------------------------------------------- #

# TODO (Consider whether not to compute prediction error at baseline in this case): For all predicted values starting from baseline

pred_error_study_bl_satur <- lapply(names(data_var_ls), function(lifepak_id) {
  compute_pred_error(data_var_ls[[lifepak_id]], pred_study_bl_satur[[lifepak_id]])
})

pred_error_study_bl_thres_a05 <- lapply(names(data_var_ls), function(lifepak_id) {
  compute_pred_error(data_var_ls[[lifepak_id]], pred_study_bl_thres_a05[[lifepak_id]])
})

names(pred_error_study_bl_satur)     <- names(data_var_ls)
names(pred_error_study_bl_thres_a05) <- names(data_var_ls)





# For mean of 4 predicted values at each time point

pred_m_error_study_4_satur     <- lapply(names(data_var_ls), function(lifepak_id) {
  compute_pred_error(data_var_ls[[lifepak_id]], pred_m_study_4_satur[[lifepak_id]])
})

pred_m_error_study_4_thres_a05 <- lapply(names(data_var_ls), function(lifepak_id) {
  compute_pred_error(data_var_ls[[lifepak_id]], pred_m_study_4_thres_a05[[lifepak_id]])
})

names(pred_m_error_study_4_satur)     <- names(data_var_ls)
names(pred_m_error_study_4_thres_a05) <- names(data_var_ls)

# ---------------------------------------------------------------------------- #
# Compute all predicted values starting from participant's max for one node and 0 for others ----
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

# Define function to plot predicted values starting from one time point or "k" predicted
# values starting many time points (pred_start_t_points = "one" or "many"). Predicted values 
# are plotted as (green) lines, points, or both (pred_plot_type = "l", "p", or "b"); observed
# values are plotted as (black) points. Options include (a) restricting displayed range of 
# time points (view_t_min and view_t_max) and, for "k" predicted values starting from
# many time points, (b) using a different color for each iteration (iter_colors = TRUE)

plot_pred_obs <- function(pred_df, obs_df, pred_start_t_points, pred_plot_type, plot_name, plot_title,
                          view_t_min = NULL, view_t_max = NULL, iter_colors = NULL) {
  obs_df$t <- 1:nrow(obs_df)
  
  # Optionally restrict displayed range of time points
  
  if (!is.null(view_t_min) & !is.null(view_t_max)) {
    obs_df  <- obs_df[obs_df$t >= view_t_min & obs_df$t <= view_t_max, ]
    pred_df <- pred_df[pred_df$t >= view_t_min & pred_df$t <= view_t_max, ]
  }
  
  # Create plots
  
  xlab <- "Time"
  ylab <- "Detrended Value"
  ylim <- c(-100, 100)
  lwd <- 1.5
  pch <- 16
  
  if (pred_start_t_points == "one") {
    color_pred <- "green"
  } else if (pred_start_t_points == "many") {
    n_iterations <- max(pred_df$iter)
    
    if (is.null(iter_colors)) {
      color_pred <- rep("green", n_iterations)
    } else if (iter_colors == TRUE) {
      color_pred <- distinctColorPalette(n_iterations)
    }
  }
  
  vars       <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  var_labels <- vars
  
  var_labels[var_labels == "bad"]      <- "Bad Self"
  var_labels[var_labels == "control"]  <- "Lack Control"
  var_labels[var_labels == "energy"]   <- "Fatigue"
  var_labels[var_labels == "focus"]    <- "Lack Focus"
  var_labels[var_labels == "fun"]      <- "Inaction"
  var_labels[var_labels == "interest"] <- "Lack Interest"
  var_labels[var_labels == "movement"] <- "Slower or Fidgety"
  var_labels[var_labels == "sad"]      <- "Sad"
  
  pred_cols <- paste0(vars, "_pred")
  obs_cols  <- paste0(vars, "_d")
  
  pdf(paste0("./results/pred_values/", plot_name, ".pdf"))
  
  par(mfrow = c(2, 2))
  
  for (i in 1:length(vars)) {
    pred_col  <- pred_cols[i]
    obs_col   <- obs_cols[i]
    var_label <- var_labels[i]
    
    # Start with empty plot
    
    plot(pred_df$t, pred_df[, pred_col], main = var_label, 
         type = "n", xlab = xlab, ylab = ylab, ylim = ylim)
    
    mtext(plot_title, side = 3, line = -1, outer = TRUE)
    
    # Plot predicted values
    
    if (pred_start_t_points == "one") {
      if (pred_plot_type        == "l") {
        lines(pred_df$t,  pred_df[, pred_col], lwd = lwd, col = color_pred)
      } else if (pred_plot_type == "p") {
        points(pred_df$t, pred_df[, pred_col], pch = pch, col = color_pred)
      } else if (pred_plot_type == "b") {
        lines(pred_df$t,  pred_df[, pred_col], lwd = lwd, col = color_pred)
        points(pred_df$t, pred_df[, pred_col], pch = pch, col = color_pred)
      }
    } else if (pred_start_t_points == "many") {
      for (j in 1:n_iterations) {
        iter_pred  <- pred_df[pred_df$iter == j, ]
        iter_color <- color_pred[j]
        
        if (pred_plot_type        == "l") {
          lines(iter_pred$t,  iter_pred[, pred_col], lwd = lwd, col = iter_color)
        } else if (pred_plot_type == "p") {
          points(iter_pred$t, iter_pred[, pred_col], pch = pch, col = iter_color)
        } else if (pred_plot_type == "b") {
          lines(iter_pred$t,  iter_pred[, pred_col], lwd = lwd, col = iter_color)
          points(iter_pred$t, iter_pred[, pred_col], pch = pch, col = iter_color)
        }
      }
    }
    
    # Plot observed values as points
    
    points(obs_df$t, obs_df[, obs_col])
  }
  
  par(mfrow = c(1, 1))
  
  dev.off()
}

dir.create("./results/pred_values/")

  # For all predicted values starting from observed baseline values

lapply(names(pred_study_bl_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_bl_thres_a05[[lifepak_id]], data_var_ls[[lifepak_id]], "one", "l",
                paste0("pred_study_bl_thres_a05_", lifepak_id),
                paste0("Through Study for Thresholded Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})
lapply(names(pred_400_bl_thres_a05),   function(lifepak_id) {
  plot_pred_obs(pred_400_bl_thres_a05[[lifepak_id]],   data_var_ls[[lifepak_id]], "one", "l",
                paste0("pred_400_bl_thres_a05_",   lifepak_id),
                paste0("Through 400 for Thresholded Starting From Obs. Baseline Values (ID ",   lifepak_id, ")"))
})

lapply(names(pred_study_bl_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_bl_satur[[lifepak_id]],     data_var_ls[[lifepak_id]], "one", "l",
                paste0("pred_study_bl_satur_",     lifepak_id),
                paste0("Through Study for Saturated Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})
lapply(names(pred_400_bl_satur),       function(lifepak_id) {
  plot_pred_obs(pred_400_bl_satur[[lifepak_id]],       data_var_ls[[lifepak_id]], "one", "l",
                paste0("pred_400_bl_satur_",       lifepak_id),
                paste0("Through 400 for Saturated Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})

  # For 4 predicted values starting from each time point

lapply(names(pred_study_4_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_4_satur[[lifepak_id]],     data_var_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_satur_",                      lifepak_id),
                paste0("Next 3 Through Study for Saturated Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})
lapply(names(pred_study_4_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_4_satur[[lifepak_id]],     data_var_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_satur_iter_colors_",          lifepak_id),
                paste0("Next 3 Through Study for Saturated Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                iter_colors = TRUE)
})
lapply(names(pred_study_4_satur),     function(lifepak_id) {
  plot_pred_obs(pred_study_4_satur[[lifepak_id]],     data_var_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_satur_1-50_iter_colors_",     lifepak_id),
                paste0("Next 3 Through Study for Saturated Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                view_t_min = 1, view_t_max = 50, iter_colors = TRUE)
})

lapply(names(pred_study_4_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_4_thres_a05[[lifepak_id]], data_var_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_thres_a05_",                  lifepak_id),
                paste0("Next 3 Through Study for Thresholded Starting From Each Obs. Value (ID ", lifepak_id, ")"))
})
lapply(names(pred_study_4_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_4_thres_a05[[lifepak_id]], data_var_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_thres_a05_iter_colors_",      lifepak_id),
                paste0("Next 3 Through Study for Thresholded Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                iter_colors = TRUE)
})
lapply(names(pred_study_4_thres_a05), function(lifepak_id) {
  plot_pred_obs(pred_study_4_thres_a05[[lifepak_id]], data_var_ls[[lifepak_id]], "many", "l",
                paste0("pred_study_4_thres_a05_1-50_iter_colors_", lifepak_id),
                paste0("Next 3 Through Study for Thresholded Starting From Each Obs. Value (ID ", lifepak_id, ")"),
                view_t_min = 1, view_t_max = 50, iter_colors = TRUE)
})

# Define function to plot predicted values from various baseline starting points

plot_pred_obs_various_bl_start <- function(various_pred_lists, various_pred_lists_focal_var_labels, data_var_ls, 
                                           plot_name_stem, thres, plot_title_stem) {
  for (i in 1:length(various_pred_lists)) {
    pred_list <- various_pred_lists[[i]]
    pred_list_name <- names(various_pred_lists)[i]
    pred_list_focal_var_label <- various_pred_lists_focal_var_labels[i]
    
    pred_list_plot_title_stem <- sub("pred_list_focal_var_label", pred_list_focal_var_label, plot_title_stem)
    
    lapply(names(pred_list), function(lifepak_id) {
      plot_pred_obs(pred_list[[lifepak_id]], data_var_ls[[lifepak_id]], "one", "l",
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

plot_pred_obs_various_bl_start(pred_study_max_one_0_others_thres_a05, pred_max_one_0_others_focal_var_labels, data_var_ls,
                               "pred_study_max_one_0_others", "thres_a05",
                               'Through Study for Thres. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')
plot_pred_obs_various_bl_start(pred_400_max_one_0_others_thres_a05,   pred_max_one_0_others_focal_var_labels, data_var_ls,
                               "pred_400_max_one_0_others",   "thres_a05",
                               'Through 400 for Thres. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')

plot_pred_obs_various_bl_start(pred_study_max_one_0_others_satur,     pred_max_one_0_others_focal_var_labels, data_var_ls,
                               "pred_study_max_one_0_others", "satur",
                               'Through Study for Satur. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')
plot_pred_obs_various_bl_start(pred_400_max_one_0_others_satur,       pred_max_one_0_others_focal_var_labels, data_var_ls,
                               "pred_400_max_one_0_others",   "satur",
                               'Through 400 for Satur. Starting From Max "pred_list_focal_var_label" and 0 Otherwise (ID ')

# ---------------------------------------------------------------------------- #
# Plot prediction errors  ----
# ---------------------------------------------------------------------------- #

# TODO (streamline logic of if/else statements): Define function to plot signed 
# prediction error over time, optionally plotting (a) absolute value of the 
# prediction error or (b) cumulative absolute prediction error





plot_pred_error <- function(pred_error, plot_name, plot_title, abs = NULL, cum_abs = NULL) {
  pdf(paste0("./results/pred_error/", plot_name, ".pdf"))
  
  par(mfrow = c(2, 2))
  
  xlab <- "Time"
  col  <- "red"
  pch  <- 16
  
  if (is.null(abs) & is.null(cum_abs)) {
    ylab <- "Prediction Error"
    ylim <- c(-100, 100)
    
    plot(pred_error$t, pred_error$bad_error,      main = "Bad Self",
         xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
    plot(pred_error$t, pred_error$control_error,  main = "Lack Control",
         xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
    plot(pred_error$t, pred_error$energy_error,   main = "Fatigue",
         xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
    plot(pred_error$t, pred_error$focus_error,    main = "Lack Focus",
         xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
    
    mtext(plot_title, side = 3, line = -1, outer = TRUE)
    
    plot(pred_error$t, pred_error$fun_error,      main = "Inaction",  
         xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
    plot(pred_error$t, pred_error$interest_error, main = "Lack Interest",
         xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
    plot(pred_error$t, pred_error$movement_error, main = "Slower or Fidgety",
         xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
    plot(pred_error$t, pred_error$sad_error,      main = "Sad", 
         xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
    
    mtext(plot_title, side = 3, line = -1, outer = TRUE)
  } else if (!is.null(abs)) {
    if (abs == TRUE) {
      ylab <- "|Prediction Error|"
      ylim <- c(0, 100)
      
      plot(pred_error$t, abs(pred_error$bad_error),      main = "Bad Self",
           xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
      plot(pred_error$t, abs(pred_error$control_error),  main = "Lack Control",
           xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
      plot(pred_error$t, abs(pred_error$energy_error),   main = "Fatigue",
           xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
      plot(pred_error$t, abs(pred_error$focus_error),    main = "Lack Focus",
           xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
      
      mtext(plot_title, side = 3, line = -1, outer = TRUE)
      
      plot(pred_error$t, abs(pred_error$fun_error),      main = "Inaction",  
           xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
      plot(pred_error$t, abs(pred_error$interest_error), main = "Lack Interest",
           xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
      plot(pred_error$t, abs(pred_error$movement_error), main = "Slower or Fidgety",
           xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
      plot(pred_error$t, abs(pred_error$sad_error),      main = "Sad", 
           xlab = xlab, ylab = ylab, col = col, pch = pch, ylim = ylim)
      
      mtext(plot_title, side = 3, line = -1, outer = TRUE)
    }
  } else if (cum_abs == TRUE) {
    ylab <- "Cumulative |Prediction Error|"

    plot(pred_error$t, pred_error$bad_cum_err,      main = "Bad Self",
         xlab = xlab, ylab = ylab, col = col, pch = pch)
    plot(pred_error$t, pred_error$control_cum_err,  main = "Lack Control",
         xlab = xlab, ylab = ylab, col = col, pch = pch)
    plot(pred_error$t, pred_error$energy_cum_err,   main = "Fatigue",
         xlab = xlab, ylab = ylab, col = col, pch = pch)
    plot(pred_error$t, pred_error$focus_cum_err,    main = "Lack Focus",
         xlab = xlab, ylab = ylab, col = col, pch = pch)
    
    mtext(plot_title, side = 3, line = -1, outer = TRUE)
    
    plot(pred_error$t, pred_error$fun_cum_err,      main = "Inaction",  
         xlab = xlab, ylab = ylab, col = col, pch = pch)
    plot(pred_error$t, pred_error$interest_cum_err, main = "Lack Interest",
         xlab = xlab, ylab = ylab, col = col, pch = pch)
    plot(pred_error$t, pred_error$movement_cum_err, main = "Slower or Fidgety",
         xlab = xlab, ylab = ylab, col = col, pch = pch)
    plot(pred_error$t, pred_error$sad_cum_err,      main = "Sad", 
         xlab = xlab, ylab = ylab, col = col, pch = pch)
    
    mtext(plot_title, side = 3, line = -1, outer = TRUE)
  }
  
  par(mfrow = c(1, 1))
  
  dev.off()
}

# Run function

dir.create("./results/pred_error/")

  # For all predicted values starting from baseline

lapply(names(pred_error_study_bl_satur),     function(lifepak_id) {
  plot_pred_error(pred_error_study_bl_satur[[lifepak_id]],
                  paste0("pred_study_bl_satur_error_", lifepak_id),
                  paste0("Error for Through Study for Saturated Starting From Obs. Baseline Values (ID ", lifepak_id, ")"))
})
lapply(names(pred_error_study_bl_satur),     function(lifepak_id) {
  plot_pred_error(pred_error_study_bl_satur[[lifepak_id]],
                  paste0("pred_study_bl_satur_error_abs_", lifepak_id),
                  paste0("Error for Through Study for Saturated Starting From Obs. Baseline Values (ID ", lifepak_id, ")"),
                  abs = TRUE)
})
lapply(names(pred_error_study_bl_satur),     function(lifepak_id) {
  plot_pred_error(pred_error_study_bl_satur[[lifepak_id]],
                  paste0("pred_study_bl_satur_error_cum_abs_", lifepak_id),
                  paste0("Error for Through Study for Saturated Starting From Obs. Baseline Values (ID ", lifepak_id, ")"),
                  cum_abs = TRUE)
})

    # TODO: Run for thresholded





  # For mean of 4 predicted values starting from each time point (excluding observed values)

lapply(names(pred_m_error_study_4_satur),     function(lifepak_id) {
  plot_pred_error(pred_m_error_study_4_satur[[lifepak_id]],
                  paste0("pred_study_4_satur_m_error_", lifepak_id),
                  paste0("Error for Next 3 Through Study for Satur. Starting From Each Obs. Value, Avg'd (ID ", lifepak_id, ")"))
})
lapply(names(pred_m_error_study_4_satur),     function(lifepak_id) {
  plot_pred_error(pred_m_error_study_4_satur[[lifepak_id]],
                  paste0("pred_study_4_satur_m_error_abs_", lifepak_id),
                  paste0("Error for Next 3 Through Study for Satur. Starting From Each Obs. Value, Avg'd (ID ", lifepak_id, ")"),
                  abs = TRUE)
})
lapply(names(pred_m_error_study_4_satur),     function(lifepak_id) {
  plot_pred_error(pred_m_error_study_4_satur[[lifepak_id]],
                  paste0("pred_study_4_satur_m_error_cum_abs_", lifepak_id),
                  paste0("Error for Next 3 Through Study for Satur. Starting From Each Obs. Value, Avg'd (ID ", lifepak_id, ")"),
                  cum_abs = TRUE)
})

lapply(names(pred_m_error_study_4_thres_a05), function(lifepak_id) {
  plot_pred_error(pred_m_error_study_4_thres_a05[[lifepak_id]],
                  paste0("pred_study_4_thres_a05_m_error_", lifepak_id),
                  paste0("Error for Next 3 Through Study for Thres. Starting From Each Obs. Value, Avg'd (ID ", lifepak_id, ")"))
})
lapply(names(pred_m_error_study_4_thres_a05), function(lifepak_id) {
  plot_pred_error(pred_m_error_study_4_thres_a05[[lifepak_id]],
                  paste0("pred_study_4_thres_a05_m_error_abs_", lifepak_id),
                  paste0("Error for Next 3 Through Study for Thres. Starting From Each Obs. Value, Avg'd (ID ", lifepak_id, ")"),
                  abs = TRUE)
})
lapply(names(pred_m_error_study_4_thres_a05), function(lifepak_id) {
  plot_pred_error(pred_m_error_study_4_thres_a05[[lifepak_id]],
                  paste0("pred_study_4_thres_a05_m_error_cum_abs_", lifepak_id),
                  paste0("Error for Next 3 Through Study for Thres. Starting From Each Obs. Value, Avg'd (ID ", lifepak_id, ")"),
                  cum_abs = TRUE)
})

# ---------------------------------------------------------------------------- #
# TODO: Experiment with GLLA ----
# ---------------------------------------------------------------------------- #

groundhog.library("EGAnet", groundhog_day)

tseries <- 49:56
deriv.tseries <- glla(tseries, n.embed = 4, tau = 1, delta = 1, order = 2)

plot(deriv.tseries[, "Obs"], deriv.tseries[, "DerivOrd1"])





