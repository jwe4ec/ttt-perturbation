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

  # For participant with high number of significant edges (10)

adj_mat_326177 <- thres_adj_mats_var[["326177"]]

plot_network(adj_mat_326177, "thres_a05")

# For example participant with medium number of significant edges (3)

adj_mat_861114 <- thres_adj_mats_var[["861114"]]

plot_network(adj_mat_861114, "thres_a05")

  # For example participant with low number of significant edges (1)

adj_mat_999341 <- thres_adj_mats_var[["999341"]]

plot_network(adj_mat_999341, "thres_a05")

# ---------------------------------------------------------------------------- #
# Explore number of observations ----
# ---------------------------------------------------------------------------- #

# View data for selected participants above

# View(data_var[data_var$lifepak_id == "326177", ])
# View(data_var[data_var$lifepak_id == "861114", ])
# View(data_var[data_var$lifepak_id == "999341", ])

# Identify approximate number of observations

sum(!is.na(data_var[data_var$lifepak_id == "326177", "bad"])) # ~98 observations
sum(!is.na(data_var[data_var$lifepak_id == "861114", "bad"])) # ~93 observations
sum(!is.na(data_var[data_var$lifepak_id == "999341", "bad"])) # ~82 observations

# Identify distribution of approximate number of observations (range is 62-102)

data_var_ls <- split(data_var, data_var$lifepak_id)

hist(unlist(lapply(data_var_ls, function(x) sum(!is.na(x$bad_d)))),
     main = "Distribution of Number of Observations",
     xlab = "Number of Observations",
     ylab = "Number of Participants",
     xlim = c(60, 105))

all(range(unlist(lapply(data_var_ls, function(x) sum(!is.na(x$bad_d))))) == c(62, 102))

# ---------------------------------------------------------------------------- #
# Extract example participants' data ----
# ---------------------------------------------------------------------------- #

data_326177 <- as.data.frame(data_var_ls[[high_sig_edges_ex_id]])
data_861114 <- as.data.frame(data_var_ls[[med_sig_edges_ex_id]])
data_999341 <- as.data.frame(data_var_ls[[low_sig_edges_ex_id]])

# ---------------------------------------------------------------------------- #
# Explore item distributions ----
# ---------------------------------------------------------------------------- #

# For example participant with high number of significant edges

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

# Not done for example participants with medium or low numbers of significant edges

# ---------------------------------------------------------------------------- #
# Plot node values over time ----
# ---------------------------------------------------------------------------- #

# For example participant with high number of significant edges

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

# Not done for example participants with medium or low numbers of significant edges

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

n_study_timepoints_326177 <- nrow(data_326177)
n_study_timepoints_861114 <- nrow(data_861114)
n_study_timepoints_999341 <- nrow(data_999341)

# Define starting values for each participant

define_start_bl <- function(subject_data) {
  start_list_bl <- list(bad      = subject_data[1, "bad_d"],
                        control  = subject_data[1, "control_d"],
                        energy   = subject_data[1, "energy_d"],
                        focus    = subject_data[1, "focus_d"],
                        fun      = subject_data[1, "fun_d"],
                        interest = subject_data[1, "interest_d"],
                        movement = subject_data[1, "movement_d"],
                        sad      = subject_data[1, "sad_d"])
}

start_list_bl_326177 <- define_start_bl(data_326177)
start_list_bl_861114 <- define_start_bl(data_861114)
start_list_bl_999341 <- define_start_bl(data_999341)

# TODO (use function to condense code): Compute predicted values (a) over study period and (b) into future





  # For example participant with high number of significant edges

pred_326177_study_bl <- compute_pred(adj_mat_326177, n_study_timepoints_326177, start_list_bl_326177)

pred_326177_400_bl   <- compute_pred(adj_mat_326177, 400,                       start_list_bl_326177)
pred_326177_4000_bl  <- compute_pred(adj_mat_326177, 4000,                      start_list_bl_326177)
pred_326177_40000_bl <- compute_pred(adj_mat_326177, 40000,                     start_list_bl_326177)

  # For example participant with medium number of significant edges

pred_861114_study_bl <- compute_pred(adj_mat_861114, n_study_timepoints_861114, start_list_bl_861114)

pred_861114_400_bl   <- compute_pred(adj_mat_861114, 400,                       start_list_bl_861114)
pred_861114_4000_bl  <- compute_pred(adj_mat_861114, 4000,                      start_list_bl_861114)
pred_861114_40000_bl <- compute_pred(adj_mat_861114, 40000,                     start_list_bl_861114)

  # For example participant with low number of significant edges

pred_999341_study_bl <- compute_pred(adj_mat_999341, n_study_timepoints_999341, start_list_bl_999341)

pred_999341_400_bl   <- compute_pred(adj_mat_999341, 400,                       start_list_bl_999341)
pred_999341_4000_bl  <- compute_pred(adj_mat_999341, 4000,                      start_list_bl_999341)
pred_999341_40000_bl <- compute_pred(adj_mat_999341, 40000,                     start_list_bl_999341)

# ---------------------------------------------------------------------------- #
# Compute predicted values starting from participant's max for one node and 0 for others ----
# ---------------------------------------------------------------------------- #

# Define starting values for each participant

define_start_max_one_0_others <- function(subject_data) {
  vars <- c("bad", "control", "energy", "focus", "fun", "interest", "movement", "sad")
  
  start_list_max_one_0_others_element <- vector("list", length = length(vars))
  names(start_list_max_one_0_others_element) <- vars
  start_list_max_one_0_others_element[ ] <- 0
  
  start_list_max_one_0_others <- vector("list", length = length(vars))
  names(start_list_max_one_0_others) <- paste0("max_", vars, "_d")
  start_list_max_one_0_others[ ] <- list(start_list_max_one_0_others_element)
  
  start_list_max_one_0_others$max_bad_d$bad           <- max(subject_data["bad_d"],      na.rm = TRUE)
  start_list_max_one_0_others$max_control_d$control   <- max(subject_data["control_d"],  na.rm = TRUE)
  start_list_max_one_0_others$max_energy_d$energy     <- max(subject_data["energy_d"],   na.rm = TRUE)
  start_list_max_one_0_others$max_focus_d$focus       <- max(subject_data["focus_d"],    na.rm = TRUE)
  start_list_max_one_0_others$max_fun_d$fun           <- max(subject_data["fun_d"],      na.rm = TRUE)
  start_list_max_one_0_others$max_interest_d$interest <- max(subject_data["interest_d"], na.rm = TRUE)
  start_list_max_one_0_others$max_movement_d$movement <- max(subject_data["movement_d"], na.rm = TRUE)
  start_list_max_one_0_others$max_sad_d$sad           <- max(subject_data["sad_d"],      na.rm = TRUE)
  
  return(start_list_max_one_0_others)
}

start_list_max_one_0_others_326177 <- define_start_max_one_0_others(data_326177)
start_list_max_one_0_others_861114 <- define_start_max_one_0_others(data_861114)
start_list_max_one_0_others_999341 <- define_start_max_one_0_others(data_999341)

# TODO (use function to condense code): Compute predicted values (a) over study period and (b) into future





  # For example participant with high number of significant edges

pred_326177_study_max_bad_d      <- compute_pred(adj_mat_326177, n_study_timepoints_326177, start_list_max_one_0_others_326177$max_bad_d)
pred_326177_study_max_control_d  <- compute_pred(adj_mat_326177, n_study_timepoints_326177, start_list_max_one_0_others_326177$max_control_d)
pred_326177_study_max_energy_d   <- compute_pred(adj_mat_326177, n_study_timepoints_326177, start_list_max_one_0_others_326177$max_energy_d)
pred_326177_study_max_focus_d    <- compute_pred(adj_mat_326177, n_study_timepoints_326177, start_list_max_one_0_others_326177$max_focus_d)
pred_326177_study_max_fun_d      <- compute_pred(adj_mat_326177, n_study_timepoints_326177, start_list_max_one_0_others_326177$max_fun_d)
pred_326177_study_max_interest_d <- compute_pred(adj_mat_326177, n_study_timepoints_326177, start_list_max_one_0_others_326177$max_interest_d)
pred_326177_study_max_movement_d <- compute_pred(adj_mat_326177, n_study_timepoints_326177, start_list_max_one_0_others_326177$max_movement_d)
pred_326177_study_max_sad_d      <- compute_pred(adj_mat_326177, n_study_timepoints_326177, start_list_max_one_0_others_326177$max_sad_d)

pred_326177_400_max_bad_d        <- compute_pred(adj_mat_326177, 400,                       start_list_max_one_0_others_326177$max_bad_d)
pred_326177_400_max_control_d    <- compute_pred(adj_mat_326177, 400,                       start_list_max_one_0_others_326177$max_control_d)
pred_326177_400_max_energy_d     <- compute_pred(adj_mat_326177, 400,                       start_list_max_one_0_others_326177$max_energy_d)
pred_326177_400_max_focus_d      <- compute_pred(adj_mat_326177, 400,                       start_list_max_one_0_others_326177$max_focus_d)
pred_326177_400_max_fun_d        <- compute_pred(adj_mat_326177, 400,                       start_list_max_one_0_others_326177$max_fun_d)
pred_326177_400_max_interest_d   <- compute_pred(adj_mat_326177, 400,                       start_list_max_one_0_others_326177$max_interest_d)
pred_326177_400_max_movement_d   <- compute_pred(adj_mat_326177, 400,                       start_list_max_one_0_others_326177$max_movement_d)
pred_326177_400_max_sad_d        <- compute_pred(adj_mat_326177, 400,                       start_list_max_one_0_others_326177$max_sad_d)

  # For example participant with medium number of significant edges

pred_861114_study_max_bad_d      <- compute_pred(adj_mat_861114, n_study_timepoints_861114, start_list_max_one_0_others_861114$max_bad_d)
pred_861114_study_max_control_d  <- compute_pred(adj_mat_861114, n_study_timepoints_861114, start_list_max_one_0_others_861114$max_control_d)
pred_861114_study_max_energy_d   <- compute_pred(adj_mat_861114, n_study_timepoints_861114, start_list_max_one_0_others_861114$max_energy_d)
pred_861114_study_max_focus_d    <- compute_pred(adj_mat_861114, n_study_timepoints_861114, start_list_max_one_0_others_861114$max_focus_d)
pred_861114_study_max_fun_d      <- compute_pred(adj_mat_861114, n_study_timepoints_861114, start_list_max_one_0_others_861114$max_fun_d)
pred_861114_study_max_interest_d <- compute_pred(adj_mat_861114, n_study_timepoints_861114, start_list_max_one_0_others_861114$max_interest_d)
pred_861114_study_max_movement_d <- compute_pred(adj_mat_861114, n_study_timepoints_861114, start_list_max_one_0_others_861114$max_movement_d)
pred_861114_study_max_sad_d      <- compute_pred(adj_mat_861114, n_study_timepoints_861114, start_list_max_one_0_others_861114$max_sad_d)

pred_861114_400_max_bad_d        <- compute_pred(adj_mat_861114, 400,                       start_list_max_one_0_others_861114$max_bad_d)
pred_861114_400_max_control_d    <- compute_pred(adj_mat_861114, 400,                       start_list_max_one_0_others_861114$max_control_d)
pred_861114_400_max_energy_d     <- compute_pred(adj_mat_861114, 400,                       start_list_max_one_0_others_861114$max_energy_d)
pred_861114_400_max_focus_d      <- compute_pred(adj_mat_861114, 400,                       start_list_max_one_0_others_861114$max_focus_d)
pred_861114_400_max_fun_d        <- compute_pred(adj_mat_861114, 400,                       start_list_max_one_0_others_861114$max_fun_d)
pred_861114_400_max_interest_d   <- compute_pred(adj_mat_861114, 400,                       start_list_max_one_0_others_861114$max_interest_d)
pred_861114_400_max_movement_d   <- compute_pred(adj_mat_861114, 400,                       start_list_max_one_0_others_861114$max_movement_d)
pred_861114_400_max_sad_d        <- compute_pred(adj_mat_861114, 400,                       start_list_max_one_0_others_861114$max_sad_d)

  # For example participant with low number of significant edges

pred_999341_study_max_bad_d      <- compute_pred(adj_mat_999341, n_study_timepoints_999341, start_list_max_one_0_others_999341$max_bad_d)
pred_999341_study_max_control_d  <- compute_pred(adj_mat_999341, n_study_timepoints_999341, start_list_max_one_0_others_999341$max_control_d)
pred_999341_study_max_energy_d   <- compute_pred(adj_mat_999341, n_study_timepoints_999341, start_list_max_one_0_others_999341$max_energy_d)
pred_999341_study_max_focus_d    <- compute_pred(adj_mat_999341, n_study_timepoints_999341, start_list_max_one_0_others_999341$max_focus_d)
pred_999341_study_max_fun_d      <- compute_pred(adj_mat_999341, n_study_timepoints_999341, start_list_max_one_0_others_999341$max_fun_d)
pred_999341_study_max_interest_d <- compute_pred(adj_mat_999341, n_study_timepoints_999341, start_list_max_one_0_others_999341$max_interest_d)
pred_999341_study_max_movement_d <- compute_pred(adj_mat_999341, n_study_timepoints_999341, start_list_max_one_0_others_999341$max_movement_d)
pred_999341_study_max_sad_d      <- compute_pred(adj_mat_999341, n_study_timepoints_999341, start_list_max_one_0_others_999341$max_sad_d)

pred_999341_400_max_bad_d        <- compute_pred(adj_mat_999341, 400,                       start_list_max_one_0_others_999341$max_bad_d)
pred_999341_400_max_control_d    <- compute_pred(adj_mat_999341, 400,                       start_list_max_one_0_others_999341$max_control_d)
pred_999341_400_max_energy_d     <- compute_pred(adj_mat_999341, 400,                       start_list_max_one_0_others_999341$max_energy_d)
pred_999341_400_max_focus_d      <- compute_pred(adj_mat_999341, 400,                       start_list_max_one_0_others_999341$max_focus_d)
pred_999341_400_max_fun_d        <- compute_pred(adj_mat_999341, 400,                       start_list_max_one_0_others_999341$max_fun_d)
pred_999341_400_max_interest_d   <- compute_pred(adj_mat_999341, 400,                       start_list_max_one_0_others_999341$max_interest_d)
pred_999341_400_max_movement_d   <- compute_pred(adj_mat_999341, 400,                       start_list_max_one_0_others_999341$max_movement_d)
pred_999341_400_max_sad_d        <- compute_pred(adj_mat_999341, 400,                       start_list_max_one_0_others_999341$max_sad_d)

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

# TODO (use function to condense code): Run function





dir.create("./results/pred_values/")

  # For example participant with high number of significant edges

plot_pred_obs(pred_326177_study_bl,             data_326177, "Through Study Starting From Obs. Baseline Values (ID 326177)")

plot_pred_obs(pred_326177_400_bl,               data_326177, "Through 400 Starting From Obs. Baseline Values (ID 326177)")
plot_pred_obs(pred_326177_4000_bl,              data_326177, "Through 4000 Starting From Obs. Baseline Values (ID 326177)")

plot_pred_obs(pred_326177_study_max_bad_d,      data_326177, 'Through Study Starting From Max "Bad Self" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_study_max_control_d,  data_326177, 'Through Study Starting From Max "Lack Control" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_study_max_energy_d,   data_326177, 'Through Study Starting From Max "Fatigue" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_study_max_focus_d,    data_326177, 'Through Study Starting From Max "Lack Focus" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_study_max_fun_d,      data_326177, 'Through Study Starting From Max "Inaction" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_study_max_interest_d, data_326177, 'Through Study Starting From Max "Lack Interest" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_study_max_movement_d, data_326177, 'Through Study Starting From Max "Slower or Fidgety" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_study_max_sad_d,      data_326177, 'Through Study Starting From Max "Sad" and 0 Otherwise (ID 326177)')

plot_pred_obs(pred_326177_400_max_bad_d,        data_326177, 'Through 400 Starting From Max "Bad Self" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_400_max_control_d,    data_326177, 'Through 400 Starting From Max "Lack Control" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_400_max_energy_d,     data_326177, 'Through 400 Starting From Max "Fatigue" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_400_max_focus_d,      data_326177, 'Through 400 Starting From Max "Lack Focus" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_400_max_fun_d,        data_326177, 'Through 400 Starting From Max "Inaction" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_400_max_interest_d,   data_326177, 'Through 400 Starting From Max "Lack Interest" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_400_max_movement_d,   data_326177, 'Through 400 Starting From Max "Slower or Fidgety" and 0 Otherwise (ID 326177)')
plot_pred_obs(pred_326177_400_max_sad_d,        data_326177, 'Through 400 Starting From Max "Sad" and 0 Otherwise (ID 326177)')

  # For example participant with medium number of significant edges

plot_pred_obs(pred_861114_study_bl,             data_861114, "Through Study Starting From Obs. Baseline Values (ID 861114)")

plot_pred_obs(pred_861114_400_bl,               data_861114, "Through 400 Starting From Obs. Baseline Values (ID 861114)")
plot_pred_obs(pred_861114_4000_bl,              data_861114, "Through 4000 Starting From Obs. Baseline Values (ID 861114)")

plot_pred_obs(pred_861114_study_max_bad_d,      data_861114, 'Through Study Starting From Max "Bad Self" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_study_max_control_d,  data_861114, 'Through Study Starting From Max "Lack Control" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_study_max_energy_d,   data_861114, 'Through Study Starting From Max "Fatigue" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_study_max_focus_d,    data_861114, 'Through Study Starting From Max "Lack Focus" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_study_max_fun_d,      data_861114, 'Through Study Starting From Max "Inaction" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_study_max_interest_d, data_861114, 'Through Study Starting From Max "Lack Interest" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_study_max_movement_d, data_861114, 'Through Study Starting From Max "Slower or Fidgety" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_study_max_sad_d,      data_861114, 'Through Study Starting From Max "Sad" and 0 Otherwise (ID 861114)')

plot_pred_obs(pred_861114_400_max_bad_d,        data_861114, 'Through 400 Starting From Max "Bad Self" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_400_max_control_d,    data_861114, 'Through 400 Starting From Max "Lack Control" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_400_max_energy_d,     data_861114, 'Through 400 Starting From Max "Fatigue" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_400_max_focus_d,      data_861114, 'Through 400 Starting From Max "Lack Focus" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_400_max_fun_d,        data_861114, 'Through 400 Starting From Max "Inaction" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_400_max_interest_d,   data_861114, 'Through 400 Starting From Max "Lack Interest" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_400_max_movement_d,   data_861114, 'Through 400 Starting From Max "Slower or Fidgety" and 0 Otherwise (ID 861114)')
plot_pred_obs(pred_861114_400_max_sad_d,        data_861114, 'Through 400 Starting From Max "Sad" and 0 Otherwise (ID 861114)')

  # For example participant with low number of significant edges

plot_pred_obs(pred_999341_study_bl,             data_999341, "Through Study Starting From Obs. Baseline Values (ID 999341)")

plot_pred_obs(pred_999341_400_bl,               data_999341, "Through 400 Starting From Obs. Baseline Values (ID 999341)")
plot_pred_obs(pred_999341_4000_bl,              data_999341, "Through 4000 Starting From Obs. Baseline Values (ID 999341)")

plot_pred_obs(pred_999341_study_max_bad_d,      data_999341, 'Through Study Starting From Max "Bad Self" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_study_max_control_d,  data_999341, 'Through Study Starting From Max "Lack Control" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_study_max_energy_d,   data_999341, 'Through Study Starting From Max "Fatigue" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_study_max_focus_d,    data_999341, 'Through Study Starting From Max "Lack Focus" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_study_max_fun_d,      data_999341, 'Through Study Starting From Max "Inaction" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_study_max_interest_d, data_999341, 'Through Study Starting From Max "Lack Interest" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_study_max_movement_d, data_999341, 'Through Study Starting From Max "Slower or Fidgety" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_study_max_sad_d,      data_999341, 'Through Study Starting From Max "Sad" and 0 Otherwise (ID 999341)')

plot_pred_obs(pred_999341_400_max_bad_d,        data_999341, 'Through 400 Starting From Max "Bad Self" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_400_max_control_d,    data_999341, 'Through 400 Starting From Max "Lack Control" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_400_max_energy_d,     data_999341, 'Through 400 Starting From Max "Fatigue" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_400_max_focus_d,      data_999341, 'Through 400 Starting From Max "Lack Focus" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_400_max_fun_d,        data_999341, 'Through 400 Starting From Max "Inaction" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_400_max_interest_d,   data_999341, 'Through 400 Starting From Max "Lack Interest" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_400_max_movement_d,   data_999341, 'Through 400 Starting From Max "Slower or Fidgety" and 0 Otherwise (ID 999341)')
plot_pred_obs(pred_999341_400_max_sad_d,        data_999341, 'Through 400 Starting From Max "Sad" and 0 Otherwise (ID 999341)')

# ---------------------------------------------------------------------------- #
# TODO: Experiment with GLLA ----
# ---------------------------------------------------------------------------- #

groundhog.library("EGAnet", groundhog_day)

tseries <- 49:56
deriv.tseries <- glla(tseries, n.embed = 4, tau = 1, delta = 1, order = 2)

plot(deriv.tseries[, "Obs"], deriv.tseries[, "DerivOrd1"])





